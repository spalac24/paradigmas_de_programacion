{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AbstractCurryPrinter (d_C_showProg, d_C_showTypeDecls, d_C_showTypeDecl, d_C_showTypeExpr, d_C_showFuncDecl, nd_C_showFuncDecl, d_C_showExpr, nd_C_showExpr, d_C_showPattern) where

import Basics
import qualified Curry_AbstractCurry
import qualified Curry_Char
import qualified Curry_FiniteMap
import qualified Curry_List
import qualified Curry_Maybe
import qualified Curry_Prelude
import qualified Curry_Sort
import qualified Curry_Read
type C_NameFM = Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit

type C_Options = Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char)

d_C_showProg :: Curry_AbstractCurry.C_CurryProg -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showProg x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CurryProg x2 x3 x4 x5 x6) -> let
          x7 = d_C_showExports x4 x5 x3500
           in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (d_OP__case_181 x7 (Curry_Prelude.d_C_null x7 x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showImports x3 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_showOpDecls x6 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeDecls x4 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_prefixInter (d_C_showFuncDeclOpt (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply (d_C_nameFM x3500) x5 x3500) x2)) x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500) x3500) x3500) x3500) x3500)
     (Curry_AbstractCurry.Choice_C_CurryProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showProg x1002 x3500) (d_C_showProg x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CurryProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showProg z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CurryProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showProg x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CurryProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_defaultOptions :: ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_defaultOptions x3500 = Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.d_C_emptyFM (acceptCs id d_C_lessString) x3500) Curry_Prelude.OP_List

nd_C_defaultOptions :: IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_defaultOptions x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.nd_C_emptyFM (wrapDX (wrapDX id) (acceptCs id d_C_lessString)) x2000 x3500) Curry_Prelude.OP_List))

d_C_showExports :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CFuncDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showExports x1 x2 x3500 = let
     x3 = Curry_Prelude.d_C_filter d_OP_showExports_dot_isPublicType_dot_7 x1 x3500
     x4 = Curry_List.d_C_partition d_OP_showExports_dot_allPublicCons_dot_7 x3 x3500
     x5 = d_OP_showExports_dot___hash_selFP2_hash_withCons x4 x3500
     x6 = d_OP_showExports_dot___hash_selFP3_hash_withoutCons x4 x3500
      in (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))))) d_OP_showExports_dot_getTypeName_dot_7 x3500) x5 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_map d_OP_showExports_dot_getTypeName_dot_7 x6 x3500) (Curry_Prelude.d_C_map d_OP_showExports_dot_getFuncName_dot_7 (Curry_Prelude.d_C_filter d_OP_showExports_dot_isPublicFunc_dot_7 x2 x3500) x3500) x3500) x3500) x3500) x3500)

d_OP_showExports_dot_isPublicType_dot_7 :: Curry_AbstractCurry.C_CTypeDecl -> ConstStore -> Curry_Prelude.C_Bool
d_OP_showExports_dot_isPublicType_dot_7 x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CType x2 x3 x4 x5) -> Curry_Prelude.d_OP_eq_eq x3 Curry_AbstractCurry.C_Public x3500
     (Curry_AbstractCurry.C_CTypeSyn x6 x7 x8 x9) -> Curry_Prelude.d_OP_eq_eq x7 Curry_AbstractCurry.C_Public x3500
     (Curry_AbstractCurry.Choice_C_CTypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot_isPublicType_dot_7 x1002 x3500) (d_OP_showExports_dot_isPublicType_dot_7 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot_isPublicType_dot_7 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot_isPublicType_dot_7 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showExports_dot_isPublicFunc_dot_7 :: Curry_AbstractCurry.C_CFuncDecl -> ConstStore -> Curry_Prelude.C_Bool
d_OP_showExports_dot_isPublicFunc_dot_7 x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CFunc x2 x3 x4 x5 x6) -> Curry_Prelude.d_OP_eq_eq x4 Curry_AbstractCurry.C_Public x3500
     (Curry_AbstractCurry.C_CmtFunc x7 x8 x9 x10 x11 x12) -> Curry_Prelude.d_OP_eq_eq x10 Curry_AbstractCurry.C_Public x3500
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot_isPublicFunc_dot_7 x1002 x3500) (d_OP_showExports_dot_isPublicFunc_dot_7 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot_isPublicFunc_dot_7 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot_isPublicFunc_dot_7 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showExports_dot_getTypeName_dot_7 :: Curry_AbstractCurry.C_CTypeDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showExports_dot_getTypeName_dot_7 x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CType x2 x3 x4 x5) -> d_OP__case_180 x2 x3500
     (Curry_AbstractCurry.C_CTypeSyn x8 x9 x10 x11) -> d_OP__case_179 x8 x3500
     (Curry_AbstractCurry.Choice_C_CTypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot_getTypeName_dot_7 x1002 x3500) (d_OP_showExports_dot_getTypeName_dot_7 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot_getTypeName_dot_7 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot_getTypeName_dot_7 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showExports_dot_allPublicCons_dot_7 :: Curry_AbstractCurry.C_CTypeDecl -> ConstStore -> Curry_Prelude.C_Bool
d_OP_showExports_dot_allPublicCons_dot_7 x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CType x2 x3 x4 x5) -> Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length (Curry_Prelude.d_C_filter d_OP_showExports_dot_allPublicCons_dot_7_dot_isPublicCons_dot_47 x5 x3500) x3500) (Curry_Prelude.d_C_length x5 x3500) x3500
     (Curry_AbstractCurry.C_CTypeSyn x6 x7 x8 x9) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CTypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot_allPublicCons_dot_7 x1002 x3500) (d_OP_showExports_dot_allPublicCons_dot_7 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot_allPublicCons_dot_7 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot_allPublicCons_dot_7 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showExports_dot_allPublicCons_dot_7_dot_isPublicCons_dot_47 :: Curry_AbstractCurry.C_CConsDecl -> ConstStore -> Curry_Prelude.C_Bool
d_OP_showExports_dot_allPublicCons_dot_7_dot_isPublicCons_dot_47 x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CCons x2 x3 x4 x5) -> Curry_Prelude.d_OP_eq_eq x4 Curry_AbstractCurry.C_Public x3500
     (Curry_AbstractCurry.Choice_C_CConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot_allPublicCons_dot_7_dot_isPublicCons_dot_47 x1002 x3500) (d_OP_showExports_dot_allPublicCons_dot_7_dot_isPublicCons_dot_47 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot_allPublicCons_dot_7_dot_isPublicCons_dot_47 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot_allPublicCons_dot_7_dot_isPublicCons_dot_47 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showExports_dot_getFuncName_dot_7 :: Curry_AbstractCurry.C_CFuncDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showExports_dot_getFuncName_dot_7 x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CFunc x2 x3 x4 x5 x6) -> d_OP__case_178 x2 x3500
     (Curry_AbstractCurry.C_CmtFunc x9 x10 x11 x12 x13 x14) -> d_OP__case_176 x10 x3500
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot_getFuncName_dot_7 x1002 x3500) (d_OP_showExports_dot_getFuncName_dot_7 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot_getFuncName_dot_7 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot_getFuncName_dot_7 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showExports_dot___hash_selFP2_hash_withCons :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl) (Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl) -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl
d_OP_showExports_dot___hash_selFP2_hash_withCons x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot___hash_selFP2_hash_withCons x1002 x3500) (d_OP_showExports_dot___hash_selFP2_hash_withCons x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot___hash_selFP2_hash_withCons z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot___hash_selFP2_hash_withCons x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showExports_dot___hash_selFP3_hash_withoutCons :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl) (Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl) -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl
d_OP_showExports_dot___hash_selFP3_hash_withoutCons x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot___hash_selFP3_hash_withoutCons x1002 x3500) (d_OP_showExports_dot___hash_selFP3_hash_withoutCons x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot___hash_selFP3_hash_withoutCons z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot___hash_selFP3_hash_withoutCons x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showImports :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showImports x1 x3500 = Curry_Prelude.d_OP_plus_plus (d_C_prefixInter d_C_showImport (Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (d_C_prelude x3500)) x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) (d_OP__case_174 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (d_C_prelude x3500) Curry_Prelude.OP_List) x3500) x3500) x3500

d_C_showImport :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showImport x1 x3500 = d_OP__case_173 x1 (Curry_Prelude.d_OP_slash_eq x1 (d_C_prelude x3500) x3500) x3500

d_C_showOpDecls :: Curry_Prelude.OP_List Curry_AbstractCurry.C_COpDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showOpDecls x1 x3500 = Curry_Prelude.d_OP_plus_plus (d_C_prefixInter d_C_showOpDecl x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) (d_OP__case_172 x1 (Curry_Prelude.d_OP_eq_eq x1 Curry_Prelude.OP_List x3500) x3500) x3500

d_C_showOpDecl :: Curry_AbstractCurry.C_COpDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showOpDecl x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_COp x2 x3 x4) -> d_OP__case_171 x3 x4 x2 x3500
     (Curry_AbstractCurry.Choice_C_COpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showOpDecl x1002 x3500) (d_C_showOpDecl x1003 x3500)
     (Curry_AbstractCurry.Choices_C_COpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showOpDecl z x3500) x1002
     (Curry_AbstractCurry.Guard_C_COpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showOpDecl x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_COpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showFixity :: Curry_AbstractCurry.C_CFixity -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showFixity x1 x3500 = case x1 of
     Curry_AbstractCurry.C_CInfixOp -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List))))
     Curry_AbstractCurry.C_CInfixlOp -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))
     Curry_AbstractCurry.C_CInfixrOp -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))
     (Curry_AbstractCurry.Choice_C_CFixity x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showFixity x1002 x3500) (d_C_showFixity x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CFixity x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showFixity z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFixity x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showFixity x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFixity x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showTypeDecls :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTypeDecls x1 x3500 = Curry_Prelude.d_OP_plus_plus (d_C_prefixInter d_C_showTypeDecl x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3500) (d_OP__case_169 x1 (Curry_Prelude.d_OP_eq_eq x1 Curry_Prelude.OP_List x3500) x3500) x3500

d_C_showTypeDecl :: Curry_AbstractCurry.C_CTypeDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTypeDecl x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CTypeSyn x2 x3 x4 x5) -> d_OP__case_168 x4 x5 x2 x3500
     (Curry_AbstractCurry.C_CType x8 x9 x10 x11) -> d_OP__case_167 x10 x11 x8 x3500
     (Curry_AbstractCurry.Choice_C_CTypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showTypeDecl x1002 x3500) (d_C_showTypeDecl x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showTypeDecl z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showTypeDecl x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showConsDecl :: Curry_AbstractCurry.C_CConsDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showConsDecl x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CCons x2 x3 x4 x5) -> d_OP__case_166 x5 x2 x3500
     (Curry_AbstractCurry.Choice_C_CConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showConsDecl x1002 x3500) (d_C_showConsDecl x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showConsDecl z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showConsDecl x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showTypeExpr :: Curry_Prelude.C_Bool -> Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTypeExpr x1 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CTVar x3) -> d_OP__case_165 x3 x3500
     (Curry_AbstractCurry.C_CFuncType x6 x7) -> d_C_maybeShowBrackets x1 (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExpr (d_C_isCFuncType x6 x3500) x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (d_C_showTypeExpr Curry_Prelude.C_False x7 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CTCons x8 x9) -> d_OP__case_164 x1 x9 x8 x3500
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showTypeExpr x1 x1002 x3500) (d_C_showTypeExpr x1 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showTypeExpr x1 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showTypeExpr x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showTypeVar :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTypeVar x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_161 x2 x3 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char 'a'#) x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x3 x3500) x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Char.d_C_isDigit x3500) x3 x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showTypeVar x1002 x3500) (d_C_showTypeVar x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showTypeVar z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showTypeVar x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showIdentifier :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showIdentifier x3500 = Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_not (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))) x3500)

nd_C_showIdentifier :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_showIdentifier x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (Curry_Prelude.nd_C_filter (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_not) (wrapNX id (Curry_Prelude.nd_C_flip (wrapNX id Curry_Prelude.nd_C_elem) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)))) x2000 x3500))))

d_C_isCFuncType :: Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.C_Bool
d_C_isCFuncType x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CFuncType x2 x3) -> Curry_Prelude.C_True
     (Curry_AbstractCurry.C_CTVar x4) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CTCons x5 x6) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecordType x7 x8) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isCFuncType x1002 x3500) (d_C_isCFuncType x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isCFuncType z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isCFuncType x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showFuncDecl :: ConstStore -> Curry_AbstractCurry.C_CFuncDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showFuncDecl x3500 = d_C_showFuncDeclOpt (d_C_defaultOptions x3500)

nd_C_showFuncDecl :: IDSupply -> ConstStore -> Func Curry_AbstractCurry.C_CFuncDecl (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_showFuncDecl x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (nd_C_showFuncDeclOpt (nd_C_defaultOptions x2000 x3500))))

d_C_showFuncDeclOpt :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CFuncDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showFuncDeclOpt x1 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CmtFunc x3 x4 x5 x6 x7 x8) -> d_C_showCmtFunc x1 x3 (Curry_AbstractCurry.C_CFunc x4 x5 x6 x7 x8) x3500
     (Curry_AbstractCurry.C_CFunc x9 x10 x11 x12 x13) -> d_C_showCmtFunc x1 Curry_Prelude.OP_List x2 x3500
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showFuncDeclOpt x1 x1002 x3500) (d_C_showFuncDeclOpt x1 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showFuncDeclOpt x1 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showFuncDeclOpt x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showFuncDeclOpt :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CFuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showFuncDeclOpt x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CmtFunc x3 x4 x5 x6 x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showCmtFunc x1 x3 (Curry_AbstractCurry.C_CFunc x4 x5 x6 x7 x8) x2000 x3500))
     (Curry_AbstractCurry.C_CFunc x9 x10 x11 x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showCmtFunc x1 Curry_Prelude.OP_List x2 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showFuncDeclOpt x1 x1002 x3000 x3500) (nd_C_showFuncDeclOpt x1 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showFuncDeclOpt x1 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showFuncDeclOpt x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showCmtFunc :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CFuncDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCmtFunc x1 x2 x3 x3500 = case x3 of
     (Curry_AbstractCurry.C_CFunc x4 x5 x6 x7 x8) -> d_OP__case_160 x1 x2 x5 x7 x8 x4 x3500
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCmtFunc x1 x2 x1002 x3500) (d_C_showCmtFunc x1 x2 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCmtFunc x1 x2 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCmtFunc x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showCmtFunc :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CFuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showCmtFunc x1 x2 x3 x3000 x3500 = case x3 of
     (Curry_AbstractCurry.C_CFunc x4 x5 x6 x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_160 x1 x2 x5 x7 x8 x4 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showCmtFunc x1 x2 x1002 x3000 x3500) (nd_C_showCmtFunc x1 x2 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showCmtFunc x1 x2 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showCmtFunc x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showCmtFunc_dot_insertName_dot_142 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showCmtFunc_dot_insertName_dot_142 x1 x2 x3 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_153 x1 x2 x3 x5 x6 (Curry_Prelude.d_OP_slash_eq x3 (Curry_Prelude.C_Int 0#) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showCmtFunc_dot_insertName_dot_142 x1 x2 x3 x1002 x3500) (d_OP_showCmtFunc_dot_insertName_dot_142 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showCmtFunc_dot_insertName_dot_142 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showCmtFunc_dot_insertName_dot_142 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showCmtFunc_dot_rulePrints_dot_142 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CRule -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showCmtFunc_dot_rulePrints_dot_142 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_concat (Curry_Prelude.d_OP_dollar (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (d_OP_showCmtFunc_dot_insertName_dot_142 x1 x2 x5) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_span (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Prelude.C_Char ' '#))) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_tail (d_C_showRule x3) x3500) x3500) x3500) x4 x3500) x3500) x3500

nd_OP_showCmtFunc_dot_rulePrints_dot_142 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CRule -> Curry_Prelude.C_Int -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_showCmtFunc_dot_rulePrints_dot_142 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2010 = x3000
      in (seq x2010 (let
          x2009 = leftSupply x2010
          x2008 = rightSupply x2010
           in (seq x2009 (seq x2008 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_concat) (let
               x2007 = leftSupply x2008
               x2006 = rightSupply x2008
                in (seq x2007 (seq x2006 (Curry_Prelude.nd_OP_dollar (wrapDX id (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) (let
                    x2005 = leftSupply x2006
                    x2004 = rightSupply x2006
                     in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_map (let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dot (wrapDX id (d_OP_showCmtFunc_dot_insertName_dot_142 x1 x2 x5)) (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_span (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_slash_eq)) (Curry_Prelude.C_Char ' '#))))) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_tail) (wrapNX id (nd_C_showRule x3)) x2000 x3500) x2001 x3500)))) x2003 x3500)))) x4 x2005 x3500)))) x2007 x3500)))) x2009 x3500)))))

d_C_funcComment :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_funcComment x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_unlines (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) Curry_Prelude.d_C_lines x3500) x3500

nd_C_funcComment :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_funcComment x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_unlines) (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_map (wrapDX id (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) (wrapDX id Curry_Prelude.d_C_lines) x2000 x3500) x2001 x3500)))))

d_C_showLocalFuncDecl :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_AbstractCurry.C_CFuncDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showLocalFuncDecl x1 x3500 = d_C_showFuncDeclOpt x1

nd_C_showLocalFuncDecl :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Func Curry_AbstractCurry.C_CFuncDecl (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_showLocalFuncDecl x1 x3000 x3500 = wrapNX id (nd_C_showFuncDeclOpt x1)

d_C_showRule :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CRule -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showRule x1 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CRule x3 x4 x5) -> Curry_Prelude.d_OP_plus_plus (d_C_prefixMap d_C_showPattern x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3500) (Curry_Prelude.d_OP_plus_plus (d_C_showGuards x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List) x4 x3500) (d_OP__case_152 x1 x5 (Curry_Prelude.d_C_null x5 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.Choice_C_CRule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showRule x1 x1002 x3500) (d_C_showRule x1 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CRule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showRule x1 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CRule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showRule x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CRule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showRule :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CRule -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showRule x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CRule x3 x4 x5) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_prefixMap (wrapDX id d_C_showPattern) x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x2000 x3500) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_showGuards x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List) x4 x2001 x3500) (nd_OP__case_152 x1 x5 (Curry_Prelude.d_C_null x5 x3500) x2002 x3500) x3500)))) x3500)))))
     (Curry_AbstractCurry.Choice_C_CRule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showRule x1 x1002 x3000 x3500) (nd_C_showRule x1 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CRule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showRule x1 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CRule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showRule x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CRule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showEvalAnnot :: Curry_AbstractCurry.C_CEvalAnnot -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showEvalAnnot x1 x3500 = case x1 of
     Curry_AbstractCurry.C_CFlex -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List)))
     Curry_AbstractCurry.C_CRigid -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))
     Curry_AbstractCurry.C_CChoice -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))
     (Curry_AbstractCurry.Choice_C_CEvalAnnot x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showEvalAnnot x1002 x3500) (d_C_showEvalAnnot x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CEvalAnnot x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showEvalAnnot z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CEvalAnnot x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showEvalAnnot x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CEvalAnnot x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showGuards :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CExpr Curry_AbstractCurry.C_CExpr) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showGuards x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_151 x1 x2 x3 x5 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showGuards x1 x2 x1002 x3500) (d_C_showGuards x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showGuards x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showGuards x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showGuards :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CExpr Curry_AbstractCurry.C_CExpr) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showGuards x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_151 x1 x2 x3 x5 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showGuards x1 x2 x1002 x3000 x3500) (nd_C_showGuards x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showGuards x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showGuards x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showGuard :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CExpr Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showGuard x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (d_C_showExprOpt x1 x4 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (d_C_showExprOpt x1 x5 x3500)) x3500)) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showGuard x1 x2 x1002 x3500) (d_C_showGuard x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showGuard x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showGuard x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showGuard :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CExpr Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showGuard x1 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showExprOpt x1 x4 x2000 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (nd_C_showExprOpt x1 x5 x2001 x3500)) x3500)) x3500)))) x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showGuard x1 x2 x1002 x3000 x3500) (nd_C_showGuard x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showGuard x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showGuard x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showLocalDecl :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CLocalDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showLocalDecl x1 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CLocalFunc x3) -> Curry_Prelude.d_C_apply (d_C_showLocalFuncDecl x1 x3500) x3 x3500
     (Curry_AbstractCurry.C_CLocalPat x4 x5 x6) -> Curry_Prelude.d_OP_plus_plus (d_C_showPattern x4 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (d_C_showExprOpt x1 x5 x3500) (d_OP__case_148 x1 x6 (Curry_Prelude.d_C_null x6 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLocalVar x7) -> Curry_Prelude.d_OP_plus_plus (d_C_showPattern (Curry_AbstractCurry.C_CPVar x7) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) x3500
     (Curry_AbstractCurry.Choice_C_CLocalDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showLocalDecl x1 x1002 x3500) (d_C_showLocalDecl x1 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CLocalDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showLocalDecl x1 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CLocalDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showLocalDecl x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CLocalDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showLocalDecl :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CLocalDecl -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showLocalDecl x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CLocalFunc x3) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_showLocalFuncDecl x1 x2000 x3500) x3 x2001 x3500)))))
     (Curry_AbstractCurry.C_CLocalPat x4 x5 x6) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (d_C_showPattern x4 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showExprOpt x1 x5 x2000 x3500) (nd_OP__case_148 x1 x6 (Curry_Prelude.d_C_null x6 x3500) x2001 x3500) x3500)))) x3500) x3500))
     (Curry_AbstractCurry.C_CLocalVar x7) -> Curry_Prelude.d_OP_plus_plus (d_C_showPattern (Curry_AbstractCurry.C_CPVar x7) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) x3500
     (Curry_AbstractCurry.Choice_C_CLocalDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showLocalDecl x1 x1002 x3000 x3500) (nd_C_showLocalDecl x1 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CLocalDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showLocalDecl x1 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CLocalDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showLocalDecl x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CLocalDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showExpr :: ConstStore -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showExpr x3500 = d_C_showExprOpt (d_C_defaultOptions x3500)

nd_C_showExpr :: IDSupply -> ConstStore -> Func Curry_AbstractCurry.C_CExpr (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_showExpr x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (nd_C_showExprOpt (nd_C_defaultOptions x2000 x3500))))

d_C_showExprOpt :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showExprOpt x1 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CVar x3) -> d_OP__case_147 x3 x3500
     (Curry_AbstractCurry.C_CLit x6) -> d_C_showLiteral x6 x3500
     (Curry_AbstractCurry.C_CSymbol x7) -> d_OP__case_146 x1 x7 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) (Curry_Prelude.d_C_snd x7 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CApply x8 x9) -> d_C_showApplication x1 (Curry_AbstractCurry.C_CApply x8 x9) x3500
     (Curry_AbstractCurry.C_CLambda x10 x11) -> d_C_showLambdaOrSection x1 x10 x11 x3500
     (Curry_AbstractCurry.C_CLetDecl x12 x13) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (d_C_showBlock (Curry_Prelude.d_OP_plus_plus (d_C_combineMap (d_C_showLocalDecl x1) x12 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (d_C_showBoxedExpr x1 x13 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CDoExpr x14) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))) (d_C_showBlock (d_C_combineMap (d_C_showStatement x1) x14 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CListComp x15 x16) -> Curry_Prelude.d_OP_dollar d_C_brackets (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x15 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (d_C_combineMap (d_C_showStatement x1) x16 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CCase x17 x18) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x17 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (d_C_showBlock (d_C_combineMap (d_C_showBranchExpr x1) x18 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showExprOpt x1 x1002 x3500) (d_C_showExprOpt x1 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showExprOpt x1 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showExprOpt x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showExprOpt :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showExprOpt x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CVar x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_147 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLit x6) -> d_C_showLiteral x6 x3500
     (Curry_AbstractCurry.C_CSymbol x7) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_146 x1 x7 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) (Curry_Prelude.d_C_snd x7 x3500) x2001 x3500)))) x2003 x3500)))))
     (Curry_AbstractCurry.C_CApply x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showApplication x1 (Curry_AbstractCurry.C_CApply x8 x9) x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambdaOrSection x1 x10 x11 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x12 x13) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (d_C_showBlock (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_combineMap (wrapNX id (nd_C_showLocalDecl x1)) x12 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (nd_C_showBoxedExpr x1 x13 x2001 x3500) x3500) x3500)))) x3500) x3500))
     (Curry_AbstractCurry.C_CDoExpr x14) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))) (d_C_showBlock (nd_C_combineMap (wrapNX id (nd_C_showStatement x1)) x14 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x2000 x3500) x3500) x3500))
     (Curry_AbstractCurry.C_CListComp x15 x16) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_brackets) (let
                    x2000 = leftSupply x2002
                    x2001 = rightSupply x2002
                     in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x15 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (nd_C_combineMap (wrapNX id (nd_C_showStatement x1)) x16 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x2001 x3500) x3500) x3500)))) x2003 x3500)))))
     (Curry_AbstractCurry.C_CCase x17 x18) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x17 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (d_C_showBlock (nd_C_combineMap (wrapNX id (nd_C_showBranchExpr x1)) x18 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x2001 x3500) x3500) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showExprOpt x1 x1002 x3000 x3500) (nd_C_showExprOpt x1 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showExprOpt x1 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showExprOpt x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showSymbol :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSymbol x1 x2 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_145 x3 x4 x2 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showSymbol x1002 x2 x3500) (d_C_showSymbol x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showSymbol z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showSymbol x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showSymbol :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showSymbol x1 x2 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_145 x3 x4 x2 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showSymbol x1002 x2 x3000 x3500) (nd_C_showSymbol x1003 x2 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showSymbol z x2 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showSymbol x1002 x2 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showLambdaOrSection :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showLambdaOrSection x1 x2 x3 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_141 x1 x2 x3 x5 x4 x3500
     Curry_Prelude.OP_List -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showLambdaOrSection x1 x1002 x3 x3500) (d_C_showLambdaOrSection x1 x1003 x3 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showLambdaOrSection x1 z x3 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showLambdaOrSection x1 x1002 x3) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showLambdaOrSection :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showLambdaOrSection x1 x2 x3 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_141 x1 x2 x3 x5 x4 x2000 x3500))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showLambdaOrSection x1 x1002 x3 x3000 x3500) (nd_C_showLambdaOrSection x1 x1003 x3 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showLambdaOrSection x1 z x3 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showLambdaOrSection x1 x1002 x3 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showLambda :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showLambda x1 x2 x3 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_combineMap d_C_showPattern x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (d_C_showExprOpt x1 x3 x3500) x3500) x3500) x3500

nd_C_showLambda :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showLambda x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) Curry_Prelude.OP_List) (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_combineMap (wrapDX id d_C_showPattern) x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (nd_C_showExprOpt x1 x3 x2001 x3500) x3500) x3500)))) x3500))

d_C_showStatement :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CStatement -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showStatement x1 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CSExpr x3) -> d_C_showExprOpt x1 x3 x3500
     (Curry_AbstractCurry.C_CSPat x4 x5) -> Curry_Prelude.d_OP_plus_plus (d_C_showPattern x4 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (d_C_showExprOpt x1 x5 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CSLet x6) -> d_OP__case_109 x1 x6 x3500
     (Curry_AbstractCurry.Choice_C_CStatement x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showStatement x1 x1002 x3500) (d_C_showStatement x1 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CStatement x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showStatement x1 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CStatement x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showStatement x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CStatement x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showStatement :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CStatement -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showStatement x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CSExpr x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showExprOpt x1 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSPat x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showPattern x4 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (nd_C_showExprOpt x1 x5 x2000 x3500) x3500) x3500))
     (Curry_AbstractCurry.C_CSLet x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_109 x1 x6 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CStatement x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showStatement x1 x1002 x3000 x3500) (nd_C_showStatement x1 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CStatement x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showStatement x1 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CStatement x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showStatement x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CStatement x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showPattern :: Curry_AbstractCurry.C_CPattern -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showPattern x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CPVar x2) -> d_OP__case_107 x2 x3500
     (Curry_AbstractCurry.C_CPLit x5) -> d_C_showLiteral x5 x3500
     (Curry_AbstractCurry.C_CPComb x6 x7) -> d_OP__case_106 x7 x6 x3500
     (Curry_AbstractCurry.C_CPAs x12 x13) -> d_OP__case_101 x13 x12 x3500
     (Curry_AbstractCurry.C_CPFuncComb x16 x17) -> d_C_showPattern (Curry_AbstractCurry.C_CPComb x16 x17) x3500
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showPattern x1002 x3500) (d_C_showPattern x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showPattern z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showPattern x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showPreludeCons :: Curry_AbstractCurry.C_CPattern -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showPreludeCons x1 x3500 = let
     x2 = d_OP_showPreludeCons_dot___hash_selFP5_hash_name x1 x3500
     x3 = d_OP_showPreludeCons_dot___hash_selFP6_hash_pattlist x1 x3500
      in (d_OP__case_100 x1 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x3500) x3500)

d_OP_showPreludeCons_dot___hash_selFP5_hash_name :: Curry_AbstractCurry.C_CPattern -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showPreludeCons_dot___hash_selFP5_hash_name x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CPComb x2 x3) -> d_OP__case_97 x2 x3500
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showPreludeCons_dot___hash_selFP5_hash_name x1002 x3500) (d_OP_showPreludeCons_dot___hash_selFP5_hash_name x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showPreludeCons_dot___hash_selFP5_hash_name z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showPreludeCons_dot___hash_selFP5_hash_name x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showPreludeCons_dot___hash_selFP6_hash_pattlist :: Curry_AbstractCurry.C_CPattern -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern
d_OP_showPreludeCons_dot___hash_selFP6_hash_pattlist x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CPComb x2 x3) -> d_OP__case_96 x3 x2 x3500
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showPreludeCons_dot___hash_selFP6_hash_pattlist x1002 x3500) (d_OP_showPreludeCons_dot___hash_selFP6_hash_pattlist x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showPreludeCons_dot___hash_selFP6_hash_pattlist z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showPreludeCons_dot___hash_selFP6_hash_pattlist x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showPatternList :: Curry_AbstractCurry.C_CPattern -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showPatternList x1 x3500 = d_OP__case_95 x1 (d_C_isClosedStringPattern x1 x3500) x3500

d_C_showPatListElems :: Curry_AbstractCurry.C_CPattern -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_showPatListElems x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CPComb x2 x3) -> d_OP__case_91 x3 x2 x3500
     (Curry_AbstractCurry.C_CPVar x14) -> Curry_Prelude.OP_Cons (d_C_showPattern (Curry_AbstractCurry.C_CPVar x14) x3500) Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CPAs x15 x16) -> Curry_Prelude.OP_Cons (d_C_showPattern (Curry_AbstractCurry.C_CPAs x15 x16) x3500) Curry_Prelude.OP_List
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showPatListElems x1002 x3500) (d_C_showPatListElems x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showPatListElems z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showPatListElems x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isClosedPatternList :: Curry_AbstractCurry.C_CPattern -> ConstStore -> Curry_Prelude.C_Bool
d_C_isClosedPatternList x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CPComb x2 x3) -> d_OP__case_80 x3 x2 x3500
     (Curry_AbstractCurry.C_CPVar x14) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CPAs x15 x16) -> d_C_isClosedPatternList x16 x3500
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isClosedPatternList x1002 x3500) (d_C_isClosedPatternList x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isClosedPatternList z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isClosedPatternList x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isClosedStringPattern :: Curry_AbstractCurry.C_CPattern -> ConstStore -> Curry_Prelude.C_Bool
d_C_isClosedStringPattern x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CPComb x2 x3) -> d_OP__case_69 x3 x2 x3500
     (Curry_AbstractCurry.C_CPVar x14) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isClosedStringPattern x1002 x3500) (d_C_isClosedStringPattern x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isClosedStringPattern z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isClosedStringPattern x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isCharPattern :: Curry_AbstractCurry.C_CPattern -> ConstStore -> Curry_Prelude.C_Bool
d_C_isCharPattern x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CPLit x2) -> d_OP__case_58 x2 x3500
     (Curry_AbstractCurry.C_CPVar x6) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CPComb x7 x8) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CPAs x9 x10) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CPFuncComb x11 x12) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CPLazy x13) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CPRecord x14 x15) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isCharPattern x1002 x3500) (d_C_isCharPattern x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isCharPattern z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isCharPattern x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isAsPattern :: Curry_AbstractCurry.C_CPattern -> ConstStore -> Curry_Prelude.C_Bool
d_C_isAsPattern x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CPAs x2 x3) -> Curry_Prelude.C_True
     (Curry_AbstractCurry.C_CPVar x4) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CPLit x5) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CPComb x6 x7) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CPFuncComb x8 x9) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CPLazy x10) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CPRecord x11 x12) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isAsPattern x1002 x3500) (d_C_isAsPattern x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isAsPattern z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isAsPattern x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showAsPatternList :: Curry_AbstractCurry.C_CPattern -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showAsPatternList x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CPAs x2 x3) -> d_OP__case_57 x3 x2 x3500
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showAsPatternList x1002 x3500) (d_C_showAsPatternList x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showAsPatternList z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showAsPatternList x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showBranchExpr :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CBranchExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showBranchExpr x1 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CBranch x3 x4) -> Curry_Prelude.d_OP_plus_plus (d_C_showPattern x3 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (d_C_showExprOpt x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CGuardedBranch x5 x6) -> Curry_Prelude.d_OP_plus_plus (d_C_showPattern x5 x3500) (d_C_showGuards x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)) x6 x3500) x3500
     (Curry_AbstractCurry.Choice_C_CBranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showBranchExpr x1 x1002 x3500) (d_C_showBranchExpr x1 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CBranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showBranchExpr x1 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CBranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showBranchExpr x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CBranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showBranchExpr :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CBranchExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showBranchExpr x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CBranch x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showPattern x3 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (nd_C_showExprOpt x1 x4 x2000 x3500) x3500) x3500))
     (Curry_AbstractCurry.C_CGuardedBranch x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showPattern x5 x3500) (nd_C_showGuards x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)) x6 x2000 x3500) x3500))
     (Curry_AbstractCurry.Choice_C_CBranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showBranchExpr x1 x1002 x3000 x3500) (nd_C_showBranchExpr x1 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CBranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showBranchExpr x1 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CBranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showBranchExpr x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CBranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showLiteral :: Curry_AbstractCurry.C_CLiteral -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showLiteral x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CIntc x2) -> Curry_Prelude.d_C_show x2 x3500
     (Curry_AbstractCurry.C_CFloatc x3) -> Curry_Prelude.d_C_show x3 x3500
     (Curry_AbstractCurry.C_CCharc x4) -> Curry_Prelude.d_OP_dollar d_C_quotes (d_C_showCCharc (Curry_AbstractCurry.C_CCharc x4) x3500) x3500
     (Curry_AbstractCurry.Choice_C_CLiteral x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showLiteral x1002 x3500) (d_C_showLiteral x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CLiteral x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showLiteral z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CLiteral x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showLiteral x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CLiteral x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showCCharc :: Curry_AbstractCurry.C_CLiteral -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCCharc x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CCharc x2) -> d_OP__case_56 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\n'#) x3500) x3500
     (Curry_AbstractCurry.Choice_C_CLiteral x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCCharc x1002 x3500) (d_C_showCCharc x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CLiteral x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCCharc z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CLiteral x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCCharc x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CLiteral x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showBlock :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showBlock x1 x3500 = d_C_combineMap Curry_Prelude.d_C_id (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_slash_eq Curry_Prelude.OP_List) (Curry_Prelude.d_C_lines x1 x3500) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500

d_C_showTypeCons :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTypeCons x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_51 x1 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x1 (d_C_prelude x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showTypeCons x1 x2 x1002 x3500) (d_C_showTypeCons x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showTypeCons x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showTypeCons x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showPreludeTypeCons :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showPreludeTypeCons x1 x2 x3500 = d_OP__case_49 x1 x2 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x2 x3500) (Curry_AbstractCurry.C_CTCons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List) x3500) x3500) x3500

d_C_showApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showApplication x1 x2 x3500 = d_OP__case_45 x1 x2 (d_C_applicationHead x2 x3500) x3500

nd_C_showApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showApplication x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_45 x1 x2 (d_C_applicationHead x2 x3500) x2000 x3500))

d_C_applicationHead :: Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_AbstractCurry.C_CExpr
d_C_applicationHead x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CApply x2 x3) -> d_C_applicationHead x2 x3500
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
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_applicationHead x1002 x3500) (d_C_applicationHead x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_applicationHead z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_applicationHead x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showSymbolApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSymbolApplication x1 x2 x3 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_44 x1 x3 x4 x5 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showSymbolApplication x1 x1002 x3 x3500) (d_C_showSymbolApplication x1 x1003 x3 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showSymbolApplication x1 z x3 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showSymbolApplication x1 x1002 x3) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showSymbolApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showSymbolApplication x1 x2 x3 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_44 x1 x3 x4 x5 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showSymbolApplication x1 x1002 x3 x3000 x3500) (nd_C_showSymbolApplication x1 x1003 x3 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showSymbolApplication x1 z x3 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showSymbolApplication x1 x1002 x3 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showListApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showListApplication x1 x2 x3500 = d_OP__case_39 x1 x2 (d_C_isStringList x2 x3500) x3500

nd_C_showListApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showListApplication x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_39 x1 x2 (d_C_isStringList x2 x3500) x2000 x3500))

d_C_showCharListApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCharListApplication x1 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x3 x4) -> d_OP__case_36 x1 x4 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCharListApplication x1 x1002 x3500) (d_C_showCharListApplication x1 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCharListApplication x1 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCharListApplication x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showCharListApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showCharListApplication x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_36 x1 x4 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showCharListApplication x1 x1002 x3000 x3500) (nd_C_showCharListApplication x1 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showCharListApplication x1 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showCharListApplication x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showConsListApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showConsListApplication x1 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x3 x4) -> d_OP__case_33 x1 x4 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showConsListApplication x1 x1002 x3500) (d_C_showConsListApplication x1 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showConsListApplication x1 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showConsListApplication x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showConsListApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showConsListApplication x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_33 x1 x4 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showConsListApplication x1 x1002 x3000 x3500) (nd_C_showConsListApplication x1 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showConsListApplication x1 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showConsListApplication x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showSimpleListApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSimpleListApplication x1 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x3 x4) -> d_OP__case_31 x1 x4 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showSimpleListApplication x1 x1002 x3500) (d_C_showSimpleListApplication x1 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showSimpleListApplication x1 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showSimpleListApplication x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showSimpleListApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showSimpleListApplication x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_31 x1 x4 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showSimpleListApplication x1 x1002 x3000 x3500) (nd_C_showSimpleListApplication x1 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showSimpleListApplication x1 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showSimpleListApplication x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showInfixApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showInfixApplication x1 x2 x3 x3500 = case x3 of
     (Curry_AbstractCurry.C_CApply x4 x5) -> d_OP__case_28 x1 x2 x5 x4 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showInfixApplication x1 x2 x1002 x3500) (d_C_showInfixApplication x1 x2 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showInfixApplication x1 x2 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showInfixApplication x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showInfixApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showInfixApplication x1 x2 x3 x3000 x3500 = case x3 of
     (Curry_AbstractCurry.C_CApply x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_28 x1 x2 x5 x4 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showInfixApplication x1 x2 x1002 x3000 x3500) (nd_C_showInfixApplication x1 x2 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showInfixApplication x1 x2 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showInfixApplication x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showITEApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showITEApplication x1 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x3 x4) -> d_OP__case_26 x1 x4 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showITEApplication x1 x1002 x3500) (d_C_showITEApplication x1 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showITEApplication x1 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showITEApplication x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showITEApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showITEApplication x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_26 x1 x4 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showITEApplication x1 x1002 x3000 x3500) (nd_C_showITEApplication x1 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showITEApplication x1 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showITEApplication x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showTupleApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTupleApplication x1 x2 x3500 = d_C_parens (d_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 x2 x3500) x3500

nd_C_showTupleApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showTupleApplication x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (d_C_parens (nd_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 x2 x2000 x3500) x3500))

d_OP_showTupleApplication_dot_p_showTuple_dot_401 :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x3 x4) -> d_OP__case_23 x1 x4 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 x1002 x3500) (d_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_showTupleApplication_dot_p_showTuple_dot_401 :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_23 x1 x4 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 x1002 x3000 x3500) (nd_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showSimpleApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSimpleApplication x1 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x3 x4) -> Curry_Prelude.d_OP_plus_plus (d_C_showSimpleApplication x1 x3 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CVar x5) -> d_C_showBoxedExpr x1 x2 x3500
     (Curry_AbstractCurry.C_CLit x6) -> d_C_showBoxedExpr x1 x2 x3500
     (Curry_AbstractCurry.C_CSymbol x7) -> d_C_showBoxedExpr x1 x2 x3500
     (Curry_AbstractCurry.C_CLambda x8 x9) -> d_C_showBoxedExpr x1 x2 x3500
     (Curry_AbstractCurry.C_CLetDecl x10 x11) -> d_C_showBoxedExpr x1 x2 x3500
     (Curry_AbstractCurry.C_CDoExpr x12) -> d_C_showBoxedExpr x1 x2 x3500
     (Curry_AbstractCurry.C_CListComp x13 x14) -> d_C_showBoxedExpr x1 x2 x3500
     (Curry_AbstractCurry.C_CCase x15 x16) -> d_C_showBoxedExpr x1 x2 x3500
     (Curry_AbstractCurry.C_CRecConstr x17) -> d_C_showBoxedExpr x1 x2 x3500
     (Curry_AbstractCurry.C_CRecSelect x18 x19) -> d_C_showBoxedExpr x1 x2 x3500
     (Curry_AbstractCurry.C_CRecUpdate x20 x21) -> d_C_showBoxedExpr x1 x2 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showSimpleApplication x1 x1002 x3500) (d_C_showSimpleApplication x1 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showSimpleApplication x1 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showSimpleApplication x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showSimpleApplication :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showSimpleApplication x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showSimpleApplication x1 x3 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CVar x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showBoxedExpr x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CLit x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showBoxedExpr x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showBoxedExpr x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showBoxedExpr x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showBoxedExpr x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x12) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showBoxedExpr x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x13 x14) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showBoxedExpr x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x15 x16) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showBoxedExpr x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x17) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showBoxedExpr x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x18 x19) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showBoxedExpr x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x20 x21) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showBoxedExpr x1 x2 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showSimpleApplication x1 x1002 x3000 x3500) (nd_C_showSimpleApplication x1 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showSimpleApplication x1 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showSimpleApplication x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showBoxedExpr :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showBoxedExpr x1 x2 x3500 = d_OP__case_22 x1 x2 (d_C_isSimpleExpr x2 x3500) x3500

nd_C_showBoxedExpr :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CExpr -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showBoxedExpr x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_22 x1 x2 (d_C_isSimpleExpr x2 x3500) x2000 x3500))

d_C_prefixMap :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_prefixMap x1 x2 x3 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_plus_plus x3) x3500) (Curry_Prelude.d_C_map x1 x2 x3500) x3500

nd_C_prefixMap :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_prefixMap x1 x2 x3 x3000 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id (Curry_Prelude.d_OP_plus_plus x3)) x2000 x3500) (Curry_Prelude.nd_C_map x1 x2 x2001 x3500) x2002 x3500))))))))

d_C_prefixInter :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_prefixInter x1 x2 x3 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse x3 (Curry_Prelude.d_C_map x1 x2 x3500) x3500) x3500

nd_C_prefixInter :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_prefixInter x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_concat) (Curry_List.d_C_intersperse x3 (Curry_Prelude.nd_C_map x1 x2 x2000 x3500) x3500) x2001 x3500)))))

d_C_combineMap :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_combineMap x1 x2 x3 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x4 x3500) (d_C_prefixMap x1 x5 x3 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_combineMap x1 x1002 x3 x3500) (d_C_combineMap x1 x1003 x3 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_combineMap x1 z x3 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_combineMap x1 x1002 x3) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_combineMap :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_combineMap x1 x2 x3 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x4 x2000 x3500) (nd_C_prefixMap x1 x5 x3 x2001 x3500) x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_combineMap x1 x1002 x3 x3000 x3500) (nd_C_combineMap x1 x1003 x3 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_combineMap x1 z x3 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_combineMap x1 x1002 x3 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_dropTags :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_dropTags x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x4 = x2
           in (d_OP__case_20 x3 x4 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Char '"'#) x3500) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_dropTags x1002 x3500) (d_C_dropTags x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_dropTags z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_dropTags x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_parens :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_parens x1 x3500 = d_C_surroundWith (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x1 x3500

d_C_brackets :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_brackets x1 x3500 = d_C_surroundWith (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x1 x3500

d_C_quotes :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_quotes x1 x3500 = d_C_surroundWith (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List) x1 x3500

d_C_doubleQuotes :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_doubleQuotes x1 x3500 = d_C_surroundWith (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x1 x3500

d_C_backQuotes :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_backQuotes x1 x3500 = d_C_surroundWith (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List) x1 x3500

d_C_surroundWith :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_surroundWith x1 x2 x3 x3500 = Curry_Prelude.d_C_concat (Curry_Prelude.OP_Cons x1 (Curry_Prelude.OP_Cons x3 (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List))) x3500

d_C_isInfixOpName :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_C_isInfixOpName x3500 = Curry_Prelude.d_C_all (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem (d_C_infixIDs x3500)) x3500

nd_C_isInfixOpName :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool
nd_C_isInfixOpName x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_all (wrapNX id (Curry_Prelude.nd_C_flip (wrapNX id Curry_Prelude.nd_C_elem) (d_C_infixIDs x3500))) x2000 x3500))

d_C_isStringList :: Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.C_Bool
d_C_isStringList x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CSymbol x2) -> d_OP__case_18 x2 x3500
     (Curry_AbstractCurry.C_CVar x5) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CApply x6 x7) -> d_OP__case_17 x7 x6 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isStringList x1002 x3500) (d_C_isStringList x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isStringList z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isStringList x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isClosedList :: Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.C_Bool
d_C_isClosedList x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CApply x2 x3) -> d_OP__case_14 x3 x2 x3500
     (Curry_AbstractCurry.C_CSymbol x44) -> d_OP__case_11 x44 x3500
     (Curry_AbstractCurry.C_CVar x47) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLit x48) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLambda x49 x50) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLetDecl x51 x52) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CDoExpr x53) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CListComp x54 x55) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CCase x56 x57) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecConstr x58) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecSelect x59 x60) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecUpdate x61 x62) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isClosedList x1002 x3500) (d_C_isClosedList x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isClosedList z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isClosedList x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isSimpleExpr :: Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.C_Bool
d_C_isSimpleExpr x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CVar x2) -> Curry_Prelude.C_True
     (Curry_AbstractCurry.C_CLit x3) -> Curry_Prelude.C_True
     (Curry_AbstractCurry.C_CSymbol x4) -> d_OP__case_10 x4 x3500
     (Curry_AbstractCurry.C_CApply x7 x8) -> d_OP__case_9 x7 (d_C_applicationHead x7 x3500) x3500
     (Curry_AbstractCurry.C_CLambda x30 x31) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLetDecl x32 x33) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CDoExpr x34) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CListComp x35 x36) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CCase x37 x38) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecConstr x39) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecSelect x40 x41) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecUpdate x42 x43) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isSimpleExpr x1002 x3500) (d_C_isSimpleExpr x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isSimpleExpr z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isSimpleExpr x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isAtom :: Curry_AbstractCurry.C_CExpr -> ConstStore -> Curry_Prelude.C_Bool
d_C_isAtom x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CVar x2) -> Curry_Prelude.C_True
     (Curry_AbstractCurry.C_CLit x3) -> Curry_Prelude.C_True
     (Curry_AbstractCurry.C_CSymbol x4) -> d_OP__case_7 x4 x3500
     (Curry_AbstractCurry.C_CApply x7 x8) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLambda x9 x10) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLetDecl x11 x12) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CDoExpr x13) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CListComp x14 x15) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CCase x16 x17) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecConstr x18) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecSelect x19 x20) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecUpdate x21 x22) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isAtom x1002 x3500) (d_C_isAtom x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isAtom z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isAtom x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isUntyped :: Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.C_Bool
d_C_isUntyped x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CTCons x2 x3) -> d_OP__case_6 x3 x2 x3500
     (Curry_AbstractCurry.C_CTVar x8) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CFuncType x9 x10) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecordType x11 x12) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isUntyped x1002 x3500) (d_C_isUntyped x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isUntyped z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isUntyped x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isTuple :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_C_isTuple x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '('#) x3500) (d_OP_isTuple_dot_p1_isTuple_dot_519 x3 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isTuple x1002 x3500) (d_C_isTuple x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isTuple z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isTuple x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_isTuple_dot_p1_isTuple_dot_519 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isTuple_dot_p1_isTuple_dot_519 x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_4 x2 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isTuple_dot_p1_isTuple_dot_519 x1002 x3500) (d_OP_isTuple_dot_p1_isTuple_dot_519 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isTuple_dot_p1_isTuple_dot_519 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isTuple_dot_p1_isTuple_dot_519 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_infixIDs :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_infixIDs x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '~'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '%'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '^'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)))))))))))))))))))

d_C_prelude :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_prelude x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))

d_C_maybeShowBrackets :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_maybeShowBrackets x1 x2 x3500 = Curry_Prelude.d_OP_plus_plus (d_OP__case_3 x1 x3500) (Curry_Prelude.d_OP_plus_plus x2 (d_OP__case_2 x1 x3500) x3500) x3500

d_C_nameFM :: ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CFuncDecl -> ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit
d_C_nameFM x3500 = Curry_Prelude.d_C_foldr (acceptCs id d_C_addName) (Curry_FiniteMap.d_C_emptyFM (acceptCs id d_C_lessString) x3500)

nd_C_nameFM :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_AbstractCurry.C_CFuncDecl) (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit)
nd_C_nameFM x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id nd_C_addName)) (Curry_FiniteMap.nd_C_emptyFM (wrapDX (wrapDX id) (acceptCs id d_C_lessString)) x2000 x3500))))

d_C_addName :: Curry_AbstractCurry.C_CFuncDecl -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit -> ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit
d_C_addName x1 x2 x3500 = case x1 of
     (Curry_AbstractCurry.C_CFunc x3 x4 x5 x6 x7) -> d_OP__case_1 x2 x3 x3500
     (Curry_AbstractCurry.C_CmtFunc x10 x11 x12 x13 x14 x15) -> d_OP__case_0 x2 x11 x3500
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_addName x1002 x2 x3500) (d_C_addName x1003 x2 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_addName z x2 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_addName x1002 x2) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_addName :: Curry_AbstractCurry.C_CFuncDecl -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit -> IDSupply -> ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.OP_Unit
nd_C_addName x1 x2 x3000 x3500 = case x1 of
     (Curry_AbstractCurry.C_CFunc x3 x4 x5 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CmtFunc x10 x11 x12 x13 x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x2 x11 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_addName x1002 x2 x3000 x3500) (nd_C_addName x1003 x2 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_addName z x2 x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_addName x1002 x2 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_lessString :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_C_lessString x1 x2 x3500 = Curry_Prelude.d_OP_eq_eq Curry_Prelude.C_LT (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Sort.d_C_cmpString x3500) x1 x3500) x2 x3500) x3500

d_OP__case_0 x2 x11 x3500 = case x11 of
     (Curry_Prelude.OP_Tuple2 x16 x17) -> Curry_FiniteMap.d_C_addToFM x2 x17 Curry_Prelude.OP_Unit x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x1002 x3500) (d_OP__case_0 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x2 x11 x3000 x3500 = case x11 of
     (Curry_Prelude.OP_Tuple2 x16 x17) -> let
          x2000 = x3000
           in (seq x2000 (Curry_FiniteMap.nd_C_addToFM x2 x17 Curry_Prelude.OP_Unit x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x2 x1002 x3000 x3500) (nd_OP__case_0 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> Curry_FiniteMap.d_C_addToFM x2 x9 Curry_Prelude.OP_Unit x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x2 x1002 x3500) (d_OP__case_1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (Curry_FiniteMap.nd_C_addToFM x2 x9 Curry_Prelude.OP_Unit x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x2 x1002 x3000 x3500) (nd_OP__case_1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1002 x3500) (d_OP__case_2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x3000 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1002 x3000 x3500) (nd_OP__case_2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1002 x3500) (d_OP__case_3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x3000 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1002 x3000 x3500) (nd_OP__case_3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char ')'#) x3500
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char ','#) x3500) (d_OP_isTuple_dot_p1_isTuple_dot_519 (Curry_Prelude.OP_Cons x4 x5) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x2 x1002 x3500) (d_OP__case_4 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char ')'#) x3500
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char ','#) x3500) (d_OP_isTuple_dot_p1_isTuple_dot_519 (Curry_Prelude.OP_Cons x4 x5) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x2 x1002 x3000 x3500) (nd_OP__case_4 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x3 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_5 x4 x5 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x3 x1002 x3500) (d_OP__case_6 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x3 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x4 x5 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x3 x1002 x3000 x3500) (nd_OP__case_6 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x4 x5 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x3500) x3500
     (Curry_Prelude.OP_Cons x6 x7) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x4 x5 x1002 x3500) (d_OP__case_5 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x4 x5 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x3500) x3500
     (Curry_Prelude.OP_Cons x6 x7) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x4 x5 x1002 x3000 x3500) (nd_OP__case_5 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_not (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x6 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1002 x3500) (d_OP__case_7 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_not) (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x6 x2001 x3500)))) x2003 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1002 x3000 x3500) (nd_OP__case_7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x7 x30 x3500 = case x30 of
     (Curry_AbstractCurry.C_CSymbol x9) -> d_OP__case_8 x9 x3500
     (Curry_AbstractCurry.C_CVar x12) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLit x13) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CApply x14 x15) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLambda x16 x17) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLetDecl x18 x19) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CDoExpr x20) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CListComp x21 x22) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CCase x23 x24) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecConstr x25) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecSelect x26 x27) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecUpdate x28 x29) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x7 x1002 x3500) (d_OP__case_9 x7 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x7 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x7 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x7 x30 x3000 x3500 = case x30 of
     (Curry_AbstractCurry.C_CSymbol x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x9 x2000 x3500))
     (Curry_AbstractCurry.C_CVar x12) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLit x13) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CApply x14 x15) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLambda x16 x17) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLetDecl x18 x19) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CDoExpr x20) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CListComp x21 x22) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CCase x23 x24) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecConstr x25) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecSelect x26 x27) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecUpdate x28 x29) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x7 x1002 x3000 x3500) (nd_OP__case_9 x7 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x7 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x10 x11) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x10 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3500) (d_C_isTuple x11 x3500) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1002 x3500) (d_OP__case_8 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x10 x11) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x10 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3500) (d_C_isTuple x11 x3500) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1002 x3000 x3500) (nd_OP__case_8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_not (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x6 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1002 x3500) (d_OP__case_10 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_not) (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x6 x2001 x3500)))) x2003 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x1002 x3000 x3500) (nd_OP__case_10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x44 x3500 = case x44 of
     (Curry_Prelude.OP_Tuple2 x45 x46) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x45 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x46 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1002 x3500) (d_OP__case_11 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x44 x3000 x3500 = case x44 of
     (Curry_Prelude.OP_Tuple2 x45 x46) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x45 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x46 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1002 x3000 x3500) (nd_OP__case_11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x3 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x4 x5) -> d_OP__case_13 x3 x4 x3500
     (Curry_AbstractCurry.C_CVar x27) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLit x28) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CSymbol x29) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLambda x30 x31) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLetDecl x32 x33) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CDoExpr x34) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CListComp x35 x36) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CCase x37 x38) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecConstr x39) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecSelect x40 x41) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecUpdate x42 x43) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x3 x1002 x3500) (d_OP__case_14 x3 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x3 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x3 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x3 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CApply x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_13 x3 x4 x2000 x3500))
     (Curry_AbstractCurry.C_CVar x27) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLit x28) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CSymbol x29) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLambda x30 x31) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLetDecl x32 x33) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CDoExpr x34) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CListComp x35 x36) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CCase x37 x38) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecConstr x39) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecSelect x40 x41) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecUpdate x42 x43) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x3 x1002 x3000 x3500) (nd_OP__case_14 x3 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x3 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x3 x4 x3500 = case x4 of
     (Curry_AbstractCurry.C_CSymbol x6) -> d_OP__case_12 x3 x6 x3500
     (Curry_AbstractCurry.C_CVar x9) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLit x10) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CApply x11 x12) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLambda x13 x14) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLetDecl x15 x16) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CDoExpr x17) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CListComp x18 x19) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CCase x20 x21) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecConstr x22) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecSelect x23 x24) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecUpdate x25 x26) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x3 x1002 x3500) (d_OP__case_13 x3 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x3 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x3 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x3 x4 x3000 x3500 = case x4 of
     (Curry_AbstractCurry.C_CSymbol x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x3 x6 x2000 x3500))
     (Curry_AbstractCurry.C_CVar x9) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLit x10) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CApply x11 x12) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLambda x13 x14) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLetDecl x15 x16) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CDoExpr x17) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CListComp x18 x19) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CCase x20 x21) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecConstr x22) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecSelect x23 x24) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecUpdate x25 x26) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x3 x1002 x3000 x3500) (nd_OP__case_13 x3 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x3 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x3 x6 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x7 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x3500) (d_C_isClosedList x3 x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x3 x1002 x3500) (d_OP__case_12 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x3 x6 x3000 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x7 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x3500) (d_C_isClosedList x3 x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x3 x1002 x3000 x3500) (nd_OP__case_12 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x7 x6 x3500 = case x6 of
     (Curry_AbstractCurry.C_CApply x8 x9) -> d_OP__case_16 x7 x9 x3500
     (Curry_AbstractCurry.C_CVar x32) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLit x33) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CSymbol x34) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLambda x35 x36) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLetDecl x37 x38) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CDoExpr x39) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CListComp x40 x41) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CCase x42 x43) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecConstr x44) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecSelect x45 x46) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecUpdate x47 x48) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x7 x1002 x3500) (d_OP__case_17 x7 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x7 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x7 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x7 x6 x3000 x3500 = case x6 of
     (Curry_AbstractCurry.C_CApply x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x7 x9 x2000 x3500))
     (Curry_AbstractCurry.C_CVar x32) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLit x33) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CSymbol x34) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLambda x35 x36) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLetDecl x37 x38) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CDoExpr x39) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CListComp x40 x41) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CCase x42 x43) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecConstr x44) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecSelect x45 x46) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecUpdate x47 x48) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x7 x1002 x3000 x3500) (nd_OP__case_17 x7 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x7 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x7 x9 x3500 = case x9 of
     (Curry_AbstractCurry.C_CLit x10) -> d_OP__case_15 x7 x10 x3500
     (Curry_AbstractCurry.C_CVar x14) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CSymbol x15) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CApply x16 x17) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLambda x18 x19) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLetDecl x20 x21) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CDoExpr x22) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CListComp x23 x24) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CCase x25 x26) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecConstr x27) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecSelect x28 x29) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecUpdate x30 x31) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x7 x1002 x3500) (d_OP__case_16 x7 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x7 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x7 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x7 x9 x3000 x3500 = case x9 of
     (Curry_AbstractCurry.C_CLit x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_15 x7 x10 x2000 x3500))
     (Curry_AbstractCurry.C_CVar x14) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CSymbol x15) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CApply x16 x17) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLambda x18 x19) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CLetDecl x20 x21) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CDoExpr x22) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CListComp x23 x24) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CCase x25 x26) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecConstr x27) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecSelect x28 x29) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecUpdate x30 x31) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x7 x1002 x3000 x3500) (nd_OP__case_16 x7 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x7 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x7 x10 x3500 = case x10 of
     (Curry_AbstractCurry.C_CCharc x11) -> d_C_isStringList x7 x3500
     (Curry_AbstractCurry.C_CIntc x12) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CFloatc x13) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CLiteral x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x7 x1002 x3500) (d_OP__case_15 x7 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CLiteral x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x7 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CLiteral x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x7 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CLiteral x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x7 x10 x3000 x3500 = case x10 of
     (Curry_AbstractCurry.C_CCharc x11) -> d_C_isStringList x7 x3500
     (Curry_AbstractCurry.C_CIntc x12) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CFloatc x13) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CLiteral x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x7 x1002 x3000 x3500) (nd_OP__case_15 x7 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CLiteral x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x7 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CLiteral x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CLiteral x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x3 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x1002 x3500) (d_OP__case_18 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x3 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x1002 x3000 x3500) (nd_OP__case_18 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_20 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar d_C_dropTags (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_tail (Curry_Prelude.d_C_dropWhile (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Prelude.C_Char '"'#)) x3 x3500) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_19 x3 x4 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Char '>'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x3 x4 x1002 x3500) (d_OP__case_20 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_dropTags) (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_tail) (Curry_Prelude.nd_C_dropWhile (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_slash_eq)) (Curry_Prelude.C_Char '"'#))) x3 x2000 x3500) x2001 x3500)))) x2003 x3500)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_19 x3 x4 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Char '>'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x3 x4 x1002 x3000 x3500) (nd_OP__case_20 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> d_C_dropTags x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x3 x4 x1002 x3500) (d_OP__case_19 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> d_C_dropTags x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x3 x4 x1002 x3000 x3500) (nd_OP__case_19 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_22 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> d_C_showExprOpt x1 x2 x3500
     Curry_Prelude.C_False -> d_OP__case_21 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1 x2 x1002 x3500) (d_OP__case_22 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showExprOpt x1 x2 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_21 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x1 x2 x1002 x3000 x3500) (nd_OP__case_22 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar d_C_parens (d_C_showExprOpt x1 x2 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x1 x2 x1002 x3500) (d_OP__case_21 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_parens) (nd_C_showExprOpt x1 x2 x2000 x3500) x2001 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x1 x2 x1002 x3000 x3500) (nd_OP__case_21 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_23 x1 x4 x3 x3500 = case x3 of
     (Curry_AbstractCurry.C_CSymbol x5) -> d_C_showExprOpt x1 x4 x3500
     (Curry_AbstractCurry.C_CApply x6 x7) -> Curry_Prelude.d_OP_plus_plus (d_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 (Curry_AbstractCurry.C_CApply x6 x7) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showExprOpt x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x1 x4 x1002 x3500) (d_OP__case_23 x1 x4 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x1 x4 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x1 x4 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_23 x1 x4 x3 x3000 x3500 = case x3 of
     (Curry_AbstractCurry.C_CSymbol x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showExprOpt x1 x4 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x6 x7) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_OP_showTupleApplication_dot_p_showTuple_dot_401 x1 (Curry_AbstractCurry.C_CApply x6 x7) x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (nd_C_showExprOpt x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x1 x4 x1002 x3000 x3500) (nd_OP__case_23 x1 x4 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x1 x4 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_26 x1 x4 x3 x3500 = case x3 of
     (Curry_AbstractCurry.C_CApply x5 x6) -> d_OP__case_25 x1 x3 x4 x6 x5 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x1 x4 x1002 x3500) (d_OP__case_26 x1 x4 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x1 x4 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x1 x4 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_26 x1 x4 x3 x3000 x3500 = case x3 of
     (Curry_AbstractCurry.C_CApply x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_25 x1 x3 x4 x6 x5 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x1 x4 x1002 x3000 x3500) (nd_OP__case_26 x1 x4 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 x1 x4 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_25 x1 x3 x4 x6 x5 x3500 = case x5 of
     (Curry_AbstractCurry.C_CApply x7 x8) -> d_OP__case_24 x1 x3 x4 x6 x8 x7 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x1 x3 x4 x6 x1002 x3500) (d_OP__case_25 x1 x3 x4 x6 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x1 x3 x4 x6 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x1 x3 x4 x6 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_25 x1 x3 x4 x6 x5 x3000 x3500 = case x5 of
     (Curry_AbstractCurry.C_CApply x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_24 x1 x3 x4 x6 x8 x7 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x1 x3 x4 x6 x1002 x3000 x3500) (nd_OP__case_25 x1 x3 x4 x6 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 x1 x3 x4 x6 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x1 x3 x4 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_24 x1 x3 x4 x6 x8 x7 x3500 = case x7 of
     (Curry_AbstractCurry.C_CSymbol x9) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (d_C_showExprOpt x1 x8 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (d_C_showExprOpt x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (d_C_showExprOpt x1 x4 x3500) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CApply x10 x11) -> Curry_Prelude.d_OP_plus_plus (d_C_parens (d_C_showITEApplication x1 x3 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (d_C_showBoxedExpr x1 x4 x3500)) x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x1 x3 x4 x6 x8 x1002 x3500) (d_OP__case_24 x1 x3 x4 x6 x8 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x1 x3 x4 x6 x8 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x1 x3 x4 x6 x8 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_24 x1 x3 x4 x6 x8 x7 x3000 x3500 = case x7 of
     (Curry_AbstractCurry.C_CSymbol x9) -> let
          x2004 = x3000
           in (seq x2004 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_showExprOpt x1 x8 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_showExprOpt x1 x6 x2001 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (nd_C_showExprOpt x1 x4 x2002 x3500) x3500) x3500)))) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.C_CApply x10 x11) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (d_C_parens (nd_C_showITEApplication x1 x3 x2000 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (nd_C_showBoxedExpr x1 x4 x2001 x3500)) x3500)))))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x1 x3 x4 x6 x8 x1002 x3000 x3500) (nd_OP__case_24 x1 x3 x4 x6 x8 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x1 x3 x4 x6 x8 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x1 x3 x4 x6 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_28 x1 x2 x5 x4 x3500 = case x4 of
     (Curry_AbstractCurry.C_CApply x6 x7) -> d_OP__case_27 x1 x2 x5 x7 x6 x3500
     (Curry_AbstractCurry.C_CVar x27) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x28) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CSymbol x29) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLambda x30 x31) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLetDecl x32 x33) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CDoExpr x34) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CListComp x35 x36) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CCase x37 x38) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CRecConstr x39) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CRecSelect x40 x41) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CRecUpdate x42 x43) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x1 x2 x5 x1002 x3500) (d_OP__case_28 x1 x2 x5 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x1 x2 x5 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x1 x2 x5 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_28 x1 x2 x5 x4 x3000 x3500 = case x4 of
     (Curry_AbstractCurry.C_CApply x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_27 x1 x2 x5 x7 x6 x2000 x3500))
     (Curry_AbstractCurry.C_CVar x27) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (nd_C_showBoxedExpr x1 x5 x2001 x3500) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.C_CLit x28) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (nd_C_showBoxedExpr x1 x5 x2001 x3500) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.C_CSymbol x29) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (nd_C_showBoxedExpr x1 x5 x2001 x3500) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.C_CLambda x30 x31) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (nd_C_showBoxedExpr x1 x5 x2001 x3500) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.C_CLetDecl x32 x33) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (nd_C_showBoxedExpr x1 x5 x2001 x3500) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.C_CDoExpr x34) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (nd_C_showBoxedExpr x1 x5 x2001 x3500) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.C_CListComp x35 x36) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (nd_C_showBoxedExpr x1 x5 x2001 x3500) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.C_CCase x37 x38) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (nd_C_showBoxedExpr x1 x5 x2001 x3500) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.C_CRecConstr x39) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (nd_C_showBoxedExpr x1 x5 x2001 x3500) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.C_CRecSelect x40 x41) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (nd_C_showBoxedExpr x1 x5 x2001 x3500) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.C_CRecUpdate x42 x43) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (nd_C_showBoxedExpr x1 x5 x2001 x3500) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_28 x1 x2 x5 x1002 x3000 x3500) (nd_OP__case_28 x1 x2 x5 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_28 x1 x2 x5 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_28 x1 x2 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_27 x1 x2 x5 x7 x6 x3500 = case x6 of
     (Curry_AbstractCurry.C_CApply x8 x9) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x9 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CVar x10) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x11) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CSymbol x12) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLambda x13 x14) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLetDecl x15 x16) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CDoExpr x17) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CListComp x18 x19) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CCase x20 x21) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CRecConstr x22) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CRecSelect x23 x24) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CRecUpdate x25 x26) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x1 x2 x5 x7 x1002 x3500) (d_OP__case_27 x1 x2 x5 x7 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x1 x2 x5 x7 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x1 x2 x5 x7 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_27 x1 x2 x5 x7 x6 x3000 x3500 = case x6 of
     (Curry_AbstractCurry.C_CApply x8 x9) -> let
          x2006 = x3000
           in (seq x2006 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (let
               x2000 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2000 (seq x2005 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x9 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                    x2001 = leftSupply x2005
                    x2004 = rightSupply x2005
                     in (seq x2001 (seq x2004 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2001 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                         x2002 = leftSupply x2004
                         x2003 = rightSupply x2004
                          in (seq x2002 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x7 x2002 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (nd_C_showBoxedExpr x1 x5 x2003 x3500) x3500) x3500)))) x3500) x3500)))) x3500) x3500)))) x3500))
     (Curry_AbstractCurry.C_CVar x10) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x7 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2001 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x5 x2002 x3500) x3500) x3500)))) x3500) x3500)))))
     (Curry_AbstractCurry.C_CLit x11) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x7 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2001 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x5 x2002 x3500) x3500) x3500)))) x3500) x3500)))))
     (Curry_AbstractCurry.C_CSymbol x12) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x7 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2001 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x5 x2002 x3500) x3500) x3500)))) x3500) x3500)))))
     (Curry_AbstractCurry.C_CLambda x13 x14) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x7 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2001 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x5 x2002 x3500) x3500) x3500)))) x3500) x3500)))))
     (Curry_AbstractCurry.C_CLetDecl x15 x16) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x7 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2001 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x5 x2002 x3500) x3500) x3500)))) x3500) x3500)))))
     (Curry_AbstractCurry.C_CDoExpr x17) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x7 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2001 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x5 x2002 x3500) x3500) x3500)))) x3500) x3500)))))
     (Curry_AbstractCurry.C_CListComp x18 x19) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x7 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2001 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x5 x2002 x3500) x3500) x3500)))) x3500) x3500)))))
     (Curry_AbstractCurry.C_CCase x20 x21) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x7 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2001 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x5 x2002 x3500) x3500) x3500)))) x3500) x3500)))))
     (Curry_AbstractCurry.C_CRecConstr x22) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x7 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2001 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x5 x2002 x3500) x3500) x3500)))) x3500) x3500)))))
     (Curry_AbstractCurry.C_CRecSelect x23 x24) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x7 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2001 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x5 x2002 x3500) x3500) x3500)))) x3500) x3500)))))
     (Curry_AbstractCurry.C_CRecUpdate x25 x26) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x7 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_showSymbol x1 x2 x2001 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x5 x2002 x3500) x3500) x3500)))) x3500) x3500)))))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x1 x2 x5 x7 x1002 x3000 x3500) (nd_OP__case_27 x1 x2 x5 x7 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 x1 x2 x5 x7 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x1 x2 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_31 x1 x4 x3 x3500 = case x3 of
     (Curry_AbstractCurry.C_CApply x5 x6) -> d_OP__case_30 x1 x6 x4 x3500
     (Curry_AbstractCurry.C_CSymbol x26) -> d_OP__case_29 x1 x4 x26 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x1 x4 x1002 x3500) (d_OP__case_31 x1 x4 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x1 x4 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x1 x4 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_31 x1 x4 x3 x3000 x3500 = case x3 of
     (Curry_AbstractCurry.C_CApply x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_30 x1 x6 x4 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x26) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_29 x1 x4 x26 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_31 x1 x4 x1002 x3000 x3500) (nd_OP__case_31 x1 x4 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_31 x1 x4 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_31 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_29 x1 x4 x26 x3500 = case x26 of
     (Curry_Prelude.OP_Tuple2 x27 x28) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x4 x3500) x28 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x1 x4 x1002 x3500) (d_OP__case_29 x1 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x1 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x1 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_29 x1 x4 x26 x3000 x3500 = case x26 of
     (Curry_Prelude.OP_Tuple2 x27 x28) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x4 x2000 x3500) x28 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_29 x1 x4 x1002 x3000 x3500) (nd_OP__case_29 x1 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_29 x1 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_29 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_30 x1 x6 x4 x3500 = case x4 of
     (Curry_AbstractCurry.C_CSymbol x7) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List))) x3500
     (Curry_AbstractCurry.C_CVar x8) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x9) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CApply x10 x11) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLambda x12 x13) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLetDecl x14 x15) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CDoExpr x16) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CListComp x17 x18) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CCase x19 x20) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CRecConstr x21) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CRecSelect x22 x23) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CRecUpdate x24 x25) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x1 x6 x1002 x3500) (d_OP__case_30 x1 x6 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x1 x6 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x1 x6 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_30 x1 x6 x4 x3000 x3500 = case x4 of
     (Curry_AbstractCurry.C_CSymbol x7) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List))) x3500))
     (Curry_AbstractCurry.C_CVar x8) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CLit x9) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CApply x10 x11) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CLambda x12 x13) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CLetDecl x14 x15) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CDoExpr x16) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CListComp x17 x18) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CCase x19 x20) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CRecConstr x21) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CRecSelect x22 x23) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CRecUpdate x24 x25) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_30 x1 x6 x1002 x3000 x3500) (nd_OP__case_30 x1 x6 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_30 x1 x6 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_30 x1 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_33 x1 x4 x3 x3500 = case x3 of
     (Curry_AbstractCurry.C_CApply x5 x6) -> d_OP__case_32 x1 x6 x4 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x1 x4 x1002 x3500) (d_OP__case_33 x1 x4 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x1 x4 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x1 x4 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_33 x1 x4 x3 x3000 x3500 = case x3 of
     (Curry_AbstractCurry.C_CApply x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_32 x1 x6 x4 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_33 x1 x4 x1002 x3000 x3500) (nd_OP__case_33 x1 x4 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_33 x1 x4 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_33 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_32 x1 x6 x4 x3500 = case x4 of
     (Curry_AbstractCurry.C_CSymbol x7) -> d_C_showBoxedExpr x1 x6 x3500
     (Curry_AbstractCurry.C_CVar x8) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x9) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CApply x10 x11) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLambda x12 x13) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLetDecl x14 x15) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CDoExpr x16) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CListComp x17 x18) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CCase x19 x20) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CRecConstr x21) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CRecSelect x22 x23) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.C_CRecUpdate x24 x25) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3500) x3500) x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x1 x6 x1002 x3500) (d_OP__case_32 x1 x6 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 x1 x6 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x1 x6 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_32 x1 x6 x4 x3000 x3500 = case x4 of
     (Curry_AbstractCurry.C_CSymbol x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showBoxedExpr x1 x6 x2000 x3500))
     (Curry_AbstractCurry.C_CVar x8) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (nd_C_showConsListApplication x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CLit x9) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (nd_C_showConsListApplication x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CApply x10 x11) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (nd_C_showConsListApplication x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CLambda x12 x13) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (nd_C_showConsListApplication x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CLetDecl x14 x15) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (nd_C_showConsListApplication x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CDoExpr x16) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (nd_C_showConsListApplication x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CListComp x17 x18) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (nd_C_showConsListApplication x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CCase x19 x20) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (nd_C_showConsListApplication x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CRecConstr x21) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (nd_C_showConsListApplication x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CRecSelect x22 x23) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (nd_C_showConsListApplication x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.C_CRecUpdate x24 x25) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x6 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (nd_C_showConsListApplication x1 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_32 x1 x6 x1002 x3000 x3500) (nd_OP__case_32 x1 x6 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_32 x1 x6 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_32 x1 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_36 x1 x4 x3 x3500 = case x3 of
     (Curry_AbstractCurry.C_CApply x5 x6) -> d_OP__case_35 x1 x4 x6 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x1 x4 x1002 x3500) (d_OP__case_36 x1 x4 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x1 x4 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x1 x4 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_36 x1 x4 x3 x3000 x3500 = case x3 of
     (Curry_AbstractCurry.C_CApply x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_35 x1 x4 x6 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_36 x1 x4 x1002 x3000 x3500) (nd_OP__case_36 x1 x4 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_36 x1 x4 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_36 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_35 x1 x4 x6 x3500 = case x6 of
     (Curry_AbstractCurry.C_CLit x7) -> d_OP__case_34 x1 x7 x4 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x1 x4 x1002 x3500) (d_OP__case_35 x1 x4 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x1 x4 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x1 x4 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_35 x1 x4 x6 x3000 x3500 = case x6 of
     (Curry_AbstractCurry.C_CLit x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_34 x1 x7 x4 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_35 x1 x4 x1002 x3000 x3500) (nd_OP__case_35 x1 x4 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_35 x1 x4 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_35 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_34 x1 x7 x4 x3500 = case x4 of
     (Curry_AbstractCurry.C_CSymbol x8) -> d_C_showCCharc x7 x3500
     (Curry_AbstractCurry.C_CVar x9) -> Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (d_C_showCharListApplication x1 x4 x3500) x3500
     (Curry_AbstractCurry.C_CLit x10) -> Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (d_C_showCharListApplication x1 x4 x3500) x3500
     (Curry_AbstractCurry.C_CApply x11 x12) -> Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (d_C_showCharListApplication x1 x4 x3500) x3500
     (Curry_AbstractCurry.C_CLambda x13 x14) -> Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (d_C_showCharListApplication x1 x4 x3500) x3500
     (Curry_AbstractCurry.C_CLetDecl x15 x16) -> Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (d_C_showCharListApplication x1 x4 x3500) x3500
     (Curry_AbstractCurry.C_CDoExpr x17) -> Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (d_C_showCharListApplication x1 x4 x3500) x3500
     (Curry_AbstractCurry.C_CListComp x18 x19) -> Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (d_C_showCharListApplication x1 x4 x3500) x3500
     (Curry_AbstractCurry.C_CCase x20 x21) -> Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (d_C_showCharListApplication x1 x4 x3500) x3500
     (Curry_AbstractCurry.C_CRecConstr x22) -> Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (d_C_showCharListApplication x1 x4 x3500) x3500
     (Curry_AbstractCurry.C_CRecSelect x23 x24) -> Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (d_C_showCharListApplication x1 x4 x3500) x3500
     (Curry_AbstractCurry.C_CRecUpdate x25 x26) -> Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (d_C_showCharListApplication x1 x4 x3500) x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x1 x7 x1002 x3500) (d_OP__case_34 x1 x7 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x1 x7 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x1 x7 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_34 x1 x7 x4 x3000 x3500 = case x4 of
     (Curry_AbstractCurry.C_CSymbol x8) -> d_C_showCCharc x7 x3500
     (Curry_AbstractCurry.C_CVar x9) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (nd_C_showCharListApplication x1 x4 x2000 x3500) x3500))
     (Curry_AbstractCurry.C_CLit x10) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (nd_C_showCharListApplication x1 x4 x2000 x3500) x3500))
     (Curry_AbstractCurry.C_CApply x11 x12) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (nd_C_showCharListApplication x1 x4 x2000 x3500) x3500))
     (Curry_AbstractCurry.C_CLambda x13 x14) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (nd_C_showCharListApplication x1 x4 x2000 x3500) x3500))
     (Curry_AbstractCurry.C_CLetDecl x15 x16) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (nd_C_showCharListApplication x1 x4 x2000 x3500) x3500))
     (Curry_AbstractCurry.C_CDoExpr x17) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (nd_C_showCharListApplication x1 x4 x2000 x3500) x3500))
     (Curry_AbstractCurry.C_CListComp x18 x19) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (nd_C_showCharListApplication x1 x4 x2000 x3500) x3500))
     (Curry_AbstractCurry.C_CCase x20 x21) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (nd_C_showCharListApplication x1 x4 x2000 x3500) x3500))
     (Curry_AbstractCurry.C_CRecConstr x22) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (nd_C_showCharListApplication x1 x4 x2000 x3500) x3500))
     (Curry_AbstractCurry.C_CRecSelect x23 x24) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (nd_C_showCharListApplication x1 x4 x2000 x3500) x3500))
     (Curry_AbstractCurry.C_CRecUpdate x25 x26) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_showCCharc x7 x3500) (nd_C_showCharListApplication x1 x4 x2000 x3500) x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_34 x1 x7 x1002 x3000 x3500) (nd_OP__case_34 x1 x7 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_34 x1 x7 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_34 x1 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_39 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showCharListApplication x1 x2 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_38 x1 x2 (d_C_isClosedList x2 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x1 x2 x1002 x3500) (d_OP__case_39 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_39 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (nd_C_showCharListApplication x1 x2 x2000 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_38 x1 x2 (d_C_isClosedList x2 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_39 x1 x2 x1002 x3000 x3500) (nd_OP__case_39 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_39 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_39 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_38 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar d_C_brackets (d_C_showConsListApplication x1 x2 x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_37 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x1 x2 x1002 x3500) (d_OP__case_38 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_38 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_brackets) (nd_C_showConsListApplication x1 x2 x2000 x3500) x2001 x3500)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_37 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_38 x1 x2 x1002 x3000 x3500) (nd_OP__case_38 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_38 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_38 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_37 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar d_C_parens (d_C_showSimpleListApplication x1 x2 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x1 x2 x1002 x3500) (d_OP__case_37 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_37 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_parens) (nd_C_showSimpleListApplication x1 x2 x2000 x3500) x2001 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_37 x1 x2 x1002 x3000 x3500) (nd_OP__case_37 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_37 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_37 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_44 x1 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_showListApplication x1 x3 x3500
     Curry_Prelude.C_False -> d_OP__case_43 x1 x3 x4 x5 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x5 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x1 x3 x4 x5 x1002 x3500) (d_OP__case_44 x1 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 x1 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x1 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_44 x1 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showListApplication x1 x3 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_43 x1 x3 x4 x5 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x5 x2001 x3500)))) x2003 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_44 x1 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_44 x1 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_44 x1 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_44 x1 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_43 x1 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_showInfixApplication x1 (Curry_Prelude.OP_Tuple2 x4 x5) x3 x3500
     Curry_Prelude.C_False -> d_OP__case_42 x1 x3 x4 x5 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x1 x3 x4 x5 x1002 x3500) (d_OP__case_43 x1 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x1 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x1 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_43 x1 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showInfixApplication x1 (Curry_Prelude.OP_Tuple2 x4 x5) x3 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_42 x1 x3 x4 x5 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_43 x1 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_43 x1 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_43 x1 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_43 x1 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_42 x1 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_showITEApplication x1 x3 x3500
     Curry_Prelude.C_False -> d_OP__case_41 x1 x3 x5 (d_C_isTuple x5 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x1 x3 x4 x5 x1002 x3500) (d_OP__case_42 x1 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x1 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x1 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_42 x1 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showITEApplication x1 x3 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_41 x1 x3 x5 (d_C_isTuple x5 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_42 x1 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_42 x1 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_42 x1 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_42 x1 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_41 x1 x3 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_showTupleApplication x1 x3 x3500
     Curry_Prelude.C_False -> d_OP__case_40 x1 x3 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x1 x3 x5 x1002 x3500) (d_OP__case_41 x1 x3 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x1 x3 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x1 x3 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_41 x1 x3 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showTupleApplication x1 x3 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_40 x1 x3 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_41 x1 x3 x5 x1002 x3000 x3500) (nd_OP__case_41 x1 x3 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_41 x1 x3 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_41 x1 x3 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_40 x1 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_showSimpleApplication x1 x3 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x1 x3 x1002 x3500) (d_OP__case_40 x1 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x1 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x1 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_40 x1 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSimpleApplication x1 x3 x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_40 x1 x3 x1002 x3000 x3500) (nd_OP__case_40 x1 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_40 x1 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_40 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_45 x1 x2 x22 x3500 = case x22 of
     (Curry_AbstractCurry.C_CSymbol x3) -> d_C_showSymbolApplication x1 x3 x2 x3500
     (Curry_AbstractCurry.C_CVar x4) -> d_C_showSimpleApplication x1 x2 x3500
     (Curry_AbstractCurry.C_CLit x5) -> d_C_showSimpleApplication x1 x2 x3500
     (Curry_AbstractCurry.C_CApply x6 x7) -> d_C_showSimpleApplication x1 x2 x3500
     (Curry_AbstractCurry.C_CLambda x8 x9) -> d_C_showSimpleApplication x1 x2 x3500
     (Curry_AbstractCurry.C_CLetDecl x10 x11) -> d_C_showSimpleApplication x1 x2 x3500
     (Curry_AbstractCurry.C_CDoExpr x12) -> d_C_showSimpleApplication x1 x2 x3500
     (Curry_AbstractCurry.C_CListComp x13 x14) -> d_C_showSimpleApplication x1 x2 x3500
     (Curry_AbstractCurry.C_CCase x15 x16) -> d_C_showSimpleApplication x1 x2 x3500
     (Curry_AbstractCurry.C_CRecConstr x17) -> d_C_showSimpleApplication x1 x2 x3500
     (Curry_AbstractCurry.C_CRecSelect x18 x19) -> d_C_showSimpleApplication x1 x2 x3500
     (Curry_AbstractCurry.C_CRecUpdate x20 x21) -> d_C_showSimpleApplication x1 x2 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x1 x2 x1002 x3500) (d_OP__case_45 x1 x2 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 x1 x2 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_45 x1 x2 x22 x3000 x3500 = case x22 of
     (Curry_AbstractCurry.C_CSymbol x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSymbolApplication x1 x3 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CVar x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSimpleApplication x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CLit x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSimpleApplication x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSimpleApplication x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSimpleApplication x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSimpleApplication x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x12) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSimpleApplication x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x13 x14) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSimpleApplication x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x15 x16) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSimpleApplication x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x17) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSimpleApplication x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x18 x19) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSimpleApplication x1 x2 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x20 x21) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSimpleApplication x1 x2 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_45 x1 x2 x1002 x3000 x3500) (nd_OP__case_45 x1 x2 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_45 x1 x2 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_45 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_49 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List)))))
     Curry_Prelude.C_False -> d_OP__case_48 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_49 x1 x2 x1002 x3500) (d_OP__case_49 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_49 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_49 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_49 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_48 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_49 x1 x2 x1002 x3000 x3500) (nd_OP__case_49 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_49 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_49 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_48 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExpr Curry_Prelude.C_False (Curry_Prelude.d_C_head x2 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_47 x1 x2 (d_C_isTuple x1 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_48 x1 x2 x1002 x3500) (d_OP__case_48 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_48 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_48 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_48 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExpr Curry_Prelude.C_False (Curry_Prelude.d_C_head x2 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_47 x1 x2 (d_C_isTuple x1 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_48 x1 x2 x1002 x3000 x3500) (nd_OP__case_48 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_48 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_48 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_47 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_combineMap (d_C_showTypeExpr Curry_Prelude.C_False) x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_46 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_47 x1 x2 x1002 x3500) (d_OP__case_47 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_47 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_47 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_47 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (nd_C_combineMap (wrapDX id (d_C_showTypeExpr Curry_Prelude.C_False)) x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) x2000 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_46 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_47 x1 x2 x1002 x3000 x3500) (nd_OP__case_47 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_47 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_47 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_46 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x1 (d_C_prefixMap (d_C_showTypeExpr Curry_Prelude.C_True) x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_46 x1 x2 x1002 x3500) (d_OP__case_46 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_46 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_46 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_46 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus x1 (nd_C_prefixMap (wrapDX id (d_C_showTypeExpr Curry_Prelude.C_True)) x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x2000 x3500) x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_46 x1 x2 x1002 x3000 x3500) (nd_OP__case_46 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_46 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_46 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_51 x1 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_showPreludeTypeCons x2 (Curry_Prelude.OP_Cons x4 x5) x3500
     Curry_Prelude.C_False -> d_OP__case_50 x2 x4 x5 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_51 x1 x2 x4 x5 x1002 x3500) (d_OP__case_51 x1 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_51 x1 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_51 x1 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_51 x1 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_showPreludeTypeCons x2 (Curry_Prelude.OP_Cons x4 x5) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_50 x2 x4 x5 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_51 x1 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_51 x1 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_51 x1 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_51 x1 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_50 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x2 (d_C_prefixMap (d_C_showTypeExpr Curry_Prelude.C_True) (Curry_Prelude.OP_Cons x4 x5) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_50 x2 x4 x5 x1002 x3500) (d_OP__case_50 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_50 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_50 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_50 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus x2 (nd_C_prefixMap (wrapDX id (d_C_showTypeExpr Curry_Prelude.C_True)) (Curry_Prelude.OP_Cons x4 x5) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x2000 x3500) x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_50 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_50 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_50 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_50 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_56 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> d_OP__case_55 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\r'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_56 x2 x1002 x3500) (d_OP__case_56 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_56 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_56 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_56 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_55 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\r'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_56 x2 x1002 x3000 x3500) (nd_OP__case_56 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_56 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_56 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_55 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> d_OP__case_54 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\\'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_55 x2 x1002 x3500) (d_OP__case_55 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_55 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_55 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_55 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_54 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\\'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_55 x2 x1002 x3000 x3500) (nd_OP__case_55 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_55 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_55 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_54 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> d_OP__case_53 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '"'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_54 x2 x1002 x3500) (d_OP__case_54 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_54 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_54 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_54 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_53 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '"'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_54 x2 x1002 x3000 x3500) (nd_OP__case_54 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_54 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_54 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_53 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> d_OP__case_52 x2 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_53 x2 x1002 x3500) (d_OP__case_53 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_53 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_53 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_53 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_52 x2 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_53 x2 x1002 x3000 x3500) (nd_OP__case_53 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_53 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_53 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_52 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_52 x2 x1002 x3500) (d_OP__case_52 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_52 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_52 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_52 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_52 x2 x1002 x3000 x3500) (nd_OP__case_52 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_52 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_52 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_57 x3 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) Curry_Prelude.OP_List) (d_C_parens (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showPatListElems x3 x3500) x3500) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_57 x3 x1002 x3500) (d_OP__case_57 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_57 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_57 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_57 x3 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) Curry_Prelude.OP_List) (d_C_parens (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showPatListElems x3 x3500) x3500) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_57 x3 x1002 x3000 x3500) (nd_OP__case_57 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_57 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_57 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_58 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CCharc x3) -> Curry_Prelude.C_True
     (Curry_AbstractCurry.C_CIntc x4) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CFloatc x5) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CLiteral x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_58 x1002 x3500) (d_OP__case_58 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CLiteral x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_58 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CLiteral x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_58 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CLiteral x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_58 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CCharc x3) -> Curry_Prelude.C_True
     (Curry_AbstractCurry.C_CIntc x4) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CFloatc x5) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CLiteral x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_58 x1002 x3000 x3500) (nd_OP__case_58 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CLiteral x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_58 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CLiteral x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_58 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CLiteral x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_69 x3 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_68 x3 x4 x5 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_69 x3 x1002 x3500) (d_OP__case_69 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_69 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_69 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_69 x3 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_68 x3 x4 x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_69 x3 x1002 x3000 x3500) (nd_OP__case_69 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_69 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_69 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_68 x3 x4 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_67 x3 x4 x7 x6 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_68 x3 x4 x1002 x3500) (d_OP__case_68 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_68 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_68 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_68 x3 x4 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_67 x3 x4 x7 x6 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_68 x3 x4 x1002 x3000 x3500) (nd_OP__case_68 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_68 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_68 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_67 x3 x4 x7 x6 x3500 = case x6 of
     (Curry_Prelude.C_Char ':'#) -> d_OP__case_66 x3 x4 x7 x3500
     (Curry_Prelude.C_Char '['#) -> d_OP__case_62 x3 x4 x7 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [(':',d_OP__case_66 x3 x4 x7 x3500),('[',d_OP__case_62 x3 x4 x7 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_67 x3 x4 x7 x1002 x3500) (d_OP__case_67 x3 x4 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_67 x3 x4 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_67 x3 x4 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_67 x3 x4 x7 x6 x3000 x3500 = case x6 of
     (Curry_Prelude.C_Char ':'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_66 x3 x4 x7 x2000 x3500))
     (Curry_Prelude.C_Char '['#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_62 x3 x4 x7 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [(':',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_66 x3 x4 x7 x2000 x3500))),('[',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_62 x3 x4 x7 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_67 x3 x4 x7 x1002 x3000 x3500) (nd_OP__case_67 x3 x4 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_67 x3 x4 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_67 x3 x4 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_62 x3 x4 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x12 x13) -> d_OP__case_61 x3 x4 x13 x12 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_62 x3 x4 x1002 x3500) (d_OP__case_62 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_62 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_62 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_62 x3 x4 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_61 x3 x4 x13 x12 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_62 x3 x4 x1002 x3000 x3500) (nd_OP__case_62 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_62 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_62 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_61 x3 x4 x13 x12 x3500 = case x12 of
     (Curry_Prelude.C_Char ']'#) -> d_OP__case_60 x3 x4 x13 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [(']',d_OP__case_60 x3 x4 x13 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_61 x3 x4 x13 x1002 x3500) (d_OP__case_61 x3 x4 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_61 x3 x4 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_61 x3 x4 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_61 x3 x4 x13 x12 x3000 x3500 = case x12 of
     (Curry_Prelude.C_Char ']'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_60 x3 x4 x13 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [(']',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_60 x3 x4 x13 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_61 x3 x4 x13 x1002 x3000 x3500) (nd_OP__case_61 x3 x4 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_61 x3 x4 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_61 x3 x4 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_60 x3 x4 x13 x3500 = case x13 of
     Curry_Prelude.OP_List -> d_OP__case_59 x4 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_60 x3 x4 x1002 x3500) (d_OP__case_60 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_60 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_60 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_60 x3 x4 x13 x3000 x3500 = case x13 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_59 x4 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_60 x3 x4 x1002 x3000 x3500) (nd_OP__case_60 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_60 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_60 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_59 x4 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_59 x4 x1002 x3500) (d_OP__case_59 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_59 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_59 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_59 x4 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_59 x4 x1002 x3000 x3500) (nd_OP__case_59 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_59 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_59 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_66 x3 x4 x7 x3500 = case x7 of
     Curry_Prelude.OP_List -> d_OP__case_65 x4 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_66 x3 x4 x1002 x3500) (d_OP__case_66 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_66 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_66 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_66 x3 x4 x7 x3000 x3500 = case x7 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_65 x4 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_66 x3 x4 x1002 x3000 x3500) (nd_OP__case_66 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_66 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_66 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_65 x4 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_64 x4 x8 x9 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_65 x4 x1002 x3500) (d_OP__case_65 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_65 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_65 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_65 x4 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_64 x4 x8 x9 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_65 x4 x1002 x3000 x3500) (nd_OP__case_65 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_65 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_65 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_64 x4 x8 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_63 x4 x8 x10 x11 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_64 x4 x8 x1002 x3500) (d_OP__case_64 x4 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_64 x4 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_64 x4 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_64 x4 x8 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_63 x4 x8 x10 x11 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_64 x4 x8 x1002 x3000 x3500) (nd_OP__case_64 x4 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_64 x4 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_64 x4 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_63 x4 x8 x10 x11 x3500 = case x11 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isCharPattern x8 x3500) (d_C_isClosedStringPattern x10 x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_63 x4 x8 x10 x1002 x3500) (d_OP__case_63 x4 x8 x10 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_63 x4 x8 x10 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_63 x4 x8 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_63 x4 x8 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isCharPattern x8 x3500) (d_C_isClosedStringPattern x10 x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_63 x4 x8 x10 x1002 x3000 x3500) (nd_OP__case_63 x4 x8 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_63 x4 x8 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_63 x4 x8 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_80 x3 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_79 x3 x4 x5 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_80 x3 x1002 x3500) (d_OP__case_80 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_80 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_80 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_80 x3 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_79 x3 x4 x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_80 x3 x1002 x3000 x3500) (nd_OP__case_80 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_80 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_80 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_79 x3 x4 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_78 x3 x4 x7 x6 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_79 x3 x4 x1002 x3500) (d_OP__case_79 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_79 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_79 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_79 x3 x4 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_78 x3 x4 x7 x6 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_79 x3 x4 x1002 x3000 x3500) (nd_OP__case_79 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_79 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_79 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_78 x3 x4 x7 x6 x3500 = case x6 of
     (Curry_Prelude.C_Char ':'#) -> d_OP__case_77 x3 x4 x7 x3500
     (Curry_Prelude.C_Char '['#) -> d_OP__case_73 x3 x4 x7 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [(':',d_OP__case_77 x3 x4 x7 x3500),('[',d_OP__case_73 x3 x4 x7 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_78 x3 x4 x7 x1002 x3500) (d_OP__case_78 x3 x4 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_78 x3 x4 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_78 x3 x4 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_78 x3 x4 x7 x6 x3000 x3500 = case x6 of
     (Curry_Prelude.C_Char ':'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_77 x3 x4 x7 x2000 x3500))
     (Curry_Prelude.C_Char '['#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_73 x3 x4 x7 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [(':',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_77 x3 x4 x7 x2000 x3500))),('[',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_73 x3 x4 x7 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_78 x3 x4 x7 x1002 x3000 x3500) (nd_OP__case_78 x3 x4 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_78 x3 x4 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_78 x3 x4 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_73 x3 x4 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x12 x13) -> d_OP__case_72 x3 x4 x13 x12 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_73 x3 x4 x1002 x3500) (d_OP__case_73 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_73 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_73 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_73 x3 x4 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_72 x3 x4 x13 x12 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_73 x3 x4 x1002 x3000 x3500) (nd_OP__case_73 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_73 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_73 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_72 x3 x4 x13 x12 x3500 = case x12 of
     (Curry_Prelude.C_Char ']'#) -> d_OP__case_71 x3 x4 x13 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [(']',d_OP__case_71 x3 x4 x13 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_72 x3 x4 x13 x1002 x3500) (d_OP__case_72 x3 x4 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_72 x3 x4 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_72 x3 x4 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_72 x3 x4 x13 x12 x3000 x3500 = case x12 of
     (Curry_Prelude.C_Char ']'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_71 x3 x4 x13 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [(']',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_71 x3 x4 x13 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_72 x3 x4 x13 x1002 x3000 x3500) (nd_OP__case_72 x3 x4 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_72 x3 x4 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_72 x3 x4 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_71 x3 x4 x13 x3500 = case x13 of
     Curry_Prelude.OP_List -> d_OP__case_70 x4 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_71 x3 x4 x1002 x3500) (d_OP__case_71 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_71 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_71 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_71 x3 x4 x13 x3000 x3500 = case x13 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_70 x4 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_71 x3 x4 x1002 x3000 x3500) (nd_OP__case_71 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_71 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_71 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_70 x4 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_70 x4 x1002 x3500) (d_OP__case_70 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_70 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_70 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_70 x4 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_70 x4 x1002 x3000 x3500) (nd_OP__case_70 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_70 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_70 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_77 x3 x4 x7 x3500 = case x7 of
     Curry_Prelude.OP_List -> d_OP__case_76 x4 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_77 x3 x4 x1002 x3500) (d_OP__case_77 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_77 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_77 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_77 x3 x4 x7 x3000 x3500 = case x7 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_76 x4 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_77 x3 x4 x1002 x3000 x3500) (nd_OP__case_77 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_77 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_77 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_76 x4 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_75 x4 x9 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_76 x4 x1002 x3500) (d_OP__case_76 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_76 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_76 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_76 x4 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_75 x4 x9 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_76 x4 x1002 x3000 x3500) (nd_OP__case_76 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_76 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_76 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_75 x4 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_74 x4 x10 x11 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_75 x4 x1002 x3500) (d_OP__case_75 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_75 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_75 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_75 x4 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_74 x4 x10 x11 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_75 x4 x1002 x3000 x3500) (nd_OP__case_75 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_75 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_75 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_74 x4 x10 x11 x3500 = case x11 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500) (d_C_isClosedPatternList x10 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_74 x4 x10 x1002 x3500) (d_OP__case_74 x4 x10 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_74 x4 x10 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_74 x4 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_74 x4 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (d_C_prelude x3500) x3500) (d_C_isClosedPatternList x10 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_74 x4 x10 x1002 x3000 x3500) (nd_OP__case_74 x4 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_74 x4 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_74 x4 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_91 x3 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_90 x3 x5 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_91 x3 x1002 x3500) (d_OP__case_91 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_91 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_91 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_91 x3 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_90 x3 x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_91 x3 x1002 x3000 x3500) (nd_OP__case_91 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_91 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_91 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_90 x3 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_89 x3 x7 x6 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_90 x3 x1002 x3500) (d_OP__case_90 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_90 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_90 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_90 x3 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_89 x3 x7 x6 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_90 x3 x1002 x3000 x3500) (nd_OP__case_90 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_90 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_90 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_89 x3 x7 x6 x3500 = case x6 of
     (Curry_Prelude.C_Char ':'#) -> d_OP__case_88 x3 x7 x3500
     (Curry_Prelude.C_Char '['#) -> d_OP__case_84 x3 x7 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [(':',d_OP__case_88 x3 x7 x3500),('[',d_OP__case_84 x3 x7 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_89 x3 x7 x1002 x3500) (d_OP__case_89 x3 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_89 x3 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_89 x3 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_89 x3 x7 x6 x3000 x3500 = case x6 of
     (Curry_Prelude.C_Char ':'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_88 x3 x7 x2000 x3500))
     (Curry_Prelude.C_Char '['#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_84 x3 x7 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [(':',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_88 x3 x7 x2000 x3500))),('[',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_84 x3 x7 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_89 x3 x7 x1002 x3000 x3500) (nd_OP__case_89 x3 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_89 x3 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_89 x3 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_84 x3 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x12 x13) -> d_OP__case_83 x3 x13 x12 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_84 x3 x1002 x3500) (d_OP__case_84 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_84 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_84 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_84 x3 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_83 x3 x13 x12 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_84 x3 x1002 x3000 x3500) (nd_OP__case_84 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_84 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_84 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_83 x3 x13 x12 x3500 = case x12 of
     (Curry_Prelude.C_Char ']'#) -> d_OP__case_82 x3 x13 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [(']',d_OP__case_82 x3 x13 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_83 x3 x13 x1002 x3500) (d_OP__case_83 x3 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_83 x3 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_83 x3 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_83 x3 x13 x12 x3000 x3500 = case x12 of
     (Curry_Prelude.C_Char ']'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_82 x3 x13 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [(']',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_82 x3 x13 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_83 x3 x13 x1002 x3000 x3500) (nd_OP__case_83 x3 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_83 x3 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_83 x3 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_82 x3 x13 x3500 = case x13 of
     Curry_Prelude.OP_List -> d_OP__case_81 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_82 x3 x1002 x3500) (d_OP__case_82 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_82 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_82 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_82 x3 x13 x3000 x3500 = case x13 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_81 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_82 x3 x1002 x3000 x3500) (nd_OP__case_82 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_82 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_82 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_81 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_81 x1002 x3500) (d_OP__case_81 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_81 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_81 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_81 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_81 x1002 x3000 x3500) (nd_OP__case_81 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_81 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_81 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_88 x3 x7 x3500 = case x7 of
     Curry_Prelude.OP_List -> d_OP__case_87 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_88 x3 x1002 x3500) (d_OP__case_88 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_88 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_88 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_88 x3 x7 x3000 x3500 = case x7 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_87 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_88 x3 x1002 x3000 x3500) (nd_OP__case_88 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_88 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_88 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_87 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_86 x8 x9 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_87 x1002 x3500) (d_OP__case_87 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_87 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_87 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_87 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_86 x8 x9 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_87 x1002 x3000 x3500) (nd_OP__case_87 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_87 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_87 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_86 x8 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_85 x8 x10 x11 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_86 x8 x1002 x3500) (d_OP__case_86 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_86 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_86 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_86 x8 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_85 x8 x10 x11 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_86 x8 x1002 x3000 x3500) (nd_OP__case_86 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_86 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_86 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_85 x8 x10 x11 x3500 = case x11 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_showPattern x8 x3500) (d_C_showPatListElems x10 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_85 x8 x10 x1002 x3500) (d_OP__case_85 x8 x10 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_85 x8 x10 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_85 x8 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_85 x8 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_showPattern x8 x3500) (d_C_showPatListElems x10 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_85 x8 x10 x1002 x3000 x3500) (nd_OP__case_85 x8 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_85 x8 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_85 x8 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_95 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar d_C_doubleQuotes (Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Prelude.C_Char '\''#)) (Curry_Prelude.d_C_concat (d_C_showPatListElems x1 x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_94 x1 (d_C_isClosedPatternList x1 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_95 x1 x1002 x3500) (d_OP__case_95 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_95 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_95 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_95 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_doubleQuotes) (Curry_Prelude.nd_C_filter (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_slash_eq)) (Curry_Prelude.C_Char '\''#))) (Curry_Prelude.d_C_concat (d_C_showPatListElems x1 x3500) x3500) x2000 x3500) x2001 x3500)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_94 x1 (d_C_isClosedPatternList x1 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_95 x1 x1002 x3000 x3500) (nd_OP__case_95 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_95 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_95 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_94 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar d_C_brackets (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showPatListElems x1 x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_93 x1 (d_C_isAsPattern x1 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_94 x1 x1002 x3500) (d_OP__case_94 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_94 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_94 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_94 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_brackets) (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showPatListElems x1 x3500) x3500) x3500) x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_93 x1 (d_C_isAsPattern x1 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_94 x1 x1002 x3000 x3500) (nd_OP__case_94 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_94 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_94 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_93 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> d_C_showAsPatternList x1 x3500
     Curry_Prelude.C_False -> d_OP__case_92 x1 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_93 x1 x1002 x3500) (d_OP__case_93 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_93 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_93 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_93 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> d_C_showAsPatternList x1 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_92 x1 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_93 x1 x1002 x3000 x3500) (nd_OP__case_93 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_93 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_93 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_92 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar d_C_parens (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showPatListElems x1 x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_92 x1 x1002 x3500) (d_OP__case_92 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_92 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_92 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_92 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_parens) (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showPatListElems x1 x3500) x3500) x3500) x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_92 x1 x1002 x3000 x3500) (nd_OP__case_92 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_92 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_92 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_96 x3 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_96 x3 x1002 x3500) (d_OP__case_96 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_96 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_96 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_96 x3 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_96 x3 x1002 x3000 x3500) (nd_OP__case_96 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_96 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_96 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_97 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x5
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_97 x1002 x3500) (d_OP__case_97 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_97 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_97 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_97 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x5
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_97 x1002 x3000 x3500) (nd_OP__case_97 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_97 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_97 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_100 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_showPatternList x1 x3500
     Curry_Prelude.C_False -> d_OP__case_99 x2 x3 (d_C_isTuple x2 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_100 x1 x2 x3 x1002 x3500) (d_OP__case_100 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_100 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_100 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_100 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_showPatternList x1 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_99 x2 x3 (d_C_isTuple x2 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_100 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_100 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_100 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_100 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_99 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar d_C_parens (d_C_combineMap d_C_showPattern x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_98 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_99 x2 x3 x1002 x3500) (d_OP__case_99 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_99 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_99 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_99 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_parens) (nd_C_combineMap (wrapDX id d_C_showPattern) x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) x2000 x3500) x2001 x3500)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_98 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_99 x2 x3 x1002 x3000 x3500) (nd_OP__case_99 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_99 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_99 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_98 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar d_C_parens (Curry_Prelude.d_OP_plus_plus x2 (d_C_prefixMap d_C_showPattern x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_98 x2 x3 x1002 x3500) (d_OP__case_98 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_98 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_98 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_98 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_parens) (Curry_Prelude.d_OP_plus_plus x2 (nd_C_prefixMap (wrapDX id d_C_showPattern) x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x2000 x3500) x3500) x2001 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_98 x2 x3 x1002 x3000 x3500) (nd_OP__case_98 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_98 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_98 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_101 x13 x12 x3500 = case x12 of
     (Curry_Prelude.OP_Tuple2 x14 x15) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_showIdentifier x3500) x15 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) Curry_Prelude.OP_List) (d_C_showPattern x13 x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_101 x13 x1002 x3500) (d_OP__case_101 x13 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_101 x13 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_101 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_101 x13 x12 x3000 x3500 = case x12 of
     (Curry_Prelude.OP_Tuple2 x14 x15) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_showIdentifier x2000 x3500) x15 x2001 x3500)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) Curry_Prelude.OP_List) (d_C_showPattern x13 x3500) x3500) x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_101 x13 x1002 x3000 x3500) (nd_OP__case_101 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_101 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_101 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_106 x7 x6 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> d_OP__case_105 x8 x9 x7 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_106 x7 x1002 x3500) (d_OP__case_106 x7 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_106 x7 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_106 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_106 x7 x6 x3000 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_105 x8 x9 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_106 x7 x1002 x3000 x3500) (nd_OP__case_106 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_106 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_106 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_105 x8 x9 x7 x3500 = case x7 of
     Curry_Prelude.OP_List -> x9
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_104 x8 x9 x10 x11 (Curry_Prelude.d_OP_eq_eq x8 (d_C_prelude x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_105 x8 x9 x1002 x3500) (d_OP__case_105 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_105 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_105 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_105 x8 x9 x7 x3000 x3500 = case x7 of
     Curry_Prelude.OP_List -> x9
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_104 x8 x9 x10 x11 (Curry_Prelude.d_OP_eq_eq x8 (d_C_prelude x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_105 x8 x9 x1002 x3000 x3500) (nd_OP__case_105 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_105 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_105 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_104 x8 x9 x10 x11 x12 x3500 = case x12 of
     Curry_Prelude.C_True -> d_C_showPreludeCons (Curry_AbstractCurry.C_CPComb (Curry_Prelude.OP_Tuple2 x8 x9) (Curry_Prelude.OP_Cons x10 x11)) x3500
     Curry_Prelude.C_False -> d_OP__case_103 x9 x10 x11 (d_C_isAsPattern x10 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_104 x8 x9 x10 x11 x1002 x3500) (d_OP__case_104 x8 x9 x10 x11 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_104 x8 x9 x10 x11 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_104 x8 x9 x10 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_104 x8 x9 x10 x11 x12 x3000 x3500 = case x12 of
     Curry_Prelude.C_True -> d_C_showPreludeCons (Curry_AbstractCurry.C_CPComb (Curry_Prelude.OP_Tuple2 x8 x9) (Curry_Prelude.OP_Cons x10 x11)) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_103 x9 x10 x11 (d_C_isAsPattern x10 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_104 x8 x9 x10 x11 x1002 x3000 x3500) (nd_OP__case_104 x8 x9 x10 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_104 x8 x9 x10 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_104 x8 x9 x10 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_103 x9 x10 x11 x12 x3500 = case x12 of
     Curry_Prelude.C_True -> d_C_showAsPatternList x10 x3500
     Curry_Prelude.C_False -> d_OP__case_102 x9 x10 x11 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_103 x9 x10 x11 x1002 x3500) (d_OP__case_103 x9 x10 x11 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_103 x9 x10 x11 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_103 x9 x10 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_103 x9 x10 x11 x12 x3000 x3500 = case x12 of
     Curry_Prelude.C_True -> d_C_showAsPatternList x10 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_102 x9 x10 x11 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_103 x9 x10 x11 x1002 x3000 x3500) (nd_OP__case_103 x9 x10 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_103 x9 x10 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_103 x9 x10 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_102 x9 x10 x11 x12 x3500 = case x12 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar d_C_parens (Curry_Prelude.d_OP_plus_plus x9 (d_C_prefixMap d_C_showPattern (Curry_Prelude.OP_Cons x10 x11) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_102 x9 x10 x11 x1002 x3500) (d_OP__case_102 x9 x10 x11 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_102 x9 x10 x11 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_102 x9 x10 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_102 x9 x10 x11 x12 x3000 x3500 = case x12 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_parens) (Curry_Prelude.d_OP_plus_plus x9 (nd_C_prefixMap (wrapDX id d_C_showPattern) (Curry_Prelude.OP_Cons x10 x11) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x2000 x3500) x3500) x2001 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_102 x9 x10 x11 x1002 x3000 x3500) (nd_OP__case_102 x9 x10 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_102 x9 x10 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_102 x9 x10 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_107 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.d_C_apply (d_C_showIdentifier x3500) x4 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_107 x1002 x3500) (d_OP__case_107 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_107 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_107 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_107 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_showIdentifier x2000 x3500) x4 x2001 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_107 x1002 x3000 x3500) (nd_OP__case_107 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_107 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_107 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_109 x1 x6 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x7 x8) -> d_OP__case_108 x1 x6 x7 x8 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (d_C_showBlock (d_C_combineMap (d_C_showLocalDecl x1) x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_109 x1 x1002 x3500) (d_OP__case_109 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_109 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_109 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_109 x1 x6 x3000 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_108 x1 x6 x7 x8 x2000 x3500))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (d_C_showBlock (nd_C_combineMap (wrapNX id (nd_C_showLocalDecl x1)) x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x2000 x3500) x3500) x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_109 x1 x1002 x3000 x3500) (nd_OP__case_109 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_109 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_109 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_108 x1 x6 x7 x8 x3500 = case x8 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (d_C_showLocalDecl x1 x7 x3500) x3500
     (Curry_Prelude.OP_Cons x9 x10) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (d_C_showBlock (d_C_combineMap (d_C_showLocalDecl x1) x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_108 x1 x6 x7 x1002 x3500) (d_OP__case_108 x1 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_108 x1 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_108 x1 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_108 x1 x6 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (nd_C_showLocalDecl x1 x7 x2000 x3500) x3500))
     (Curry_Prelude.OP_Cons x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (d_C_showBlock (nd_C_combineMap (wrapNX id (nd_C_showLocalDecl x1)) x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x2000 x3500) x3500) x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_108 x1 x6 x7 x1002 x3000 x3500) (nd_OP__case_108 x1 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_108 x1 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_108 x1 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_141 x1 x2 x3 x5 x4 x3500 = case x4 of
     (Curry_AbstractCurry.C_CPVar x6) -> d_OP__case_140 x1 x2 x3 x6 x5 x3500
     (Curry_AbstractCurry.C_CPLit x296) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CPComb x297 x298) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CPAs x299 x300) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CPFuncComb x301 x302) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CPLazy x303) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CPRecord x304 x305) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_141 x1 x2 x3 x5 x1002 x3500) (d_OP__case_141 x1 x2 x3 x5 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_141 x1 x2 x3 x5 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_141 x1 x2 x3 x5 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_141 x1 x2 x3 x5 x4 x3000 x3500 = case x4 of
     (Curry_AbstractCurry.C_CPVar x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_140 x1 x2 x3 x6 x5 x2000 x3500))
     (Curry_AbstractCurry.C_CPLit x296) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CPComb x297 x298) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CPAs x299 x300) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CPFuncComb x301 x302) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CPLazy x303) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CPRecord x304 x305) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_141 x1 x2 x3 x5 x1002 x3000 x3500) (nd_OP__case_141 x1 x2 x3 x5 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_141 x1 x2 x3 x5 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_141 x1 x2 x3 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_140 x1 x2 x3 x6 x5 x3500 = case x5 of
     Curry_Prelude.OP_List -> d_OP__case_139 x1 x2 x6 x3 x3500
     (Curry_Prelude.OP_Cons x294 x295) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_140 x1 x2 x3 x6 x1002 x3500) (d_OP__case_140 x1 x2 x3 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_140 x1 x2 x3 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_140 x1 x2 x3 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_140 x1 x2 x3 x6 x5 x3000 x3500 = case x5 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_139 x1 x2 x6 x3 x2000 x3500))
     (Curry_Prelude.OP_Cons x294 x295) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_140 x1 x2 x3 x6 x1002 x3000 x3500) (nd_OP__case_140 x1 x2 x3 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_140 x1 x2 x3 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_140 x1 x2 x3 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_139 x1 x2 x6 x3 x3500 = case x3 of
     (Curry_AbstractCurry.C_CApply x7 x8) -> d_OP__case_138 x1 x2 x3 x6 x8 x7 x3500
     (Curry_AbstractCurry.C_CVar x277) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLit x278) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CSymbol x279) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x280 x281) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x282 x283) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x284) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x285 x286) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x287 x288) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x289) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x290 x291) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x292 x293) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_139 x1 x2 x6 x1002 x3500) (d_OP__case_139 x1 x2 x6 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_139 x1 x2 x6 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_139 x1 x2 x6 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_139 x1 x2 x6 x3 x3000 x3500 = case x3 of
     (Curry_AbstractCurry.C_CApply x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_138 x1 x2 x3 x6 x8 x7 x2000 x3500))
     (Curry_AbstractCurry.C_CVar x277) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLit x278) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x279) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x280 x281) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x282 x283) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x284) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x285 x286) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x287 x288) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x289) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x290 x291) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x292 x293) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_139 x1 x2 x6 x1002 x3000 x3500) (nd_OP__case_139 x1 x2 x6 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_139 x1 x2 x6 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_139 x1 x2 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_138 x1 x2 x3 x6 x8 x7 x3500 = case x7 of
     (Curry_AbstractCurry.C_CApply x9 x10) -> d_OP__case_137 x1 x2 x3 x6 x8 x10 x9 x3500
     (Curry_AbstractCurry.C_CVar x260) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLit x261) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CSymbol x262) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x263 x264) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x265 x266) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x267) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x268 x269) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x270 x271) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x272) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x273 x274) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x275 x276) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_138 x1 x2 x3 x6 x8 x1002 x3500) (d_OP__case_138 x1 x2 x3 x6 x8 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_138 x1 x2 x3 x6 x8 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_138 x1 x2 x3 x6 x8 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_138 x1 x2 x3 x6 x8 x7 x3000 x3500 = case x7 of
     (Curry_AbstractCurry.C_CApply x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_137 x1 x2 x3 x6 x8 x10 x9 x2000 x3500))
     (Curry_AbstractCurry.C_CVar x260) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLit x261) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x262) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x263 x264) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x265 x266) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x267) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x268 x269) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x270 x271) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x272) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x273 x274) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x275 x276) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_138 x1 x2 x3 x6 x8 x1002 x3000 x3500) (nd_OP__case_138 x1 x2 x3 x6 x8 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_138 x1 x2 x3 x6 x8 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_138 x1 x2 x3 x6 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_137 x1 x2 x3 x6 x8 x10 x9 x3500 = case x9 of
     (Curry_AbstractCurry.C_CSymbol x11) -> d_OP__case_136 x1 x2 x3 x6 x8 x10 x11 x3500
     (Curry_AbstractCurry.C_CVar x242) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLit x243) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CApply x244 x245) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x246 x247) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x248 x249) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x250) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x251 x252) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x253 x254) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x255) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x256 x257) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x258 x259) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_137 x1 x2 x3 x6 x8 x10 x1002 x3500) (d_OP__case_137 x1 x2 x3 x6 x8 x10 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_137 x1 x2 x3 x6 x8 x10 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_137 x1 x2 x3 x6 x8 x10 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_137 x1 x2 x3 x6 x8 x10 x9 x3000 x3500 = case x9 of
     (Curry_AbstractCurry.C_CSymbol x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_136 x1 x2 x3 x6 x8 x10 x11 x2000 x3500))
     (Curry_AbstractCurry.C_CVar x242) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLit x243) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x244 x245) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x246 x247) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x248 x249) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x250) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x251 x252) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x253 x254) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x255) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x256 x257) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x258 x259) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_137 x1 x2 x3 x6 x8 x10 x1002 x3000 x3500) (nd_OP__case_137 x1 x2 x3 x6 x8 x10 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_137 x1 x2 x3 x6 x8 x10 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_137 x1 x2 x3 x6 x8 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_136 x1 x2 x3 x6 x8 x10 x11 x3500 = case x11 of
     (Curry_Prelude.OP_Tuple2 x12 x13) -> d_OP__case_135 x1 x2 x3 x6 x10 x13 x8 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_136 x1 x2 x3 x6 x8 x10 x1002 x3500) (d_OP__case_136 x1 x2 x3 x6 x8 x10 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_136 x1 x2 x3 x6 x8 x10 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_136 x1 x2 x3 x6 x8 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_136 x1 x2 x3 x6 x8 x10 x11 x3000 x3500 = case x11 of
     (Curry_Prelude.OP_Tuple2 x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_135 x1 x2 x3 x6 x10 x13 x8 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_136 x1 x2 x3 x6 x8 x10 x1002 x3000 x3500) (nd_OP__case_136 x1 x2 x3 x6 x8 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_136 x1 x2 x3 x6 x8 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_136 x1 x2 x3 x6 x8 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_135 x1 x2 x3 x6 x10 x13 x8 x3500 = case x8 of
     (Curry_AbstractCurry.C_CVar x14) -> d_OP__case_134 x1 x2 x3 x6 x10 x13 x14 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x13 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom x10 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x14) x10 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x15) -> d_OP__case_131 x1 x2 x3 x6 x13 x15 x10 x3500
     (Curry_AbstractCurry.C_CSymbol x35) -> d_OP__case_129 x1 x2 x3 x6 x13 x35 x10 x3500
     (Curry_AbstractCurry.C_CApply x55 x56) -> d_OP__case_127 x1 x2 x3 x6 x13 x55 x56 x10 x3500
     (Curry_AbstractCurry.C_CLambda x76 x77) -> d_OP__case_125 x1 x2 x3 x6 x13 x76 x77 x10 x3500
     (Curry_AbstractCurry.C_CLetDecl x97 x98) -> d_OP__case_123 x1 x2 x3 x6 x13 x97 x98 x10 x3500
     (Curry_AbstractCurry.C_CDoExpr x118) -> d_OP__case_121 x1 x2 x3 x6 x13 x118 x10 x3500
     (Curry_AbstractCurry.C_CListComp x138 x139) -> d_OP__case_119 x1 x2 x3 x6 x13 x138 x139 x10 x3500
     (Curry_AbstractCurry.C_CCase x159 x160) -> d_OP__case_117 x1 x2 x3 x6 x13 x159 x160 x10 x3500
     (Curry_AbstractCurry.C_CRecConstr x180) -> d_OP__case_115 x1 x2 x3 x6 x13 x180 x10 x3500
     (Curry_AbstractCurry.C_CRecSelect x200 x201) -> d_OP__case_113 x1 x2 x3 x6 x13 x200 x201 x10 x3500
     (Curry_AbstractCurry.C_CRecUpdate x221 x222) -> d_OP__case_111 x1 x2 x3 x6 x13 x221 x222 x10 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_135 x1 x2 x3 x6 x10 x13 x1002 x3500) (d_OP__case_135 x1 x2 x3 x6 x10 x13 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_135 x1 x2 x3 x6 x10 x13 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_135 x1 x2 x3 x6 x10 x13 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_135 x1 x2 x3 x6 x10 x13 x8 x3000 x3500 = case x8 of
     (Curry_AbstractCurry.C_CVar x14) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_134 x1 x2 x3 x6 x10 x13 x14 (Curry_Prelude.d_OP_ampersand_ampersand (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x13 x2001 x3500)))) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom x10 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x14) x10 x3500) x3500) x3500) x2003 x3500)))))
     (Curry_AbstractCurry.C_CLit x15) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_131 x1 x2 x3 x6 x13 x15 x10 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x35) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_129 x1 x2 x3 x6 x13 x35 x10 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x55 x56) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_127 x1 x2 x3 x6 x13 x55 x56 x10 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x76 x77) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_125 x1 x2 x3 x6 x13 x76 x77 x10 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x97 x98) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_123 x1 x2 x3 x6 x13 x97 x98 x10 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x118) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_121 x1 x2 x3 x6 x13 x118 x10 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x138 x139) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_119 x1 x2 x3 x6 x13 x138 x139 x10 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x159 x160) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_117 x1 x2 x3 x6 x13 x159 x160 x10 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x180) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_115 x1 x2 x3 x6 x13 x180 x10 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x200 x201) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_113 x1 x2 x3 x6 x13 x200 x201 x10 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x221 x222) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_111 x1 x2 x3 x6 x13 x221 x222 x10 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_135 x1 x2 x3 x6 x10 x13 x1002 x3000 x3500) (nd_OP__case_135 x1 x2 x3 x6 x10 x13 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_135 x1 x2 x3 x6 x10 x13 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_135 x1 x2 x3 x6 x10 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_111 x1 x2 x3 x6 x13 x221 x222 x10 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x223) -> d_OP__case_110 x1 x2 x3 x6 x13 x221 x222 x223 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x13 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x223 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CRecUpdate x221 x222) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x223) (Curry_AbstractCurry.C_CRecUpdate x221 x222) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x224) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CSymbol x225) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CApply x226 x227) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x228 x229) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x230 x231) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x232) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x233 x234) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x235 x236) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x237) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x238 x239) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x240 x241) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_111 x1 x2 x3 x6 x13 x221 x222 x1002 x3500) (d_OP__case_111 x1 x2 x3 x6 x13 x221 x222 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_111 x1 x2 x3 x6 x13 x221 x222 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_111 x1 x2 x3 x6 x13 x221 x222 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_111 x1 x2 x3 x6 x13 x221 x222 x10 x3000 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x223) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_110 x1 x2 x3 x6 x13 x221 x222 x223 (Curry_Prelude.d_OP_ampersand_ampersand (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x13 x2001 x3500)))) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x223 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CRecUpdate x221 x222) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x223) (Curry_AbstractCurry.C_CRecUpdate x221 x222) x3500) x3500) x3500) x3500) x2003 x3500)))))
     (Curry_AbstractCurry.C_CLit x224) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x225) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x226 x227) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x228 x229) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x230 x231) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x232) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x233 x234) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x235 x236) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x237) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x238 x239) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x240 x241) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_111 x1 x2 x3 x6 x13 x221 x222 x1002 x3000 x3500) (nd_OP__case_111 x1 x2 x3 x6 x13 x221 x222 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_111 x1 x2 x3 x6 x13 x221 x222 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_111 x1 x2 x3 x6 x13 x221 x222 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_110 x1 x2 x3 x6 x13 x221 x222 x223 x224 x3500 = case x224 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CRecUpdate x221 x222) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_110 x1 x2 x3 x6 x13 x221 x222 x223 x1002 x3500) (d_OP__case_110 x1 x2 x3 x6 x13 x221 x222 x223 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_110 x1 x2 x3 x6 x13 x221 x222 x223 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_110 x1 x2 x3 x6 x13 x221 x222 x223 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_110 x1 x2 x3 x6 x13 x221 x222 x223 x224 x3000 x3500 = case x224 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CRecUpdate x221 x222) x2000 x3500) x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_110 x1 x2 x3 x6 x13 x221 x222 x223 x1002 x3000 x3500) (nd_OP__case_110 x1 x2 x3 x6 x13 x221 x222 x223 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_110 x1 x2 x3 x6 x13 x221 x222 x223 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_110 x1 x2 x3 x6 x13 x221 x222 x223 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_113 x1 x2 x3 x6 x13 x200 x201 x10 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x202) -> d_OP__case_112 x1 x2 x3 x6 x13 x200 x201 x202 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x13 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x202 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CRecSelect x200 x201) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x202) (Curry_AbstractCurry.C_CRecSelect x200 x201) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x203) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CSymbol x204) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CApply x205 x206) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x207 x208) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x209 x210) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x211) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x212 x213) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x214 x215) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x216) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x217 x218) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x219 x220) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_113 x1 x2 x3 x6 x13 x200 x201 x1002 x3500) (d_OP__case_113 x1 x2 x3 x6 x13 x200 x201 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_113 x1 x2 x3 x6 x13 x200 x201 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_113 x1 x2 x3 x6 x13 x200 x201 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_113 x1 x2 x3 x6 x13 x200 x201 x10 x3000 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x202) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_112 x1 x2 x3 x6 x13 x200 x201 x202 (Curry_Prelude.d_OP_ampersand_ampersand (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x13 x2001 x3500)))) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x202 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CRecSelect x200 x201) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x202) (Curry_AbstractCurry.C_CRecSelect x200 x201) x3500) x3500) x3500) x3500) x2003 x3500)))))
     (Curry_AbstractCurry.C_CLit x203) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x204) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x205 x206) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x207 x208) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x209 x210) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x211) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x212 x213) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x214 x215) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x216) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x217 x218) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x219 x220) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_113 x1 x2 x3 x6 x13 x200 x201 x1002 x3000 x3500) (nd_OP__case_113 x1 x2 x3 x6 x13 x200 x201 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_113 x1 x2 x3 x6 x13 x200 x201 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_113 x1 x2 x3 x6 x13 x200 x201 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_112 x1 x2 x3 x6 x13 x200 x201 x202 x203 x3500 = case x203 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CRecSelect x200 x201) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_112 x1 x2 x3 x6 x13 x200 x201 x202 x1002 x3500) (d_OP__case_112 x1 x2 x3 x6 x13 x200 x201 x202 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_112 x1 x2 x3 x6 x13 x200 x201 x202 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_112 x1 x2 x3 x6 x13 x200 x201 x202 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_112 x1 x2 x3 x6 x13 x200 x201 x202 x203 x3000 x3500 = case x203 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CRecSelect x200 x201) x2000 x3500) x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_112 x1 x2 x3 x6 x13 x200 x201 x202 x1002 x3000 x3500) (nd_OP__case_112 x1 x2 x3 x6 x13 x200 x201 x202 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_112 x1 x2 x3 x6 x13 x200 x201 x202 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_112 x1 x2 x3 x6 x13 x200 x201 x202 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_115 x1 x2 x3 x6 x13 x180 x10 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x181) -> d_OP__case_114 x1 x2 x3 x6 x13 x180 x181 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x13 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x181 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CRecConstr x180) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x181) (Curry_AbstractCurry.C_CRecConstr x180) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x182) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CSymbol x183) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CApply x184 x185) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x186 x187) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x188 x189) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x190) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x191 x192) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x193 x194) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x195) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x196 x197) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x198 x199) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_115 x1 x2 x3 x6 x13 x180 x1002 x3500) (d_OP__case_115 x1 x2 x3 x6 x13 x180 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_115 x1 x2 x3 x6 x13 x180 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_115 x1 x2 x3 x6 x13 x180 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_115 x1 x2 x3 x6 x13 x180 x10 x3000 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x181) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_114 x1 x2 x3 x6 x13 x180 x181 (Curry_Prelude.d_OP_ampersand_ampersand (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x13 x2001 x3500)))) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x181 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CRecConstr x180) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x181) (Curry_AbstractCurry.C_CRecConstr x180) x3500) x3500) x3500) x3500) x2003 x3500)))))
     (Curry_AbstractCurry.C_CLit x182) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x183) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x184 x185) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x186 x187) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x188 x189) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x190) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x191 x192) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x193 x194) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x195) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x196 x197) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x198 x199) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_115 x1 x2 x3 x6 x13 x180 x1002 x3000 x3500) (nd_OP__case_115 x1 x2 x3 x6 x13 x180 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_115 x1 x2 x3 x6 x13 x180 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_115 x1 x2 x3 x6 x13 x180 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_114 x1 x2 x3 x6 x13 x180 x181 x182 x3500 = case x182 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CRecConstr x180) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_114 x1 x2 x3 x6 x13 x180 x181 x1002 x3500) (d_OP__case_114 x1 x2 x3 x6 x13 x180 x181 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_114 x1 x2 x3 x6 x13 x180 x181 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_114 x1 x2 x3 x6 x13 x180 x181 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_114 x1 x2 x3 x6 x13 x180 x181 x182 x3000 x3500 = case x182 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CRecConstr x180) x2000 x3500) x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_114 x1 x2 x3 x6 x13 x180 x181 x1002 x3000 x3500) (nd_OP__case_114 x1 x2 x3 x6 x13 x180 x181 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_114 x1 x2 x3 x6 x13 x180 x181 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_114 x1 x2 x3 x6 x13 x180 x181 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_117 x1 x2 x3 x6 x13 x159 x160 x10 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x161) -> d_OP__case_116 x1 x2 x3 x6 x13 x159 x160 x161 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x13 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x161 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CCase x159 x160) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x161) (Curry_AbstractCurry.C_CCase x159 x160) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x162) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CSymbol x163) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CApply x164 x165) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x166 x167) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x168 x169) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x170) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x171 x172) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x173 x174) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x175) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x176 x177) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x178 x179) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_117 x1 x2 x3 x6 x13 x159 x160 x1002 x3500) (d_OP__case_117 x1 x2 x3 x6 x13 x159 x160 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_117 x1 x2 x3 x6 x13 x159 x160 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_117 x1 x2 x3 x6 x13 x159 x160 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_117 x1 x2 x3 x6 x13 x159 x160 x10 x3000 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x161) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_116 x1 x2 x3 x6 x13 x159 x160 x161 (Curry_Prelude.d_OP_ampersand_ampersand (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x13 x2001 x3500)))) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x161 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CCase x159 x160) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x161) (Curry_AbstractCurry.C_CCase x159 x160) x3500) x3500) x3500) x3500) x2003 x3500)))))
     (Curry_AbstractCurry.C_CLit x162) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x163) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x164 x165) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x166 x167) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x168 x169) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x170) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x171 x172) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x173 x174) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x175) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x176 x177) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x178 x179) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_117 x1 x2 x3 x6 x13 x159 x160 x1002 x3000 x3500) (nd_OP__case_117 x1 x2 x3 x6 x13 x159 x160 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_117 x1 x2 x3 x6 x13 x159 x160 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_117 x1 x2 x3 x6 x13 x159 x160 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_116 x1 x2 x3 x6 x13 x159 x160 x161 x162 x3500 = case x162 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CCase x159 x160) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_116 x1 x2 x3 x6 x13 x159 x160 x161 x1002 x3500) (d_OP__case_116 x1 x2 x3 x6 x13 x159 x160 x161 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_116 x1 x2 x3 x6 x13 x159 x160 x161 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_116 x1 x2 x3 x6 x13 x159 x160 x161 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_116 x1 x2 x3 x6 x13 x159 x160 x161 x162 x3000 x3500 = case x162 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CCase x159 x160) x2000 x3500) x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_116 x1 x2 x3 x6 x13 x159 x160 x161 x1002 x3000 x3500) (nd_OP__case_116 x1 x2 x3 x6 x13 x159 x160 x161 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_116 x1 x2 x3 x6 x13 x159 x160 x161 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_116 x1 x2 x3 x6 x13 x159 x160 x161 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_119 x1 x2 x3 x6 x13 x138 x139 x10 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x140) -> d_OP__case_118 x1 x2 x3 x6 x13 x138 x139 x140 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x13 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x140 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CListComp x138 x139) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x140) (Curry_AbstractCurry.C_CListComp x138 x139) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x141) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CSymbol x142) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CApply x143 x144) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x145 x146) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x147 x148) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x149) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x150 x151) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x152 x153) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x154) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x155 x156) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x157 x158) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_119 x1 x2 x3 x6 x13 x138 x139 x1002 x3500) (d_OP__case_119 x1 x2 x3 x6 x13 x138 x139 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_119 x1 x2 x3 x6 x13 x138 x139 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_119 x1 x2 x3 x6 x13 x138 x139 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_119 x1 x2 x3 x6 x13 x138 x139 x10 x3000 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x140) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_118 x1 x2 x3 x6 x13 x138 x139 x140 (Curry_Prelude.d_OP_ampersand_ampersand (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x13 x2001 x3500)))) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x140 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CListComp x138 x139) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x140) (Curry_AbstractCurry.C_CListComp x138 x139) x3500) x3500) x3500) x3500) x2003 x3500)))))
     (Curry_AbstractCurry.C_CLit x141) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x142) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x143 x144) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x145 x146) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x147 x148) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x149) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x150 x151) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x152 x153) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x154) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x155 x156) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x157 x158) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_119 x1 x2 x3 x6 x13 x138 x139 x1002 x3000 x3500) (nd_OP__case_119 x1 x2 x3 x6 x13 x138 x139 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_119 x1 x2 x3 x6 x13 x138 x139 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_119 x1 x2 x3 x6 x13 x138 x139 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_118 x1 x2 x3 x6 x13 x138 x139 x140 x141 x3500 = case x141 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CListComp x138 x139) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_118 x1 x2 x3 x6 x13 x138 x139 x140 x1002 x3500) (d_OP__case_118 x1 x2 x3 x6 x13 x138 x139 x140 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_118 x1 x2 x3 x6 x13 x138 x139 x140 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_118 x1 x2 x3 x6 x13 x138 x139 x140 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_118 x1 x2 x3 x6 x13 x138 x139 x140 x141 x3000 x3500 = case x141 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CListComp x138 x139) x2000 x3500) x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_118 x1 x2 x3 x6 x13 x138 x139 x140 x1002 x3000 x3500) (nd_OP__case_118 x1 x2 x3 x6 x13 x138 x139 x140 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_118 x1 x2 x3 x6 x13 x138 x139 x140 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_118 x1 x2 x3 x6 x13 x138 x139 x140 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_121 x1 x2 x3 x6 x13 x118 x10 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x119) -> d_OP__case_120 x1 x2 x3 x6 x13 x118 x119 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x13 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x119 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CDoExpr x118) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x119) (Curry_AbstractCurry.C_CDoExpr x118) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x120) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CSymbol x121) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CApply x122 x123) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x124 x125) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x126 x127) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x128) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x129 x130) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x131 x132) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x133) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x134 x135) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x136 x137) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_121 x1 x2 x3 x6 x13 x118 x1002 x3500) (d_OP__case_121 x1 x2 x3 x6 x13 x118 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_121 x1 x2 x3 x6 x13 x118 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_121 x1 x2 x3 x6 x13 x118 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_121 x1 x2 x3 x6 x13 x118 x10 x3000 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x119) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_120 x1 x2 x3 x6 x13 x118 x119 (Curry_Prelude.d_OP_ampersand_ampersand (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x13 x2001 x3500)))) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x119 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CDoExpr x118) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x119) (Curry_AbstractCurry.C_CDoExpr x118) x3500) x3500) x3500) x3500) x2003 x3500)))))
     (Curry_AbstractCurry.C_CLit x120) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x121) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x122 x123) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x124 x125) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x126 x127) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x128) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x129 x130) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x131 x132) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x133) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x134 x135) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x136 x137) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_121 x1 x2 x3 x6 x13 x118 x1002 x3000 x3500) (nd_OP__case_121 x1 x2 x3 x6 x13 x118 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_121 x1 x2 x3 x6 x13 x118 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_121 x1 x2 x3 x6 x13 x118 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_120 x1 x2 x3 x6 x13 x118 x119 x120 x3500 = case x120 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CDoExpr x118) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_120 x1 x2 x3 x6 x13 x118 x119 x1002 x3500) (d_OP__case_120 x1 x2 x3 x6 x13 x118 x119 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_120 x1 x2 x3 x6 x13 x118 x119 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_120 x1 x2 x3 x6 x13 x118 x119 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_120 x1 x2 x3 x6 x13 x118 x119 x120 x3000 x3500 = case x120 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CDoExpr x118) x2000 x3500) x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_120 x1 x2 x3 x6 x13 x118 x119 x1002 x3000 x3500) (nd_OP__case_120 x1 x2 x3 x6 x13 x118 x119 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_120 x1 x2 x3 x6 x13 x118 x119 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_120 x1 x2 x3 x6 x13 x118 x119 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_123 x1 x2 x3 x6 x13 x97 x98 x10 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x99) -> d_OP__case_122 x1 x2 x3 x6 x13 x97 x98 x99 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x13 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x99 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CLetDecl x97 x98) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x99) (Curry_AbstractCurry.C_CLetDecl x97 x98) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x100) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CSymbol x101) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CApply x102 x103) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x104 x105) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x106 x107) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x108) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x109 x110) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x111 x112) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x113) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x114 x115) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x116 x117) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_123 x1 x2 x3 x6 x13 x97 x98 x1002 x3500) (d_OP__case_123 x1 x2 x3 x6 x13 x97 x98 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_123 x1 x2 x3 x6 x13 x97 x98 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_123 x1 x2 x3 x6 x13 x97 x98 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_123 x1 x2 x3 x6 x13 x97 x98 x10 x3000 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x99) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_122 x1 x2 x3 x6 x13 x97 x98 x99 (Curry_Prelude.d_OP_ampersand_ampersand (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x13 x2001 x3500)))) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x99 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CLetDecl x97 x98) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x99) (Curry_AbstractCurry.C_CLetDecl x97 x98) x3500) x3500) x3500) x3500) x2003 x3500)))))
     (Curry_AbstractCurry.C_CLit x100) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x101) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x102 x103) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x104 x105) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x106 x107) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x108) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x109 x110) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x111 x112) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x113) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x114 x115) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x116 x117) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_123 x1 x2 x3 x6 x13 x97 x98 x1002 x3000 x3500) (nd_OP__case_123 x1 x2 x3 x6 x13 x97 x98 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_123 x1 x2 x3 x6 x13 x97 x98 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_123 x1 x2 x3 x6 x13 x97 x98 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_122 x1 x2 x3 x6 x13 x97 x98 x99 x100 x3500 = case x100 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CLetDecl x97 x98) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_122 x1 x2 x3 x6 x13 x97 x98 x99 x1002 x3500) (d_OP__case_122 x1 x2 x3 x6 x13 x97 x98 x99 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_122 x1 x2 x3 x6 x13 x97 x98 x99 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_122 x1 x2 x3 x6 x13 x97 x98 x99 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_122 x1 x2 x3 x6 x13 x97 x98 x99 x100 x3000 x3500 = case x100 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CLetDecl x97 x98) x2000 x3500) x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_122 x1 x2 x3 x6 x13 x97 x98 x99 x1002 x3000 x3500) (nd_OP__case_122 x1 x2 x3 x6 x13 x97 x98 x99 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_122 x1 x2 x3 x6 x13 x97 x98 x99 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_122 x1 x2 x3 x6 x13 x97 x98 x99 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_125 x1 x2 x3 x6 x13 x76 x77 x10 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x78) -> d_OP__case_124 x1 x2 x3 x6 x13 x76 x77 x78 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x13 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x78 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CLambda x76 x77) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x78) (Curry_AbstractCurry.C_CLambda x76 x77) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x79) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CSymbol x80) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CApply x81 x82) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x83 x84) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x85 x86) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x87) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x88 x89) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x90 x91) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x92) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x93 x94) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x95 x96) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_125 x1 x2 x3 x6 x13 x76 x77 x1002 x3500) (d_OP__case_125 x1 x2 x3 x6 x13 x76 x77 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_125 x1 x2 x3 x6 x13 x76 x77 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_125 x1 x2 x3 x6 x13 x76 x77 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_125 x1 x2 x3 x6 x13 x76 x77 x10 x3000 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x78) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_124 x1 x2 x3 x6 x13 x76 x77 x78 (Curry_Prelude.d_OP_ampersand_ampersand (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x13 x2001 x3500)))) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x78 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CLambda x76 x77) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x78) (Curry_AbstractCurry.C_CLambda x76 x77) x3500) x3500) x3500) x3500) x2003 x3500)))))
     (Curry_AbstractCurry.C_CLit x79) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x80) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x81 x82) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x83 x84) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x85 x86) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x87) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x88 x89) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x90 x91) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x92) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x93 x94) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x95 x96) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_125 x1 x2 x3 x6 x13 x76 x77 x1002 x3000 x3500) (nd_OP__case_125 x1 x2 x3 x6 x13 x76 x77 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_125 x1 x2 x3 x6 x13 x76 x77 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_125 x1 x2 x3 x6 x13 x76 x77 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_124 x1 x2 x3 x6 x13 x76 x77 x78 x79 x3500 = case x79 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CLambda x76 x77) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_124 x1 x2 x3 x6 x13 x76 x77 x78 x1002 x3500) (d_OP__case_124 x1 x2 x3 x6 x13 x76 x77 x78 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_124 x1 x2 x3 x6 x13 x76 x77 x78 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_124 x1 x2 x3 x6 x13 x76 x77 x78 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_124 x1 x2 x3 x6 x13 x76 x77 x78 x79 x3000 x3500 = case x79 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CLambda x76 x77) x2000 x3500) x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_124 x1 x2 x3 x6 x13 x76 x77 x78 x1002 x3000 x3500) (nd_OP__case_124 x1 x2 x3 x6 x13 x76 x77 x78 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_124 x1 x2 x3 x6 x13 x76 x77 x78 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_124 x1 x2 x3 x6 x13 x76 x77 x78 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_127 x1 x2 x3 x6 x13 x55 x56 x10 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x57) -> d_OP__case_126 x1 x2 x3 x6 x13 x55 x56 x57 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x13 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x57 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CApply x55 x56) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x57) (Curry_AbstractCurry.C_CApply x55 x56) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x58) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CSymbol x59) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CApply x60 x61) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x62 x63) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x64 x65) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x66) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x67 x68) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x69 x70) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x71) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x72 x73) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x74 x75) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_127 x1 x2 x3 x6 x13 x55 x56 x1002 x3500) (d_OP__case_127 x1 x2 x3 x6 x13 x55 x56 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_127 x1 x2 x3 x6 x13 x55 x56 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_127 x1 x2 x3 x6 x13 x55 x56 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_127 x1 x2 x3 x6 x13 x55 x56 x10 x3000 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x57) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_126 x1 x2 x3 x6 x13 x55 x56 x57 (Curry_Prelude.d_OP_ampersand_ampersand (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x13 x2001 x3500)))) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x57 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CApply x55 x56) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x57) (Curry_AbstractCurry.C_CApply x55 x56) x3500) x3500) x3500) x3500) x2003 x3500)))))
     (Curry_AbstractCurry.C_CLit x58) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x59) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x60 x61) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x62 x63) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x64 x65) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x66) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x67 x68) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x69 x70) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x71) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x72 x73) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x74 x75) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_127 x1 x2 x3 x6 x13 x55 x56 x1002 x3000 x3500) (nd_OP__case_127 x1 x2 x3 x6 x13 x55 x56 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_127 x1 x2 x3 x6 x13 x55 x56 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_127 x1 x2 x3 x6 x13 x55 x56 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_126 x1 x2 x3 x6 x13 x55 x56 x57 x58 x3500 = case x58 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CApply x55 x56) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_126 x1 x2 x3 x6 x13 x55 x56 x57 x1002 x3500) (d_OP__case_126 x1 x2 x3 x6 x13 x55 x56 x57 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_126 x1 x2 x3 x6 x13 x55 x56 x57 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_126 x1 x2 x3 x6 x13 x55 x56 x57 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_126 x1 x2 x3 x6 x13 x55 x56 x57 x58 x3000 x3500 = case x58 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CApply x55 x56) x2000 x3500) x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_126 x1 x2 x3 x6 x13 x55 x56 x57 x1002 x3000 x3500) (nd_OP__case_126 x1 x2 x3 x6 x13 x55 x56 x57 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_126 x1 x2 x3 x6 x13 x55 x56 x57 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_126 x1 x2 x3 x6 x13 x55 x56 x57 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_129 x1 x2 x3 x6 x13 x35 x10 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x36) -> d_OP__case_128 x1 x2 x3 x6 x13 x35 x36 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x13 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x36 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CSymbol x35) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x36) (Curry_AbstractCurry.C_CSymbol x35) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x37) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CSymbol x38) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CApply x39 x40) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x41 x42) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x43 x44) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x45) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x46 x47) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x48 x49) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x50) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x51 x52) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x53 x54) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_129 x1 x2 x3 x6 x13 x35 x1002 x3500) (d_OP__case_129 x1 x2 x3 x6 x13 x35 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_129 x1 x2 x3 x6 x13 x35 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_129 x1 x2 x3 x6 x13 x35 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_129 x1 x2 x3 x6 x13 x35 x10 x3000 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x36) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_128 x1 x2 x3 x6 x13 x35 x36 (Curry_Prelude.d_OP_ampersand_ampersand (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x13 x2001 x3500)))) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x36 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CSymbol x35) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x36) (Curry_AbstractCurry.C_CSymbol x35) x3500) x3500) x3500) x3500) x2003 x3500)))))
     (Curry_AbstractCurry.C_CLit x37) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x38) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x39 x40) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x41 x42) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x43 x44) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x45) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x46 x47) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x48 x49) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x50) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x51 x52) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x53 x54) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_129 x1 x2 x3 x6 x13 x35 x1002 x3000 x3500) (nd_OP__case_129 x1 x2 x3 x6 x13 x35 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_129 x1 x2 x3 x6 x13 x35 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_129 x1 x2 x3 x6 x13 x35 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_128 x1 x2 x3 x6 x13 x35 x36 x37 x3500 = case x37 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CSymbol x35) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_128 x1 x2 x3 x6 x13 x35 x36 x1002 x3500) (d_OP__case_128 x1 x2 x3 x6 x13 x35 x36 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_128 x1 x2 x3 x6 x13 x35 x36 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_128 x1 x2 x3 x6 x13 x35 x36 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_128 x1 x2 x3 x6 x13 x35 x36 x37 x3000 x3500 = case x37 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CSymbol x35) x2000 x3500) x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_128 x1 x2 x3 x6 x13 x35 x36 x1002 x3000 x3500) (nd_OP__case_128 x1 x2 x3 x6 x13 x35 x36 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_128 x1 x2 x3 x6 x13 x35 x36 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_128 x1 x2 x3 x6 x13 x35 x36 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_131 x1 x2 x3 x6 x13 x15 x10 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x16) -> d_OP__case_130 x1 x2 x3 x6 x13 x15 x16 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x13 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x16 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CLit x15) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x16) (Curry_AbstractCurry.C_CLit x15) x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CLit x17) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CSymbol x18) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CApply x19 x20) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLambda x21 x22) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CLetDecl x23 x24) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CDoExpr x25) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CListComp x26 x27) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CCase x28 x29) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecConstr x30) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecSelect x31 x32) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.C_CRecUpdate x33 x34) -> d_C_showLambda x1 x2 x3 x3500
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_131 x1 x2 x3 x6 x13 x15 x1002 x3500) (d_OP__case_131 x1 x2 x3 x6 x13 x15 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_131 x1 x2 x3 x6 x13 x15 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_131 x1 x2 x3 x6 x13 x15 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_131 x1 x2 x3 x6 x13 x15 x10 x3000 x3500 = case x10 of
     (Curry_AbstractCurry.C_CVar x16) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_130 x1 x2 x3 x6 x13 x15 x16 (Curry_Prelude.d_OP_ampersand_ampersand (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x13 x2001 x3500)))) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x16 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractCurry.C_CLit x15) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractCurry.C_CVar x16) (Curry_AbstractCurry.C_CLit x15) x3500) x3500) x3500) x3500) x2003 x3500)))))
     (Curry_AbstractCurry.C_CLit x17) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CSymbol x18) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CApply x19 x20) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLambda x21 x22) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CLetDecl x23 x24) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CDoExpr x25) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CListComp x26 x27) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CCase x28 x29) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecConstr x30) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecSelect x31 x32) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.C_CRecUpdate x33 x34) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_131 x1 x2 x3 x6 x13 x15 x1002 x3000 x3500) (nd_OP__case_131 x1 x2 x3 x6 x13 x15 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_131 x1 x2 x3 x6 x13 x15 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_131 x1 x2 x3 x6 x13 x15 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_130 x1 x2 x3 x6 x13 x15 x16 x17 x3500 = case x17 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CLit x15) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_130 x1 x2 x3 x6 x13 x15 x16 x1002 x3500) (d_OP__case_130 x1 x2 x3 x6 x13 x15 x16 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_130 x1 x2 x3 x6 x13 x15 x16 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_130 x1 x2 x3 x6 x13 x15 x16 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_130 x1 x2 x3 x6 x13 x15 x16 x17 x3000 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showBoxedExpr x1 (Curry_AbstractCurry.C_CLit x15) x2000 x3500) x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_130 x1 x2 x3 x6 x13 x15 x16 x1002 x3000 x3500) (nd_OP__case_130 x1 x2 x3 x6 x13 x15 x16 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_130 x1 x2 x3 x6 x13 x15 x16 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_130 x1 x2 x3 x6 x13 x15 x16 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_134 x1 x2 x3 x6 x10 x13 x14 x15 x3500 = case x15 of
     Curry_Prelude.C_True -> d_OP__case_133 x1 x2 x3 x6 x10 x13 x14 (Curry_Prelude.d_OP_eq_eq x6 x14 x3500) x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_134 x1 x2 x3 x6 x10 x13 x14 x1002 x3500) (d_OP__case_134 x1 x2 x3 x6 x10 x13 x14 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_134 x1 x2 x3 x6 x10 x13 x14 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_134 x1 x2 x3 x6 x10 x13 x14 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_134 x1 x2 x3 x6 x10 x13 x14 x15 x3000 x3500 = case x15 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_133 x1 x2 x3 x6 x10 x13 x14 (Curry_Prelude.d_OP_eq_eq x6 x14 x3500) x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_134 x1 x2 x3 x6 x10 x13 x14 x1002 x3000 x3500) (nd_OP__case_134 x1 x2 x3 x6 x10 x13 x14 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_134 x1 x2 x3 x6 x10 x13 x14 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_134 x1 x2 x3 x6 x10 x13 x14 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_133 x1 x2 x3 x6 x10 x13 x14 x15 x3500 = case x15 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x10 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x13 x3500) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_132 x1 x2 x3 x6 x10 x13 x14 (Curry_Prelude.d_OP_eq_eq x10 (Curry_AbstractCurry.C_CVar x6) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_133 x1 x2 x3 x6 x10 x13 x14 x1002 x3500) (d_OP__case_133 x1 x2 x3 x6 x10 x13 x14 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_133 x1 x2 x3 x6 x10 x13 x14 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_133 x1 x2 x3 x6 x10 x13 x14 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_133 x1 x2 x3 x6 x10 x13 x14 x15 x3000 x3500 = case x15 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (Curry_Prelude.d_OP_plus_plus (nd_C_showBoxedExpr x1 x10 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x13 x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_132 x1 x2 x3 x6 x10 x13 x14 (Curry_Prelude.d_OP_eq_eq x10 (Curry_AbstractCurry.C_CVar x6) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_133 x1 x2 x3 x6 x10 x13 x14 x1002 x3000 x3500) (nd_OP__case_133 x1 x2 x3 x6 x10 x13 x14 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_133 x1 x2 x3 x6 x10 x13 x14 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_133 x1 x2 x3 x6 x10 x13 x14 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_132 x1 x2 x3 x6 x10 x13 x14 x15 x3500 = case x15 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showExprOpt x1 (Curry_AbstractCurry.C_CVar x14) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_132 x1 x2 x3 x6 x10 x13 x14 x1002 x3500) (d_OP__case_132 x1 x2 x3 x6 x10 x13 x14 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_132 x1 x2 x3 x6 x10 x13 x14 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_132 x1 x2 x3 x6 x10 x13 x14 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_132 x1 x2 x3 x6 x10 x13 x14 x15 x3000 x3500 = case x15 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (nd_C_showExprOpt x1 (Curry_AbstractCurry.C_CVar x14) x2000 x3500) x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showLambda x1 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_132 x1 x2 x3 x6 x10 x13 x14 x1002 x3000 x3500) (nd_OP__case_132 x1 x2 x3 x6 x10 x13 x14 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_132 x1 x2 x3 x6 x10 x13 x14 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_132 x1 x2 x3 x6 x10 x13 x14 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_145 x3 x4 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_144 x3 x4 x5 x6 (Curry_Prelude.d_OP_eq_eq x4 x5 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_145 x3 x4 x1002 x3500) (d_OP__case_145 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_145 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_145 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_145 x3 x4 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_144 x3 x4 x5 x6 (Curry_Prelude.d_OP_eq_eq x4 x5 x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_145 x3 x4 x1002 x3000 x3500) (nd_OP__case_145 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_145 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_145 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_144 x3 x4 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> d_OP__case_143 x3 x5 x6 (Curry_Maybe.d_C_isJust (Curry_FiniteMap.d_C_lookupFM x3 x6 x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_144 x3 x4 x5 x6 x1002 x3500) (d_OP__case_144 x3 x4 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_144 x3 x4 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_144 x3 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_144 x3 x4 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_143 x3 x5 x6 (Curry_Maybe.d_C_isJust (Curry_FiniteMap.nd_C_lookupFM x3 x6 x2000 x3500) x3500) x2001 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_144 x3 x4 x5 x6 x1002 x3000 x3500) (nd_OP__case_144 x3 x4 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_144 x3 x4 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_144 x3 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_143 x3 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) x6 x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_142 x6 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_143 x3 x5 x6 x1002 x3500) (d_OP__case_143 x3 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_143 x3 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_143 x3 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_143 x3 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) x6 x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_142 x6 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_143 x3 x5 x6 x1002 x3000 x3500) (nd_OP__case_143 x3 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_143 x3 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_143 x3 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_142 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_142 x6 x1002 x3500) (d_OP__case_142 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_142 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_142 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_142 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_142 x6 x1002 x3000 x3500) (nd_OP__case_142 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_142 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_142 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_146 x1 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_parens (d_C_showSymbol x1 x7 x3500) x3500
     Curry_Prelude.C_False -> d_C_showSymbol x1 x7 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_146 x1 x7 x1002 x3500) (d_OP__case_146 x1 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_146 x1 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_146 x1 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_146 x1 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_parens (nd_C_showSymbol x1 x7 x2000 x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showSymbol x1 x7 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_146 x1 x7 x1002 x3000 x3500) (nd_OP__case_146 x1 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_146 x1 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_146 x1 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_147 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_C_apply (d_C_showIdentifier x3500) x5 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_147 x1002 x3500) (d_OP__case_147 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_147 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_147 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_147 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_showIdentifier x2000 x3500) x5 x2001 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_147 x1002 x3000 x3500) (nd_OP__case_147 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_147 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_147 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_148 x1 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))) (d_C_showBlock (d_C_prefixMap (d_C_showLocalDecl x1) x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_148 x1 x6 x1002 x3500) (d_OP__case_148 x1 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_148 x1 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_148 x1 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_148 x1 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))) (d_C_showBlock (nd_C_prefixMap (wrapNX id (nd_C_showLocalDecl x1)) x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x2000 x3500) x3500) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_148 x1 x6 x1002 x3000 x3500) (nd_OP__case_148 x1 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_148 x1 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_148 x1 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_151 x1 x2 x3 x5 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_150 x1 x2 x3 x5 x6 x7 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x5 Curry_Prelude.OP_List x3500) (Curry_Prelude.d_OP_eq_eq x6 (Curry_AbstractCurry.C_CSymbol (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_151 x1 x2 x3 x5 x1002 x3500) (d_OP__case_151 x1 x2 x3 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_151 x1 x2 x3 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_151 x1 x2 x3 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_151 x1 x2 x3 x5 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_150 x1 x2 x3 x5 x6 x7 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x5 Curry_Prelude.OP_List x3500) (Curry_Prelude.d_OP_eq_eq x6 (Curry_AbstractCurry.C_CSymbol (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_151 x1 x2 x3 x5 x1002 x3000 x3500) (nd_OP__case_151 x1 x2 x3 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_151 x1 x2 x3 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_151 x1 x2 x3 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_150 x1 x2 x3 x5 x6 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (d_C_showExprOpt x1 x7 x3500)) x3500)
     Curry_Prelude.C_False -> d_OP__case_149 x1 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_150 x1 x2 x3 x5 x6 x7 x1002 x3500) (d_OP__case_150 x1 x2 x3 x5 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_150 x1 x2 x3 x5 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_150 x1 x2 x3 x5 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_150 x1 x2 x3 x5 x6 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (nd_C_showExprOpt x1 x7 x2000 x3500)) x3500)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_149 x1 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_150 x1 x2 x3 x5 x6 x7 x1002 x3000 x3500) (nd_OP__case_150 x1 x2 x3 x5 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_150 x1 x2 x3 x5 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_150 x1 x2 x3 x5 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_149 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (d_C_showBlock (d_C_combineMap (d_C_showGuard x1 x2) x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_149 x1 x2 x3 x1002 x3500) (d_OP__case_149 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_149 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_149 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_149 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (d_C_showBlock (nd_C_combineMap (wrapNX id (nd_C_showGuard x1 x2)) x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x2000 x3500) x3500)))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_149 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_149 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_149 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_149 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_152 x1 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))) (d_C_showBlock (d_C_prefixMap (d_C_showLocalDecl x1) x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_152 x1 x5 x1002 x3500) (d_OP__case_152 x1 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_152 x1 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_152 x1 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_152 x1 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))) (d_C_showBlock (nd_C_prefixMap (wrapNX id (nd_C_showLocalDecl x1)) x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x2000 x3500) x3500) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_152 x1 x5 x1002 x3000 x3500) (nd_OP__case_152 x1 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_152 x1 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_152 x1 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_153 x1 x2 x3 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x2 x6 x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x5 x6 x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_153 x1 x2 x3 x5 x6 x1002 x3500) (d_OP__case_153 x1 x2 x3 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_153 x1 x2 x3 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_153 x1 x2 x3 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_153 x1 x2 x3 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x2 x6 x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x5 x6 x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_153 x1 x2 x3 x5 x6 x1002 x3000 x3500) (nd_OP__case_153 x1 x2 x3 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_153 x1 x2 x3 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_153 x1 x2 x3 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_160 x1 x2 x5 x7 x8 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x9 x10) -> d_OP__case_159 x1 x2 x5 x7 x10 x8 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_160 x1 x2 x5 x7 x8 x1002 x3500) (d_OP__case_160 x1 x2 x5 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_160 x1 x2 x5 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_160 x1 x2 x5 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_160 x1 x2 x5 x7 x8 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_159 x1 x2 x5 x7 x10 x8 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_160 x1 x2 x5 x7 x8 x1002 x3000 x3500) (nd_OP__case_160 x1 x2 x5 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_160 x1 x2 x5 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_160 x1 x2 x5 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_159 x1 x2 x5 x7 x10 x8 x3500 = case x8 of
     (Curry_AbstractCurry.C_CRules x11 x12) -> let
          x13 = Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x10 x3500
          x14 = d_OP__case_155 x10 x13 x3500
           in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_funcComment x3500) x2 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP__case_158 x11 x14 (Curry_Prelude.d_OP_eq_eq x11 Curry_AbstractCurry.C_CFlex x3500) x3500) (Curry_Prelude.d_OP_plus_plus (d_OP__case_157 x7 x14 (d_C_isUntyped x7 x3500) x3500) (d_OP__case_156 x1 x5 x10 x12 x14 x13 x3500) x3500) x3500) x3500)
     (Curry_AbstractCurry.C_CExternal x15) -> let
          x16 = d_OP__case_154 x10 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x10 x3500) x3500
           in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_funcComment x3500) x2 x3500) (Curry_Prelude.d_OP_plus_plus x16 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExpr Curry_Prelude.C_False x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x16 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))))))) x3500) x3500) x3500) x3500) x3500) x3500)
     (Curry_AbstractCurry.Choice_C_CRules x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_159 x1 x2 x5 x7 x10 x1002 x3500) (d_OP__case_159 x1 x2 x5 x7 x10 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CRules x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_159 x1 x2 x5 x7 x10 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CRules x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_159 x1 x2 x5 x7 x10 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CRules x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_159 x1 x2 x5 x7 x10 x8 x3000 x3500 = case x8 of
     (Curry_AbstractCurry.C_CRules x11 x12) -> let
          x2013 = x3000
           in (seq x2013 (let
               x2002 = leftSupply x2013
               x2014 = rightSupply x2013
                in (seq x2002 (seq x2014 (let
                    x2003 = leftSupply x2014
                    x2012 = rightSupply x2014
                     in (seq x2003 (seq x2012 (let
                         x13 = let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x10 x2001 x3500)))
                         x14 = nd_OP__case_155 x10 x13 x2003 x3500
                          in (let
                              x2006 = leftSupply x2012
                              x2011 = rightSupply x2012
                               in (seq x2006 (seq x2011 (Curry_Prelude.d_OP_plus_plus (let
                                   x2005 = leftSupply x2006
                                   x2004 = rightSupply x2006
                                    in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (nd_C_funcComment x2004 x3500) x2 x2005 x3500)))) (let
                                   x2007 = leftSupply x2011
                                   x2010 = rightSupply x2011
                                    in (seq x2007 (seq x2010 (Curry_Prelude.d_OP_plus_plus (nd_OP__case_158 x11 x14 (Curry_Prelude.d_OP_eq_eq x11 Curry_AbstractCurry.C_CFlex x3500) x2007 x3500) (let
                                        x2008 = leftSupply x2010
                                        x2009 = rightSupply x2010
                                         in (seq x2008 (seq x2009 (Curry_Prelude.d_OP_plus_plus (nd_OP__case_157 x7 x14 (d_C_isUntyped x7 x3500) x2008 x3500) (nd_OP__case_156 x1 x5 x10 x12 x14 x13 x2009 x3500) x3500)))) x3500)))) x3500))))))))))))
     (Curry_AbstractCurry.C_CExternal x15) -> let
          x2008 = x3000
           in (seq x2008 (let
               x2004 = leftSupply x2008
               x2007 = rightSupply x2008
                in (seq x2004 (seq x2007 (let
                    x16 = let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (nd_OP__case_154 x10 (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x10 x2001 x3500)))) x2003 x3500)))
                     in (Curry_Prelude.d_OP_plus_plus (let
                         x2006 = leftSupply x2007
                         x2005 = rightSupply x2007
                          in (seq x2006 (seq x2005 (Curry_Prelude.nd_C_apply (nd_C_funcComment x2005 x3500) x2 x2006 x3500)))) (Curry_Prelude.d_OP_plus_plus x16 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExpr Curry_Prelude.C_False x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x16 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))))))) x3500) x3500) x3500) x3500) x3500) x3500))))))
     (Curry_AbstractCurry.Choice_C_CRules x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_159 x1 x2 x5 x7 x10 x1002 x3000 x3500) (nd_OP__case_159 x1 x2 x5 x7 x10 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CRules x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_159 x1 x2 x5 x7 x10 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CRules x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_159 x1 x2 x5 x7 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CRules x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_154 x10 x11 x3500 = case x11 of
     Curry_Prelude.C_True -> d_C_parens x10 x3500
     Curry_Prelude.C_False -> x10
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_154 x10 x1002 x3500) (d_OP__case_154 x10 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_154 x10 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_154 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_154 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> d_C_parens x10 x3500
     Curry_Prelude.C_False -> x10
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_154 x10 x1002 x3000 x3500) (nd_OP__case_154 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_154 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_154 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_155 x10 x13 x3500 = case x13 of
     Curry_Prelude.C_True -> d_C_parens x10 x3500
     Curry_Prelude.C_False -> x10
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_155 x10 x1002 x3500) (d_OP__case_155 x10 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_155 x10 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_155 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_155 x10 x13 x3000 x3500 = case x13 of
     Curry_Prelude.C_True -> d_C_parens x10 x3500
     Curry_Prelude.C_False -> x10
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_155 x10 x1002 x3000 x3500) (nd_OP__case_155 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_155 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_155 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_156 x1 x5 x10 x12 x14 x13 x3500 = case x13 of
     Curry_Prelude.C_True -> d_OP_showCmtFunc_dot_rulePrints_dot_142 x14 x10 x1 x12 x5 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus x10 (d_C_prefixInter (d_C_showRule x1) x12 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x10 x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_156 x1 x5 x10 x12 x14 x1002 x3500) (d_OP__case_156 x1 x5 x10 x12 x14 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_156 x1 x5 x10 x12 x14 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_156 x1 x5 x10 x12 x14 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_156 x1 x5 x10 x12 x14 x13 x3000 x3500 = case x13 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_showCmtFunc_dot_rulePrints_dot_142 x14 x10 x1 x12 x5 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus x10 (nd_C_prefixInter (wrapNX id (nd_C_showRule x1)) x12 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x10 x3500) x2000 x3500) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_156 x1 x5 x10 x12 x14 x1002 x3000 x3500) (nd_OP__case_156 x1 x5 x10 x12 x14 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_156 x1 x5 x10 x12 x14 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_156 x1 x5 x10 x12 x14 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_157 x7 x14 x15 x3500 = case x15 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus x14 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExpr Curry_Prelude.C_False x7 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_157 x7 x14 x1002 x3500) (d_OP__case_157 x7 x14 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_157 x7 x14 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_157 x7 x14 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_157 x7 x14 x15 x3000 x3500 = case x15 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus x14 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExpr Curry_Prelude.C_False x7 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_157 x7 x14 x1002 x3000 x3500) (nd_OP__case_157 x7 x14 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_157 x7 x14 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_157 x7 x14 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_158 x11 x14 x15 x3500 = case x15 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus x14 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (d_C_showEvalAnnot x11 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_158 x11 x14 x1002 x3500) (d_OP__case_158 x11 x14 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_158 x11 x14 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_158 x11 x14 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_158 x11 x14 x15 x3000 x3500 = case x15 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus x14 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (d_C_showEvalAnnot x11 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_158 x11 x14 x1002 x3000 x3500) (nd_OP__case_158 x11 x14 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_158 x11 x14 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_158 x11 x14 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_161 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) x3)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x2 x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_161 x2 x3 x1002 x3500) (d_OP__case_161 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_161 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_161 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_161 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) x3)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x2 x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_161 x2 x3 x1002 x3000 x3500) (nd_OP__case_161 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_161 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_161 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_164 x1 x9 x8 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x10 x11) -> d_OP__case_163 x1 x9 x10 x11 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x10 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_164 x1 x9 x1002 x3500) (d_OP__case_164 x1 x9 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_164 x1 x9 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_164 x1 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_164 x1 x9 x8 x3000 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_163 x1 x9 x10 x11 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x10 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_164 x1 x9 x1002 x3000 x3500) (nd_OP__case_164 x1 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_164 x1 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_164 x1 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_163 x1 x9 x10 x11 x12 x3500 = case x12 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_162 x1 x9 x10 x11 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_163 x1 x9 x10 x11 x1002 x3500) (d_OP__case_163 x1 x9 x10 x11 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_163 x1 x9 x10 x11 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_163 x1 x9 x10 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_163 x1 x9 x10 x11 x12 x3000 x3500 = case x12 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_162 x1 x9 x10 x11 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_163 x1 x9 x10 x11 x1002 x3000 x3500) (nd_OP__case_163 x1 x9 x10 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_163 x1 x9 x10 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_163 x1 x9 x10 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_162 x1 x9 x10 x11 x12 x3500 = case x12 of
     Curry_Prelude.C_True -> d_C_maybeShowBrackets (Curry_Prelude.d_OP_ampersand_ampersand x1 (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x9 x3500) x3500) x3500) (d_C_showTypeCons x10 x11 x9 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_162 x1 x9 x10 x11 x1002 x3500) (d_OP__case_162 x1 x9 x10 x11 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_162 x1 x9 x10 x11 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_162 x1 x9 x10 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_162 x1 x9 x10 x11 x12 x3000 x3500 = case x12 of
     Curry_Prelude.C_True -> d_C_maybeShowBrackets (Curry_Prelude.d_OP_ampersand_ampersand x1 (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x9 x3500) x3500) x3500) (d_C_showTypeCons x10 x11 x9 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_162 x1 x9 x10 x11 x1002 x3000 x3500) (nd_OP__case_162 x1 x9 x10 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_162 x1 x9 x10 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_162 x1 x9 x10 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_165 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_C_showTypeVar (Curry_Prelude.d_C_apply (d_C_showIdentifier x3500) x5 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_165 x1002 x3500) (d_OP__case_165 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_165 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_165 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_165 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (d_C_showTypeVar (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_showIdentifier x2000 x3500) x5 x2001 x3500)))) x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_165 x1002 x3000 x3500) (nd_OP__case_165 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_165 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_165 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_166 x5 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> Curry_Prelude.d_OP_plus_plus x7 (d_C_prefixMap (d_C_showTypeExpr Curry_Prelude.C_True) x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_166 x5 x1002 x3500) (d_OP__case_166 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_166 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_166 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_166 x5 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus x7 (nd_C_prefixMap (wrapDX id (d_C_showTypeExpr Curry_Prelude.C_True)) x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x2000 x3500) x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_166 x5 x1002 x3000 x3500) (nd_OP__case_166 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_166 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_166 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_167 x10 x11 x8 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x12 x13) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (d_C_prefixMap (d_C_showTypeExpr Curry_Prelude.C_False) (Curry_Prelude.d_C_map (acceptCs id Curry_AbstractCurry.C_CTVar) x10 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (d_C_showBlock (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_combineMap d_C_showConsDecl x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) x3500) x3500) x3500) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_167 x10 x11 x1002 x3500) (d_OP__case_167 x10 x11 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_167 x10 x11 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_167 x10 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_167 x10 x11 x8 x3000 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x12 x13) -> let
          x2004 = x3000
           in (seq x2004 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x13 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (Curry_Prelude.d_OP_plus_plus (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_prefixMap (wrapDX id (d_C_showTypeExpr Curry_Prelude.C_False)) (Curry_Prelude.nd_C_map (wrapDX id (acceptCs id Curry_AbstractCurry.C_CTVar)) x10 x2000 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x2001 x3500)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (d_C_showBlock (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (nd_C_combineMap (wrapDX id d_C_showConsDecl) x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) x2003 x3500) x3500) x3500) x3500) x3500)))) x3500) x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_167 x10 x11 x1002 x3000 x3500) (nd_OP__case_167 x10 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_167 x10 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_167 x10 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_168 x4 x5 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.d_OP_plus_plus (d_C_prefixMap (d_C_showTypeExpr Curry_Prelude.C_False) (Curry_Prelude.d_C_map (acceptCs id Curry_AbstractCurry.C_CTVar) x4 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (d_C_showTypeExpr Curry_Prelude.C_False x5 x3500) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_168 x4 x5 x1002 x3500) (d_OP__case_168 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_168 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_168 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_168 x4 x5 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_prefixMap (wrapDX id (d_C_showTypeExpr Curry_Prelude.C_False)) (Curry_Prelude.nd_C_map (wrapDX id (acceptCs id Curry_AbstractCurry.C_CTVar)) x4 x2000 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x2001 x3500)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (d_C_showTypeExpr Curry_Prelude.C_False x5 x3500) x3500) x3500) x3500) x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_168 x4 x5 x1002 x3000 x3500) (nd_OP__case_168 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_168 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_168 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_169 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_169 x1 x1002 x3500) (d_OP__case_169 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_169 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_169 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_169 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_169 x1 x1002 x3000 x3500) (nd_OP__case_169 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_169 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_169 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_171 x3 x4 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.d_OP_plus_plus (d_C_showFixity x3 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x4 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_OP__case_170 x6 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x6 x3500) x3500) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_171 x3 x4 x1002 x3500) (d_OP__case_171 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_171 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_171 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_171 x3 x4 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2004 = x3000
           in (seq x2004 (Curry_Prelude.d_OP_plus_plus (d_C_showFixity x3 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x4 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_170 x6 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x6 x2001 x3500)))) x2003 x3500)))) x3500) x3500) x3500) x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_171 x3 x4 x1002 x3000 x3500) (nd_OP__case_171 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_171 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_171 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_170 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> d_C_backQuotes x6 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_170 x6 x1002 x3500) (d_OP__case_170 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_170 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_170 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_170 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> d_C_backQuotes x6 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_170 x6 x1002 x3000 x3500) (nd_OP__case_170 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_170 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_170 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_172 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_172 x1 x1002 x3500) (d_OP__case_172 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_172 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_172 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_172 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_172 x1 x1002 x3000 x3500) (nd_OP__case_172 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_172 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_172 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_173 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) x1 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_173 x1 x1002 x3500) (d_OP__case_173 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_173 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_173 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_173 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) x1 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_173 x1 x1002 x3000 x3500) (nd_OP__case_173 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_173 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_173 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_174 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_174 x1 x1002 x3500) (d_OP__case_174 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_174 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_174 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_174 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_174 x1 x1002 x3000 x3500) (nd_OP__case_174 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_174 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_174 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_176 x10 x3500 = case x10 of
     (Curry_Prelude.OP_Tuple2 x15 x16) -> d_OP__case_175 x16 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x16 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_176 x1002 x3500) (d_OP__case_176 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_176 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_176 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_176 x10 x3000 x3500 = case x10 of
     (Curry_Prelude.OP_Tuple2 x15 x16) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_175 x16 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x16 x2001 x3500)))) x2003 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_176 x1002 x3000 x3500) (nd_OP__case_176 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_176 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_176 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_175 x16 x17 x3500 = case x17 of
     Curry_Prelude.C_True -> d_C_parens x16 x3500
     Curry_Prelude.C_False -> x16
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_175 x16 x1002 x3500) (d_OP__case_175 x16 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_175 x16 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_175 x16 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_175 x16 x17 x3000 x3500 = case x17 of
     Curry_Prelude.C_True -> d_C_parens x16 x3500
     Curry_Prelude.C_False -> x16
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_175 x16 x1002 x3000 x3500) (nd_OP__case_175 x16 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_175 x16 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_175 x16 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_178 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> d_OP__case_177 x8 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3500) x8 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_178 x1002 x3500) (d_OP__case_178 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_178 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_178 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_178 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_177 x8 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_isInfixOpName x2000 x3500) x8 x2001 x3500)))) x2003 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_178 x1002 x3000 x3500) (nd_OP__case_178 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_178 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_178 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_177 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_parens x8 x3500
     Curry_Prelude.C_False -> x8
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_177 x8 x1002 x3500) (d_OP__case_177 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_177 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_177 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_177 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_parens x8 x3500
     Curry_Prelude.C_False -> x8
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_177 x8 x1002 x3000 x3500) (nd_OP__case_177 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_177 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_177 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_179 x8 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x12 x13) -> x13
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_179 x1002 x3500) (d_OP__case_179 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_179 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_179 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_179 x8 x3000 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x12 x13) -> x13
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_179 x1002 x3000 x3500) (nd_OP__case_179 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_179 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_179 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_180 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> x7
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_180 x1002 x3500) (d_OP__case_180 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_180 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_180 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_180 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> x7
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_180 x1002 x3000 x3500) (nd_OP__case_180 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_180 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_180 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_181 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (d_C_parens x7 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_181 x7 x1002 x3500) (d_OP__case_181 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_181 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_181 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_181 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (d_C_parens x7 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_181 x7 x1002 x3000 x3500) (nd_OP__case_181 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_181 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_181 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
