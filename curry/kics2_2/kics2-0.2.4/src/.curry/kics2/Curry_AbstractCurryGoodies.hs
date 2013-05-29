{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AbstractCurryGoodies (d_C_isPolyType, d_C_isFunctionalType, d_C_isIOType, d_C_isIOReturnType, d_C_modsOfType, d_C_showMonoTypeExpr) where

import Basics
import qualified Curry_AbstractCurry
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_Utils
d_C_isPolyType :: Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.C_Bool
d_C_isPolyType x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CTVar x2) -> Curry_Prelude.C_True
     (Curry_AbstractCurry.C_CFuncType x3 x4) -> Curry_Prelude.d_OP_bar_bar (d_C_isPolyType x3 x3500) (d_C_isPolyType x4 x3500) x3500
     (Curry_AbstractCurry.C_CTCons x5 x6) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_C_isPolyType x3500) x6 x3500
     (Curry_AbstractCurry.C_CRecordType x7 x8) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_C_isPolyType x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x7 x3500) x3500
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isPolyType x1002 x3500) (d_C_isPolyType x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isPolyType z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isPolyType x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isFunctionalType :: Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.C_Bool
d_C_isFunctionalType x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CFuncType x2 x3) -> Curry_Prelude.C_True
     (Curry_AbstractCurry.C_CTVar x4) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CTCons x5 x6) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecordType x7 x8) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isFunctionalType x1002 x3500) (d_C_isFunctionalType x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isFunctionalType z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isFunctionalType x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isIOType :: Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.C_Bool
d_C_isIOType x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CTCons x2 x3) -> Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) Curry_Prelude.OP_List))) x3500
     (Curry_AbstractCurry.C_CTVar x4) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CFuncType x5 x6) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecordType x7 x8) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isIOType x1002 x3500) (d_C_isIOType x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isIOType z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isIOType x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isIOReturnType :: Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.C_Bool
d_C_isIOReturnType x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CTVar x2) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CFuncType x3 x4) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CTCons x5 x6) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) Curry_Prelude.OP_List))) x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_head x6 x3500) (Curry_AbstractCurry.C_CTCons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))) Curry_Prelude.OP_List) x3500) (Curry_Prelude.d_C_not (d_C_isFunctionalType (Curry_Prelude.d_C_head x6 x3500) x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CRecordType x7 x8) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isIOReturnType x1002 x3500) (d_C_isIOReturnType x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isIOReturnType z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isIOReturnType x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_modsOfType :: Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_modsOfType x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CTVar x2) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CFuncType x3 x4) -> Curry_List.d_C_union (d_C_modsOfType x3 x3500) (d_C_modsOfType x4 x3500) x3500
     (Curry_AbstractCurry.C_CTCons x5 x6) -> d_OP__case_12 x6 x5 x3500
     (Curry_AbstractCurry.C_CRecordType x9 x10) -> Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_foldr (acceptCs id Curry_List.d_C_union) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot d_C_modsOfType Curry_Prelude.d_C_snd x3500) x9 x3500) x3500
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_modsOfType x1002 x3500) (d_C_modsOfType x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_modsOfType z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_modsOfType x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showMonoTypeExpr :: Curry_Prelude.C_Bool -> Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showMonoTypeExpr x1 x2 x3500 = d_C_showMonoTypeExpr' x1 Curry_Prelude.C_False x2 x3500

d_C_showMonoTypeExpr' :: Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showMonoTypeExpr' x1 x2 x3 x3500 = case x3 of
     (Curry_AbstractCurry.C_CTVar x4) -> d_OP__case_11 x1 x4 x3500
     (Curry_AbstractCurry.C_CFuncType x7 x8) -> Curry_Prelude.d_OP_dollar (d_C_parens x2) (Curry_Prelude.d_OP_plus_plus (d_C_showMonoTypeExpr' x1 (d_C_isFunctionalType x7 x3500) x7 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (d_C_showMonoTypeExpr' x1 Curry_Prelude.C_False x8 x3500) x3500) x3500) x3500
     (Curry_AbstractCurry.C_CTCons x9 x10) -> d_OP__case_9 x1 x2 x10 x9 x3500
     (Curry_AbstractCurry.C_CRecordType x13 x14) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.d_OP_plus_plus (Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_C_map (d_C_showField x1 x2) x13 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3500)
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showMonoTypeExpr' x1 x2 x1002 x3500) (d_C_showMonoTypeExpr' x1 x2 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showMonoTypeExpr' x1 x2 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showMonoTypeExpr' x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showTypeCons :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTypeCons x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP__case_6 x1 x2 x3 x4 (Curry_Prelude.d_OP_eq_eq x2 (d_C_prelude x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showTypeCons x1 x2 x3 x1002 x3500) (d_C_showTypeCons x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showTypeCons x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showTypeCons x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showPreludeTypeCons :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showPreludeTypeCons x1 x2 x3 x3500 = d_OP__case_4 x1 x2 x3 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x3 x3500) (Curry_AbstractCurry.C_CTCons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List) x3500) x3500) x3500

d_C_showField :: Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showField x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (d_C_showMonoTypeExpr' x1 x2 x5 x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showField x1 x2 x1002 x3500) (d_C_showField x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showField x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showField x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showIdentifier :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showIdentifier x3500 = Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip Curry_Prelude.d_C_notElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)))

nd_C_showIdentifier :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_showIdentifier x3000 x3500 = wrapNX id (Curry_Prelude.nd_C_filter (wrapNX id (Curry_Prelude.nd_C_flip (wrapNX id Curry_Prelude.nd_C_notElem) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)))))

d_C_prelude :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_prelude x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))

d_C_parens :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_parens x1 x2 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500)
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_parens x1002 x2 x3500) (d_C_parens x1003 x2 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_parens z x2 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_parens x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_prefixMap :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> ConstStore -> Curry_Prelude.OP_List t1) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t1 -> ConstStore -> Curry_Prelude.OP_List t1
d_C_prefixMap x1 x2 x3 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_plus_plus x3) x3500) (Curry_Prelude.d_C_map x1 x2 x3500) x3500

nd_C_prefixMap :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Curry_Prelude.OP_List t1) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t1 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t1
nd_C_prefixMap x1 x2 x3 x3000 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id (Curry_Prelude.d_OP_plus_plus x3)) x2000 x3500) (Curry_Prelude.nd_C_map x1 x2 x2001 x3500) x2002 x3500))))))))

d_C_combineMap :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> ConstStore -> Curry_Prelude.OP_List t1) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t1 -> ConstStore -> Curry_Prelude.OP_List t1
d_C_combineMap x1 x2 x3 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x4 x3500) (d_C_prefixMap x1 x5 x3 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_combineMap x1 x1002 x3 x3500) (d_C_combineMap x1 x1003 x3 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_combineMap x1 z x3 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_combineMap x1 x1002 x3) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_combineMap :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Curry_Prelude.OP_List t1) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t1 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t1
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

d_C_isTuple :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_C_isTuple x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '('#) x3500) (d_OP_isTuple_dot_p1_isTuple_dot_97 x3 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isTuple x1002 x3500) (d_C_isTuple x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isTuple z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isTuple x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_isTuple_dot_p1_isTuple_dot_97 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isTuple_dot_p1_isTuple_dot_97 x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_0 x2 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isTuple_dot_p1_isTuple_dot_97 x1002 x3500) (d_OP_isTuple_dot_p1_isTuple_dot_97 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isTuple_dot_p1_isTuple_dot_97 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isTuple_dot_p1_isTuple_dot_97 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char ')'#) x3500
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char ','#) x3500) (d_OP_isTuple_dot_p1_isTuple_dot_97 (Curry_Prelude.OP_Cons x4 x5) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x1002 x3500) (d_OP__case_0 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char ')'#) x3500
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char ','#) x3500) (d_OP_isTuple_dot_p1_isTuple_dot_97 (Curry_Prelude.OP_Cons x4 x5) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x2 x1002 x3000 x3500) (nd_OP__case_0 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List)))))
     Curry_Prelude.C_False -> d_OP__case_3 x1 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x2 x3 x1002 x3500) (d_OP__case_4 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x1 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_4 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showMonoTypeExpr' x1 Curry_Prelude.C_False (Curry_Prelude.d_C_head x3 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_2 x1 x2 x3 (d_C_isTuple x2 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x2 x3 x1002 x3500) (d_OP__case_3 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showMonoTypeExpr' x1 Curry_Prelude.C_False (Curry_Prelude.d_C_head x3 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x1 x2 x3 (d_C_isTuple x2 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_3 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_combineMap (d_C_showMonoTypeExpr' x1 Curry_Prelude.C_False) x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_1 x1 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x2 x3 x1002 x3500) (d_OP__case_2 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (nd_C_combineMap (wrapDX id (d_C_showMonoTypeExpr' x1 Curry_Prelude.C_False)) x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) x2000 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x1 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_2 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x2 (d_C_prefixMap (d_C_showMonoTypeExpr' x1 Curry_Prelude.C_True) x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x2 x3 x1002 x3500) (d_OP__case_1 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus x2 (nd_C_prefixMap (wrapDX id (d_C_showMonoTypeExpr' x1 Curry_Prelude.C_True)) x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x2000 x3500) x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_1 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x1 x2 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> d_C_showPreludeTypeCons x1 x3 x4 x3500
     Curry_Prelude.C_False -> d_OP__case_5 x1 x3 x4 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x2 x3 x4 x1002 x3500) (d_OP__case_6 x1 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x1 x2 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> d_C_showPreludeTypeCons x1 x3 x4 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x1 x3 x4 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_6 x1 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x1 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x1 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x3 (d_C_prefixMap (d_C_showMonoTypeExpr' x1 Curry_Prelude.C_True) x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x3 x4 x1002 x3500) (d_OP__case_5 x1 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x1 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus x3 (nd_C_prefixMap (wrapDX id (d_C_showMonoTypeExpr' x1 Curry_Prelude.C_True)) x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x2000 x3500) x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_5 x1 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x1 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x1 x2 x10 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> d_OP__case_8 x1 x2 x10 x11 x12 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x11 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x2 x10 x1002 x3500) (d_OP__case_9 x1 x2 x10 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 x2 x10 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x2 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x1 x2 x10 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x1 x2 x10 x11 x12 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x11 (d_C_prelude x3500) x3500) (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x2 x10 x1002 x3000 x3500) (nd_OP__case_9 x1 x2 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 x2 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x2 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x1 x2 x10 x11 x12 x13 x3500 = case x13 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_7 x1 x2 x10 x11 x12 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x2 x10 x11 x12 x1002 x3500) (d_OP__case_8 x1 x2 x10 x11 x12 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 x2 x10 x11 x12 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x2 x10 x11 x12 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x1 x2 x10 x11 x12 x13 x3000 x3500 = case x13 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x1 x2 x10 x11 x12 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1 x2 x10 x11 x12 x1002 x3000 x3500) (nd_OP__case_8 x1 x2 x10 x11 x12 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x1 x2 x10 x11 x12 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1 x2 x10 x11 x12 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x1 x2 x10 x11 x12 x13 x3500 = case x13 of
     Curry_Prelude.C_True -> d_C_parens (Curry_Prelude.d_OP_ampersand_ampersand x2 (Curry_Prelude.d_C_apply (Curry_Utils.d_C_notNull x3500) x10 x3500) x3500) (d_C_showTypeCons x1 x11 x12 x10 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x2 x10 x11 x12 x1002 x3500) (d_OP__case_7 x1 x2 x10 x11 x12 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 x2 x10 x11 x12 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x2 x10 x11 x12 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x1 x2 x10 x11 x12 x13 x3000 x3500 = case x13 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (d_C_parens (Curry_Prelude.d_OP_ampersand_ampersand x2 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Utils.nd_C_notNull x2000 x3500) x10 x2001 x3500)))) x3500) (d_C_showTypeCons x1 x11 x12 x10 x3500) x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1 x2 x10 x11 x12 x1002 x3000 x3500) (nd_OP__case_7 x1 x2 x10 x11 x12 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x1 x2 x10 x11 x12 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1 x2 x10 x11 x12 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x1 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_10 x6 x1 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1 x1002 x3500) (d_OP__case_11 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x1 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x6 x1 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1 x1002 x3000 x3500) (nd_OP__case_11 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x6 x1 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (d_C_showIdentifier x3500) x6 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x6 x1002 x3500) (d_OP__case_10 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x6 x1 x3000 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_showIdentifier x2000 x3500) x6 x2001 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x6 x1002 x3000 x3500) (nd_OP__case_10 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x6 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_foldr (acceptCs id Curry_List.d_C_union) (Curry_Prelude.OP_Cons x7 Curry_Prelude.OP_List)) (Curry_Prelude.d_C_map d_C_modsOfType x6 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x6 x1002 x3500) (d_OP__case_12 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x6 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapDX id) (acceptCs id Curry_List.d_C_union)) (Curry_Prelude.OP_Cons x7 Curry_Prelude.OP_List))) (Curry_Prelude.nd_C_map (wrapDX id d_C_modsOfType) x6 x2000 x3500) x2001 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x6 x1002 x3000 x3500) (nd_OP__case_12 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
