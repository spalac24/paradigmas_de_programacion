{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AbstractHaskellGoodies (d_C_lowerFirst, d_C_applyF, d_C_constF, d_C_applyV, d_C_tuplePat, d_C_tupleExpr, d_C_tuplePattern, d_OP_tilde_gt, d_C_baseType, d_C_listType, d_C_tupleType, d_C_ioType, d_C_maybeType, d_C_stringType, d_C_intType, d_C_boolType, d_C_dateType, d_C_tfunc, d_C_ctfunc, d_C_ufunc, d_C_cmtfunc, d_C_string2ac, d_C_noGuard, d_C_pre, d_C_cvar, d_C_clet, d_C_ctvar, d_C_list2ac, d_C_renameSymbolInProg, nd_C_renameSymbolInProg, d_C_renameSymbolInTypeDecl, nd_C_renameSymbolInTypeDecl, d_C_renameSymbolInConsDecl, nd_C_renameSymbolInConsDecl, d_C_renameSymbolInTypeExpr, nd_C_renameSymbolInTypeExpr, d_C_renameSymbolInExpr, nd_C_renameSymbolInExpr, d_C_renameSymbolInPat, nd_C_renameSymbolInPat, d_C_renameSymbolInBranch, nd_C_renameSymbolInBranch, d_C_renameSymbolInStat, nd_C_renameSymbolInStat, d_C_renameSymbolInLocal, nd_C_renameSymbolInLocal, d_C_renameSymbolInTypeSig, nd_C_renameSymbolInTypeSig, d_C_renameSymbolInFunc, nd_C_renameSymbolInFunc, d_C_renameSymbolInRules, nd_C_renameSymbolInRules, d_C_renameSymbolInRule, nd_C_renameSymbolInRule, d_C_renameOpDecl, nd_C_renameOpDecl, d_C_funcDecls, d_C_funcName, d_C_typeOf, d_C_commentOf) where

import Basics
import qualified Curry_AbstractHaskell
import qualified Curry_Char
import qualified Curry_Prelude
d_C_lowerFirst :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_lowerFirst x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.OP_Cons (Curry_Char.d_C_toLower x2 x3500) x3
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_lowerFirst x1002 x3500) (d_C_lowerFirst x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_lowerFirst z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_lowerFirst x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_applyF :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Expr -> ConstStore -> Curry_AbstractHaskell.C_Expr
d_C_applyF x1 x2 x3500 = Curry_Prelude.d_C_foldl (acceptCs (acceptCs id) Curry_AbstractHaskell.C_Apply) (Curry_AbstractHaskell.C_Symbol x1) x2 x3500

d_C_constF :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_AbstractHaskell.C_Expr
d_C_constF x1 x3500 = d_C_applyF x1 Curry_Prelude.OP_List x3500

d_C_applyV :: Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Expr -> ConstStore -> Curry_AbstractHaskell.C_Expr
d_C_applyV x1 x2 x3500 = Curry_Prelude.d_C_foldl (acceptCs (acceptCs id) Curry_AbstractHaskell.C_Apply) (Curry_AbstractHaskell.C_Var x1) x2 x3500

d_C_tuplePat :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> ConstStore -> Curry_AbstractHaskell.C_Pattern
d_C_tuplePat x1 x3500 = let
     x2 = Curry_Prelude.d_C_length x1 x3500
      in (d_OP__case_12 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 0#) x3500) x3500)

d_C_tupleExpr :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Expr -> ConstStore -> Curry_AbstractHaskell.C_Expr
d_C_tupleExpr x1 x3500 = let
     x2 = Curry_Prelude.d_C_length x1 x3500
      in (d_OP__case_9 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 0#) x3500) x3500)

d_C_tuplePattern :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> ConstStore -> Curry_AbstractHaskell.C_Pattern
d_C_tuplePattern x1 x3500 = let
     x2 = Curry_Prelude.d_C_length x1 x3500
      in (d_OP__case_6 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 0#) x3500) x3500)

d_OP_tilde_gt :: Curry_AbstractHaskell.C_TypeExpr -> Curry_AbstractHaskell.C_TypeExpr -> ConstStore -> Curry_AbstractHaskell.C_TypeExpr
d_OP_tilde_gt x1 x2 x3500 = Curry_AbstractHaskell.C_FuncType x1 x2

d_C_baseType :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_AbstractHaskell.C_TypeExpr
d_C_baseType x1 x3500 = Curry_AbstractHaskell.C_TCons x1 Curry_Prelude.OP_List

d_C_listType :: Curry_AbstractHaskell.C_TypeExpr -> ConstStore -> Curry_AbstractHaskell.C_TypeExpr
d_C_listType x1 x3500 = Curry_AbstractHaskell.C_TCons (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List)

d_C_tupleType :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeExpr -> ConstStore -> Curry_AbstractHaskell.C_TypeExpr
d_C_tupleType x1 x3500 = let
     x2 = Curry_Prelude.d_C_length x1 x3500
      in (d_OP__case_3 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 0#) x3500) x3500)

d_C_ioType :: Curry_AbstractHaskell.C_TypeExpr -> ConstStore -> Curry_AbstractHaskell.C_TypeExpr
d_C_ioType x1 x3500 = Curry_AbstractHaskell.C_TCons (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) Curry_Prelude.OP_List)) x3500) (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List)

d_C_maybeType :: Curry_AbstractHaskell.C_TypeExpr -> ConstStore -> Curry_AbstractHaskell.C_TypeExpr
d_C_maybeType x1 x3500 = Curry_AbstractHaskell.C_TCons (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) x3500) (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List)

d_C_stringType :: ConstStore -> Curry_AbstractHaskell.C_TypeExpr
d_C_stringType x3500 = d_C_baseType (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List)))))) x3500) x3500

d_C_intType :: ConstStore -> Curry_AbstractHaskell.C_TypeExpr
d_C_intType x3500 = d_C_baseType (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))) x3500) x3500

d_C_boolType :: ConstStore -> Curry_AbstractHaskell.C_TypeExpr
d_C_boolType x3500 = d_C_baseType (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3500) x3500

d_C_dateType :: ConstStore -> Curry_AbstractHaskell.C_TypeExpr
d_C_dateType x3500 = d_C_baseType (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))) x3500

d_C_tfunc :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_AbstractHaskell.C_Visibility -> Curry_AbstractHaskell.C_TypeExpr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Rule -> ConstStore -> Curry_AbstractHaskell.C_FuncDecl
d_C_tfunc x1 x2 x3 x4 x5 x3500 = Curry_AbstractHaskell.C_Func Curry_Prelude.OP_List x1 x2 x3 (Curry_AbstractHaskell.C_FType x4) (Curry_AbstractHaskell.C_Rules x5)

d_C_ctfunc :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_AbstractHaskell.C_Visibility -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Context -> Curry_AbstractHaskell.C_TypeExpr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Rule -> ConstStore -> Curry_AbstractHaskell.C_FuncDecl
d_C_ctfunc x1 x2 x3 x4 x5 x6 x3500 = Curry_AbstractHaskell.C_Func Curry_Prelude.OP_List x1 x2 x3 (Curry_AbstractHaskell.C_CType x4 x5) (Curry_AbstractHaskell.C_Rules x6)

d_C_ufunc :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_AbstractHaskell.C_Visibility -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Rule -> ConstStore -> Curry_AbstractHaskell.C_FuncDecl
d_C_ufunc x1 x2 x3 x4 x3500 = Curry_AbstractHaskell.C_Func Curry_Prelude.OP_List x1 x2 x3 Curry_AbstractHaskell.C_Untyped (Curry_AbstractHaskell.C_Rules x4)

d_C_cmtfunc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_AbstractHaskell.C_Visibility -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Context -> Curry_AbstractHaskell.C_TypeExpr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Rule -> ConstStore -> Curry_AbstractHaskell.C_FuncDecl
d_C_cmtfunc x1 x2 x3 x4 x5 x6 x7 x3500 = Curry_AbstractHaskell.C_Func x1 x2 x3 x4 (Curry_AbstractHaskell.C_CType x5 x6) (Curry_AbstractHaskell.C_Rules x7)

d_C_string2ac :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_AbstractHaskell.C_Expr
d_C_string2ac x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> d_C_constF (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) x3500
     (Curry_Prelude.OP_Cons x2 x3) -> d_C_applyF (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x3500) (Curry_Prelude.OP_Cons (Curry_AbstractHaskell.C_Lit (Curry_AbstractHaskell.C_Charc x2)) (Curry_Prelude.OP_Cons (d_C_string2ac x3 x3500) Curry_Prelude.OP_List)) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_string2ac x1002 x3500) (d_C_string2ac x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_string2ac z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_string2ac x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_noGuard :: Curry_AbstractHaskell.C_Expr -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_AbstractHaskell.C_Expr Curry_AbstractHaskell.C_Expr
d_C_noGuard x1 x3500 = Curry_Prelude.OP_Tuple2 (Curry_AbstractHaskell.C_Symbol (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))) x3500)) x1

d_C_pre :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_pre x1 x3500 = Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x1

d_C_cvar :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_AbstractHaskell.C_Expr
d_C_cvar x1 x3500 = Curry_AbstractHaskell.C_Var (Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Int 1#) x1)

d_C_clet :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_LocalDecl -> Curry_AbstractHaskell.C_Expr -> ConstStore -> Curry_AbstractHaskell.C_Expr
d_C_clet x1 x2 x3500 = d_OP__case_0 x1 x2 (Curry_Prelude.d_C_null x1 x3500) x3500

d_C_ctvar :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_AbstractHaskell.C_TypeExpr
d_C_ctvar x1 x3500 = Curry_AbstractHaskell.C_TVar (Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Int 1#) x1)

d_C_list2ac :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Expr -> ConstStore -> Curry_AbstractHaskell.C_Expr
d_C_list2ac x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> d_C_applyF (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3500) Curry_Prelude.OP_List x3500
     (Curry_Prelude.OP_Cons x2 x3) -> d_C_applyF (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x3500) (Curry_Prelude.OP_Cons x2 (Curry_Prelude.OP_Cons (d_C_list2ac x3 x3500) Curry_Prelude.OP_List)) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_list2ac x1002 x3500) (d_C_list2ac x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_list2ac z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_list2ac x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_renameSymbolInProg :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Prog -> ConstStore -> Curry_AbstractHaskell.C_Prog
d_C_renameSymbolInProg x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Prog x3 x4 x5 x6 x7) -> Curry_AbstractHaskell.C_Prog (Curry_Prelude.d_C_fst (Curry_Prelude.d_C_apply x1 (Curry_Prelude.OP_Tuple2 x3 Curry_Prelude.OP_List) x3500) x3500) (Curry_Prelude.d_C_map (d_OP_renameSymbolInProg_dot___hash_lambda1 x1) x4 x3500) (Curry_Prelude.d_C_map (d_C_renameSymbolInTypeDecl x1) x5 x3500) (Curry_Prelude.d_C_map (d_C_renameSymbolInFunc x1) x6 x3500) (Curry_Prelude.d_C_map (d_C_renameOpDecl x1) x7 x3500)
     (Curry_AbstractHaskell.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameSymbolInProg x1 x1002 x3500) (d_C_renameSymbolInProg x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameSymbolInProg x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameSymbolInProg x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameSymbolInProg :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Prog -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_Prog
nd_C_renameSymbolInProg x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Prog x3 x4 x5 x6 x7) -> let
          x2005 = x3000
           in (seq x2005 (let
               x2006 = leftSupply x2005
               x2007 = rightSupply x2005
                in (seq x2006 (seq x2007 (let
                    x2000 = leftSupply x2006
                    x2001 = rightSupply x2006
                     in (seq x2000 (seq x2001 (let
                         x2002 = leftSupply x2007
                         x2008 = rightSupply x2007
                          in (seq x2002 (seq x2008 (let
                              x2003 = leftSupply x2008
                              x2004 = rightSupply x2008
                               in (seq x2003 (seq x2004 (Curry_AbstractHaskell.C_Prog (Curry_Prelude.d_C_fst (Curry_Prelude.nd_C_apply x1 (Curry_Prelude.OP_Tuple2 x3 Curry_Prelude.OP_List) x2000 x3500) x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_renameSymbolInProg_dot___hash_lambda1 x1)) x4 x2001 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInTypeDecl x1)) x5 x2002 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInFunc x1)) x6 x2003 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameOpDecl x1)) x7 x2004 x3500)))))))))))))))
     (Curry_AbstractHaskell.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameSymbolInProg x1 x1002 x3000 x3500) (nd_C_renameSymbolInProg x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameSymbolInProg x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameSymbolInProg x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_renameSymbolInProg_dot___hash_lambda1 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_renameSymbolInProg_dot___hash_lambda1 x1 x2 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_fst (Curry_Prelude.d_C_apply x1 (Curry_Prelude.OP_Tuple2 x2 Curry_Prelude.OP_List) x3500) x3500

nd_OP_renameSymbolInProg_dot___hash_lambda1 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_renameSymbolInProg_dot___hash_lambda1 x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_fst) (Curry_Prelude.nd_C_apply x1 (Curry_Prelude.OP_Tuple2 x2 Curry_Prelude.OP_List) x2000 x3500) x2001 x3500)))))

d_C_renameSymbolInTypeDecl :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_TypeDecl -> ConstStore -> Curry_AbstractHaskell.C_TypeDecl
d_C_renameSymbolInTypeDecl x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Type x3 x4 x5 x6) -> Curry_AbstractHaskell.C_Type (Curry_Prelude.d_C_apply x1 x3 x3500) x4 x5 (Curry_Prelude.d_C_map (d_C_renameSymbolInConsDecl x1) x6 x3500)
     (Curry_AbstractHaskell.C_TypeSyn x7 x8 x9 x10) -> Curry_AbstractHaskell.C_TypeSyn (Curry_Prelude.d_C_apply x1 x7 x3500) x8 x9 (d_C_renameSymbolInTypeExpr x1 x10 x3500)
     (Curry_AbstractHaskell.C_Instance x11 x12 x13 x14) -> Curry_AbstractHaskell.C_Instance (Curry_Prelude.d_C_apply x1 x11 x3500) (d_C_renameSymbolInTypeExpr x1 x12 x3500) (Curry_Prelude.d_C_map (d_OP_renameSymbolInTypeDecl_dot___hash_lambda3 x1) x13 x3500) (Curry_Prelude.d_C_map (d_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 x1) x14 x3500)
     (Curry_AbstractHaskell.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameSymbolInTypeDecl x1 x1002 x3500) (d_C_renameSymbolInTypeDecl x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameSymbolInTypeDecl x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameSymbolInTypeDecl x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameSymbolInTypeDecl :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_TypeDecl -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_TypeDecl
nd_C_renameSymbolInTypeDecl x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Type x3 x4 x5 x6) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_Type (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) x4 x5 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInConsDecl x1)) x6 x2001 x3500))))))
     (Curry_AbstractHaskell.C_TypeSyn x7 x8 x9 x10) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_TypeSyn (Curry_Prelude.nd_C_apply x1 x7 x2000 x3500) x8 x9 (nd_C_renameSymbolInTypeExpr x1 x10 x2001 x3500))))))
     (Curry_AbstractHaskell.C_Instance x11 x12 x13 x14) -> let
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
                          in (seq x2002 (seq x2003 (Curry_AbstractHaskell.C_Instance (Curry_Prelude.nd_C_apply x1 x11 x2000 x3500) (nd_C_renameSymbolInTypeExpr x1 x12 x2001 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_renameSymbolInTypeDecl_dot___hash_lambda3 x1)) x13 x2002 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 x1)) x14 x2003 x3500))))))))))))
     (Curry_AbstractHaskell.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameSymbolInTypeDecl x1 x1002 x3000 x3500) (nd_C_renameSymbolInTypeDecl x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameSymbolInTypeDecl x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameSymbolInTypeDecl x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_AbstractHaskell.C_Rule -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_AbstractHaskell.C_Rule
d_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply x1 x3 x3500) (d_C_renameSymbolInRule x1 x4 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 x1 x1002 x3500) (d_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_AbstractHaskell.C_Rule -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_AbstractHaskell.C_Rule
nd_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) (nd_C_renameSymbolInRule x1 x4 x2001 x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 x1 x1002 x3000 x3500) (nd_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_renameSymbolInTypeDecl_dot_renameSymbolInInstRule_dot_85 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_renameSymbolInTypeDecl_dot___hash_lambda3 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Context -> ConstStore -> Curry_AbstractHaskell.C_Context
d_OP_renameSymbolInTypeDecl_dot___hash_lambda3 x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Context x3 x4) -> Curry_AbstractHaskell.C_Context (Curry_Prelude.d_C_apply x1 x3 x3500) x4
     (Curry_AbstractHaskell.Choice_C_Context x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_renameSymbolInTypeDecl_dot___hash_lambda3 x1 x1002 x3500) (d_OP_renameSymbolInTypeDecl_dot___hash_lambda3 x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Context x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_renameSymbolInTypeDecl_dot___hash_lambda3 x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Context x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_renameSymbolInTypeDecl_dot___hash_lambda3 x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Context x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_renameSymbolInTypeDecl_dot___hash_lambda3 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Context -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_Context
nd_OP_renameSymbolInTypeDecl_dot___hash_lambda3 x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Context x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AbstractHaskell.C_Context (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) x4))
     (Curry_AbstractHaskell.Choice_C_Context x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_renameSymbolInTypeDecl_dot___hash_lambda3 x1 x1002 x3000 x3500) (nd_OP_renameSymbolInTypeDecl_dot___hash_lambda3 x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_Context x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_renameSymbolInTypeDecl_dot___hash_lambda3 x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Context x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_renameSymbolInTypeDecl_dot___hash_lambda3 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Context x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_renameSymbolInConsDecl :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_ConsDecl -> ConstStore -> Curry_AbstractHaskell.C_ConsDecl
d_C_renameSymbolInConsDecl x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Cons x3 x4 x5 x6) -> Curry_AbstractHaskell.C_Cons (Curry_Prelude.d_C_apply x1 x3 x3500) x4 x5 (Curry_Prelude.d_C_map (d_C_renameSymbolInTypeExpr x1) x6 x3500)
     (Curry_AbstractHaskell.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameSymbolInConsDecl x1 x1002 x3500) (d_C_renameSymbolInConsDecl x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameSymbolInConsDecl x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameSymbolInConsDecl x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameSymbolInConsDecl :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_ConsDecl -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_ConsDecl
nd_C_renameSymbolInConsDecl x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Cons x3 x4 x5 x6) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_Cons (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) x4 x5 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInTypeExpr x1)) x6 x2001 x3500))))))
     (Curry_AbstractHaskell.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameSymbolInConsDecl x1 x1002 x3000 x3500) (nd_C_renameSymbolInConsDecl x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameSymbolInConsDecl x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameSymbolInConsDecl x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_renameSymbolInTypeExpr :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_TypeExpr -> ConstStore -> Curry_AbstractHaskell.C_TypeExpr
d_C_renameSymbolInTypeExpr x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_TCons x3 x4) -> Curry_AbstractHaskell.C_TCons (Curry_Prelude.d_C_apply x1 x3 x3500) (Curry_Prelude.d_C_map (d_C_renameSymbolInTypeExpr x1) x4 x3500)
     (Curry_AbstractHaskell.C_FuncType x5 x6) -> Curry_AbstractHaskell.C_FuncType (d_C_renameSymbolInTypeExpr x1 x5 x3500) (d_C_renameSymbolInTypeExpr x1 x6 x3500)
     (Curry_AbstractHaskell.C_TVar x7) -> Curry_AbstractHaskell.C_TVar x7
     (Curry_AbstractHaskell.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameSymbolInTypeExpr x1 x1002 x3500) (d_C_renameSymbolInTypeExpr x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameSymbolInTypeExpr x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameSymbolInTypeExpr x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameSymbolInTypeExpr :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_TypeExpr -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_TypeExpr
nd_C_renameSymbolInTypeExpr x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_TCons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_TCons (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInTypeExpr x1)) x4 x2001 x3500))))))
     (Curry_AbstractHaskell.C_FuncType x5 x6) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_FuncType (nd_C_renameSymbolInTypeExpr x1 x5 x2000 x3500) (nd_C_renameSymbolInTypeExpr x1 x6 x2001 x3500))))))
     (Curry_AbstractHaskell.C_TVar x7) -> Curry_AbstractHaskell.C_TVar x7
     (Curry_AbstractHaskell.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameSymbolInTypeExpr x1 x1002 x3000 x3500) (nd_C_renameSymbolInTypeExpr x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameSymbolInTypeExpr x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameSymbolInTypeExpr x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_renameSymbolInExpr :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Expr -> ConstStore -> Curry_AbstractHaskell.C_Expr
d_C_renameSymbolInExpr x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Symbol x3) -> Curry_AbstractHaskell.C_Symbol (Curry_Prelude.d_C_apply x1 x3 x3500)
     (Curry_AbstractHaskell.C_Apply x4 x5) -> Curry_AbstractHaskell.C_Apply (d_C_renameSymbolInExpr x1 x4 x3500) (d_C_renameSymbolInExpr x1 x5 x3500)
     (Curry_AbstractHaskell.C_Lambda x6 x7) -> Curry_AbstractHaskell.C_Lambda (Curry_Prelude.d_C_map (d_C_renameSymbolInPat x1) x6 x3500) (d_C_renameSymbolInExpr x1 x7 x3500)
     (Curry_AbstractHaskell.C_Let x8 x9) -> Curry_AbstractHaskell.C_Let (Curry_Prelude.d_C_map (d_C_renameSymbolInLocal x1) x8 x3500) (d_C_renameSymbolInExpr x1 x9 x3500)
     (Curry_AbstractHaskell.C_DoExpr x10) -> Curry_AbstractHaskell.C_DoExpr (Curry_Prelude.d_C_map (d_C_renameSymbolInStat x1) x10 x3500)
     (Curry_AbstractHaskell.C_ListComp x11 x12) -> Curry_AbstractHaskell.C_ListComp (d_C_renameSymbolInExpr x1 x11 x3500) (Curry_Prelude.d_C_map (d_C_renameSymbolInStat x1) x12 x3500)
     (Curry_AbstractHaskell.C_Case x13 x14) -> Curry_AbstractHaskell.C_Case (d_C_renameSymbolInExpr x1 x13 x3500) (Curry_Prelude.d_C_map (d_C_renameSymbolInBranch x1) x14 x3500)
     (Curry_AbstractHaskell.C_Typed x15 x16) -> Curry_AbstractHaskell.C_Typed (d_C_renameSymbolInExpr x1 x15 x3500) x16
     (Curry_AbstractHaskell.C_Var x17) -> x2
     (Curry_AbstractHaskell.C_Lit x18) -> x2
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameSymbolInExpr x1 x1002 x3500) (d_C_renameSymbolInExpr x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameSymbolInExpr x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameSymbolInExpr x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameSymbolInExpr :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Expr -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_Expr
nd_C_renameSymbolInExpr x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Symbol x3) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AbstractHaskell.C_Symbol (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500)))
     (Curry_AbstractHaskell.C_Apply x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_Apply (nd_C_renameSymbolInExpr x1 x4 x2000 x3500) (nd_C_renameSymbolInExpr x1 x5 x2001 x3500))))))
     (Curry_AbstractHaskell.C_Lambda x6 x7) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_Lambda (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInPat x1)) x6 x2000 x3500) (nd_C_renameSymbolInExpr x1 x7 x2001 x3500))))))
     (Curry_AbstractHaskell.C_Let x8 x9) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_Let (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInLocal x1)) x8 x2000 x3500) (nd_C_renameSymbolInExpr x1 x9 x2001 x3500))))))
     (Curry_AbstractHaskell.C_DoExpr x10) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AbstractHaskell.C_DoExpr (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInStat x1)) x10 x2000 x3500)))
     (Curry_AbstractHaskell.C_ListComp x11 x12) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_ListComp (nd_C_renameSymbolInExpr x1 x11 x2000 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInStat x1)) x12 x2001 x3500))))))
     (Curry_AbstractHaskell.C_Case x13 x14) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_Case (nd_C_renameSymbolInExpr x1 x13 x2000 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInBranch x1)) x14 x2001 x3500))))))
     (Curry_AbstractHaskell.C_Typed x15 x16) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AbstractHaskell.C_Typed (nd_C_renameSymbolInExpr x1 x15 x2000 x3500) x16))
     (Curry_AbstractHaskell.C_Var x17) -> x2
     (Curry_AbstractHaskell.C_Lit x18) -> x2
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameSymbolInExpr x1 x1002 x3000 x3500) (nd_C_renameSymbolInExpr x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameSymbolInExpr x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameSymbolInExpr x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_renameSymbolInPat :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Pattern -> ConstStore -> Curry_AbstractHaskell.C_Pattern
d_C_renameSymbolInPat x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_PComb x3 x4) -> Curry_AbstractHaskell.C_PComb (Curry_Prelude.d_C_apply x1 x3 x3500) (Curry_Prelude.d_C_map (d_C_renameSymbolInPat x1) x4 x3500)
     (Curry_AbstractHaskell.C_PAs x5 x6) -> Curry_AbstractHaskell.C_PAs x5 (d_C_renameSymbolInPat x1 x6 x3500)
     (Curry_AbstractHaskell.C_PFuncComb x7 x8) -> Curry_AbstractHaskell.C_PFuncComb (Curry_Prelude.d_C_apply x1 x7 x3500) (Curry_Prelude.d_C_map (d_C_renameSymbolInPat x1) x8 x3500)
     (Curry_AbstractHaskell.C_PVar x9) -> x2
     (Curry_AbstractHaskell.C_PLit x10) -> x2
     (Curry_AbstractHaskell.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameSymbolInPat x1 x1002 x3500) (d_C_renameSymbolInPat x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameSymbolInPat x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameSymbolInPat x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameSymbolInPat :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Pattern -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_Pattern
nd_C_renameSymbolInPat x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_PComb x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_PComb (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInPat x1)) x4 x2001 x3500))))))
     (Curry_AbstractHaskell.C_PAs x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AbstractHaskell.C_PAs x5 (nd_C_renameSymbolInPat x1 x6 x2000 x3500)))
     (Curry_AbstractHaskell.C_PFuncComb x7 x8) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_PFuncComb (Curry_Prelude.nd_C_apply x1 x7 x2000 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInPat x1)) x8 x2001 x3500))))))
     (Curry_AbstractHaskell.C_PVar x9) -> x2
     (Curry_AbstractHaskell.C_PLit x10) -> x2
     (Curry_AbstractHaskell.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameSymbolInPat x1 x1002 x3000 x3500) (nd_C_renameSymbolInPat x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameSymbolInPat x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameSymbolInPat x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_renameSymbolInBranch :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_BranchExpr -> ConstStore -> Curry_AbstractHaskell.C_BranchExpr
d_C_renameSymbolInBranch x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Branch x3 x4) -> Curry_AbstractHaskell.C_Branch (d_C_renameSymbolInPat x1 x3 x3500) (d_C_renameSymbolInExpr x1 x4 x3500)
     (Curry_AbstractHaskell.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameSymbolInBranch x1 x1002 x3500) (d_C_renameSymbolInBranch x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameSymbolInBranch x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameSymbolInBranch x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameSymbolInBranch :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_BranchExpr -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_BranchExpr
nd_C_renameSymbolInBranch x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Branch x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_Branch (nd_C_renameSymbolInPat x1 x3 x2000 x3500) (nd_C_renameSymbolInExpr x1 x4 x2001 x3500))))))
     (Curry_AbstractHaskell.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameSymbolInBranch x1 x1002 x3000 x3500) (nd_C_renameSymbolInBranch x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameSymbolInBranch x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameSymbolInBranch x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_renameSymbolInStat :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Statement -> ConstStore -> Curry_AbstractHaskell.C_Statement
d_C_renameSymbolInStat x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_SExpr x3) -> Curry_AbstractHaskell.C_SExpr (d_C_renameSymbolInExpr x1 x3 x3500)
     (Curry_AbstractHaskell.C_SPat x4 x5) -> Curry_AbstractHaskell.C_SPat (d_C_renameSymbolInPat x1 x4 x3500) (d_C_renameSymbolInExpr x1 x5 x3500)
     (Curry_AbstractHaskell.C_SLet x6) -> Curry_AbstractHaskell.C_SLet (Curry_Prelude.d_C_map (d_C_renameSymbolInLocal x1) x6 x3500)
     (Curry_AbstractHaskell.Choice_C_Statement x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameSymbolInStat x1 x1002 x3500) (d_C_renameSymbolInStat x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Statement x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameSymbolInStat x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Statement x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameSymbolInStat x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Statement x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameSymbolInStat :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Statement -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_Statement
nd_C_renameSymbolInStat x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_SExpr x3) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AbstractHaskell.C_SExpr (nd_C_renameSymbolInExpr x1 x3 x2000 x3500)))
     (Curry_AbstractHaskell.C_SPat x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_SPat (nd_C_renameSymbolInPat x1 x4 x2000 x3500) (nd_C_renameSymbolInExpr x1 x5 x2001 x3500))))))
     (Curry_AbstractHaskell.C_SLet x6) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AbstractHaskell.C_SLet (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInLocal x1)) x6 x2000 x3500)))
     (Curry_AbstractHaskell.Choice_C_Statement x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameSymbolInStat x1 x1002 x3000 x3500) (nd_C_renameSymbolInStat x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_Statement x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameSymbolInStat x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Statement x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameSymbolInStat x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Statement x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_renameSymbolInLocal :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_LocalDecl -> ConstStore -> Curry_AbstractHaskell.C_LocalDecl
d_C_renameSymbolInLocal x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_LocalFunc x3) -> Curry_AbstractHaskell.C_LocalFunc (d_C_renameSymbolInFunc x1 x3 x3500)
     (Curry_AbstractHaskell.C_LocalPat x4 x5 x6) -> Curry_AbstractHaskell.C_LocalPat (d_C_renameSymbolInPat x1 x4 x3500) (d_C_renameSymbolInExpr x1 x5 x3500) (Curry_Prelude.d_C_map (d_C_renameSymbolInLocal x1) x6 x3500)
     (Curry_AbstractHaskell.C_LocalVar x7) -> x2
     (Curry_AbstractHaskell.Choice_C_LocalDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameSymbolInLocal x1 x1002 x3500) (d_C_renameSymbolInLocal x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_LocalDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameSymbolInLocal x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_LocalDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameSymbolInLocal x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_LocalDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameSymbolInLocal :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_LocalDecl -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_LocalDecl
nd_C_renameSymbolInLocal x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_LocalFunc x3) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AbstractHaskell.C_LocalFunc (nd_C_renameSymbolInFunc x1 x3 x2000 x3500)))
     (Curry_AbstractHaskell.C_LocalPat x4 x5 x6) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (Curry_AbstractHaskell.C_LocalPat (nd_C_renameSymbolInPat x1 x4 x2000 x3500) (nd_C_renameSymbolInExpr x1 x5 x2001 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInLocal x1)) x6 x2002 x3500)))))))))
     (Curry_AbstractHaskell.C_LocalVar x7) -> x2
     (Curry_AbstractHaskell.Choice_C_LocalDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameSymbolInLocal x1 x1002 x3000 x3500) (nd_C_renameSymbolInLocal x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_LocalDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameSymbolInLocal x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_LocalDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameSymbolInLocal x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_LocalDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_renameSymbolInTypeSig :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_TypeSig -> ConstStore -> Curry_AbstractHaskell.C_TypeSig
d_C_renameSymbolInTypeSig x1 x2 x3500 = case x2 of
     Curry_AbstractHaskell.C_Untyped -> Curry_AbstractHaskell.C_Untyped
     (Curry_AbstractHaskell.C_FType x3) -> Curry_AbstractHaskell.C_FType (d_C_renameSymbolInTypeExpr x1 x3 x3500)
     (Curry_AbstractHaskell.C_CType x4 x5) -> Curry_AbstractHaskell.C_CType (Curry_Prelude.d_C_map (d_OP_renameSymbolInTypeSig_dot___hash_lambda9 x1) x4 x3500) (d_C_renameSymbolInTypeExpr x1 x5 x3500)
     (Curry_AbstractHaskell.Choice_C_TypeSig x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameSymbolInTypeSig x1 x1002 x3500) (d_C_renameSymbolInTypeSig x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeSig x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameSymbolInTypeSig x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeSig x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameSymbolInTypeSig x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeSig x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameSymbolInTypeSig :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_TypeSig -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_TypeSig
nd_C_renameSymbolInTypeSig x1 x2 x3000 x3500 = case x2 of
     Curry_AbstractHaskell.C_Untyped -> Curry_AbstractHaskell.C_Untyped
     (Curry_AbstractHaskell.C_FType x3) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AbstractHaskell.C_FType (nd_C_renameSymbolInTypeExpr x1 x3 x2000 x3500)))
     (Curry_AbstractHaskell.C_CType x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AbstractHaskell.C_CType (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_renameSymbolInTypeSig_dot___hash_lambda9 x1)) x4 x2000 x3500) (nd_C_renameSymbolInTypeExpr x1 x5 x2001 x3500))))))
     (Curry_AbstractHaskell.Choice_C_TypeSig x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameSymbolInTypeSig x1 x1002 x3000 x3500) (nd_C_renameSymbolInTypeSig x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeSig x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameSymbolInTypeSig x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeSig x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameSymbolInTypeSig x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeSig x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_renameSymbolInTypeSig_dot___hash_lambda9 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Context -> ConstStore -> Curry_AbstractHaskell.C_Context
d_OP_renameSymbolInTypeSig_dot___hash_lambda9 x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Context x3 x4) -> Curry_AbstractHaskell.C_Context (Curry_Prelude.d_C_apply x1 x3 x3500) x4
     (Curry_AbstractHaskell.Choice_C_Context x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_renameSymbolInTypeSig_dot___hash_lambda9 x1 x1002 x3500) (d_OP_renameSymbolInTypeSig_dot___hash_lambda9 x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Context x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_renameSymbolInTypeSig_dot___hash_lambda9 x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Context x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_renameSymbolInTypeSig_dot___hash_lambda9 x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Context x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_renameSymbolInTypeSig_dot___hash_lambda9 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Context -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_Context
nd_OP_renameSymbolInTypeSig_dot___hash_lambda9 x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Context x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AbstractHaskell.C_Context (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) x4))
     (Curry_AbstractHaskell.Choice_C_Context x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_renameSymbolInTypeSig_dot___hash_lambda9 x1 x1002 x3000 x3500) (nd_OP_renameSymbolInTypeSig_dot___hash_lambda9 x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_Context x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_renameSymbolInTypeSig_dot___hash_lambda9 x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Context x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_renameSymbolInTypeSig_dot___hash_lambda9 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Context x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_renameSymbolInFunc :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_FuncDecl -> ConstStore -> Curry_AbstractHaskell.C_FuncDecl
d_C_renameSymbolInFunc x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Func x3 x4 x5 x6 x7 x8) -> Curry_AbstractHaskell.C_Func x3 (Curry_Prelude.d_C_apply x1 x4 x3500) x5 x6 (d_C_renameSymbolInTypeSig x1 x7 x3500) (d_C_renameSymbolInRules x1 x8 x3500)
     (Curry_AbstractHaskell.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameSymbolInFunc x1 x1002 x3500) (d_C_renameSymbolInFunc x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameSymbolInFunc x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameSymbolInFunc x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameSymbolInFunc :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_FuncDecl -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_FuncDecl
nd_C_renameSymbolInFunc x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Func x3 x4 x5 x6 x7 x8) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (Curry_AbstractHaskell.C_Func x3 (Curry_Prelude.nd_C_apply x1 x4 x2000 x3500) x5 x6 (nd_C_renameSymbolInTypeSig x1 x7 x2001 x3500) (nd_C_renameSymbolInRules x1 x8 x2002 x3500)))))))))
     (Curry_AbstractHaskell.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameSymbolInFunc x1 x1002 x3000 x3500) (nd_C_renameSymbolInFunc x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameSymbolInFunc x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameSymbolInFunc x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_renameSymbolInRules :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Rules -> ConstStore -> Curry_AbstractHaskell.C_Rules
d_C_renameSymbolInRules x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Rules x3) -> Curry_AbstractHaskell.C_Rules (Curry_Prelude.d_C_map (d_C_renameSymbolInRule x1) x3 x3500)
     (Curry_AbstractHaskell.C_External x4) -> Curry_AbstractHaskell.C_External x4
     (Curry_AbstractHaskell.Choice_C_Rules x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameSymbolInRules x1 x1002 x3500) (d_C_renameSymbolInRules x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Rules x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameSymbolInRules x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Rules x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameSymbolInRules x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Rules x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameSymbolInRules :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Rules -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_Rules
nd_C_renameSymbolInRules x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Rules x3) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AbstractHaskell.C_Rules (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInRule x1)) x3 x2000 x3500)))
     (Curry_AbstractHaskell.C_External x4) -> Curry_AbstractHaskell.C_External x4
     (Curry_AbstractHaskell.Choice_C_Rules x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameSymbolInRules x1 x1002 x3000 x3500) (nd_C_renameSymbolInRules x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_Rules x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameSymbolInRules x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Rules x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameSymbolInRules x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Rules x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_renameSymbolInRule :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Rule -> ConstStore -> Curry_AbstractHaskell.C_Rule
d_C_renameSymbolInRule x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Rule x3 x4 x5) -> Curry_AbstractHaskell.C_Rule (Curry_Prelude.d_C_map (d_C_renameSymbolInPat x1) x3 x3500) (Curry_Prelude.d_C_map (d_OP_renameSymbolInRule_dot___hash_lambda10 x1) x4 x3500) (Curry_Prelude.d_C_map (d_C_renameSymbolInLocal x1) x5 x3500)
     (Curry_AbstractHaskell.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameSymbolInRule x1 x1002 x3500) (d_C_renameSymbolInRule x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameSymbolInRule x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameSymbolInRule x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameSymbolInRule :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_Rule -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_Rule
nd_C_renameSymbolInRule x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Rule x3 x4 x5) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (Curry_AbstractHaskell.C_Rule (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInPat x1)) x3 x2000 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_renameSymbolInRule_dot___hash_lambda10 x1)) x4 x2001 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_renameSymbolInLocal x1)) x5 x2002 x3500)))))))))
     (Curry_AbstractHaskell.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameSymbolInRule x1 x1002 x3000 x3500) (nd_C_renameSymbolInRule x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameSymbolInRule x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameSymbolInRule x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_renameSymbolInRule_dot___hash_lambda10 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 Curry_AbstractHaskell.C_Expr Curry_AbstractHaskell.C_Expr -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_AbstractHaskell.C_Expr Curry_AbstractHaskell.C_Expr
d_OP_renameSymbolInRule_dot___hash_lambda10 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 (d_C_renameSymbolInExpr x1 x3 x3500) (d_C_renameSymbolInExpr x1 x4 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_renameSymbolInRule_dot___hash_lambda10 x1 x1002 x3500) (d_OP_renameSymbolInRule_dot___hash_lambda10 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_renameSymbolInRule_dot___hash_lambda10 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_renameSymbolInRule_dot___hash_lambda10 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_renameSymbolInRule_dot___hash_lambda10 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 Curry_AbstractHaskell.C_Expr Curry_AbstractHaskell.C_Expr -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_AbstractHaskell.C_Expr Curry_AbstractHaskell.C_Expr
nd_OP_renameSymbolInRule_dot___hash_lambda10 x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Tuple2 (nd_C_renameSymbolInExpr x1 x3 x2000 x3500) (nd_C_renameSymbolInExpr x1 x4 x2001 x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_renameSymbolInRule_dot___hash_lambda10 x1 x1002 x3000 x3500) (nd_OP_renameSymbolInRule_dot___hash_lambda10 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_renameSymbolInRule_dot___hash_lambda10 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_renameSymbolInRule_dot___hash_lambda10 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_renameOpDecl :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_OpDecl -> ConstStore -> Curry_AbstractHaskell.C_OpDecl
d_C_renameOpDecl x1 x2 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Op x3 x4 x5) -> Curry_AbstractHaskell.C_Op (Curry_Prelude.d_C_apply x1 x3 x3500) x4 x5
     (Curry_AbstractHaskell.Choice_C_OpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_renameOpDecl x1 x1002 x3500) (d_C_renameOpDecl x1 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_OpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_renameOpDecl x1 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_OpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_renameOpDecl x1 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_OpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_renameOpDecl :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractHaskell.C_OpDecl -> IDSupply -> ConstStore -> Curry_AbstractHaskell.C_OpDecl
nd_C_renameOpDecl x1 x2 x3000 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Op x3 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AbstractHaskell.C_Op (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) x4 x5))
     (Curry_AbstractHaskell.Choice_C_OpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_renameOpDecl x1 x1002 x3000 x3500) (nd_C_renameOpDecl x1 x1003 x3000 x3500)
     (Curry_AbstractHaskell.Choices_C_OpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_renameOpDecl x1 z x3000 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_OpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_renameOpDecl x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_OpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_funcDecls :: Curry_AbstractHaskell.C_Prog -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_FuncDecl
d_C_funcDecls x1 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Prog x2 x3 x4 x5 x6) -> x5
     (Curry_AbstractHaskell.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_funcDecls x1002 x3500) (d_C_funcDecls x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_funcDecls z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_funcDecls x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_funcName :: Curry_AbstractHaskell.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_funcName x1 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Func x2 x3 x4 x5 x6 x7) -> x3
     (Curry_AbstractHaskell.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_funcName x1002 x3500) (d_C_funcName x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_funcName z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_funcName x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_typeOf :: Curry_AbstractHaskell.C_FuncDecl -> ConstStore -> Curry_AbstractHaskell.C_TypeSig
d_C_typeOf x1 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Func x2 x3 x4 x5 x6 x7) -> x6
     (Curry_AbstractHaskell.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_typeOf x1002 x3500) (d_C_typeOf x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_typeOf z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_typeOf x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_commentOf :: Curry_AbstractHaskell.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_commentOf x1 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Func x2 x3 x4 x5 x6 x7) -> x2
     (Curry_AbstractHaskell.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_commentOf x1002 x3500) (d_C_commentOf x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_commentOf z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_commentOf x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_AbstractHaskell.C_Let x1 x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x2 x1002 x3500) (d_OP__case_0 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_AbstractHaskell.C_Let x1 x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x2 x1002 x3000 x3500) (nd_OP__case_0 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> d_C_baseType (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_2 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 1#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x2 x1002 x3500) (d_OP__case_3 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> d_C_baseType (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 1#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x2 x1002 x3000 x3500) (nd_OP__case_3 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_head x1 x3500
     Curry_Prelude.C_False -> d_OP__case_1 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x2 x1002 x3500) (d_OP__case_2 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_head x1 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x2 x1002 x3000 x3500) (nd_OP__case_2 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_TCons (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_take (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ','#) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500)) x3500) x1
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x2 x1002 x3500) (d_OP__case_1 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_TCons (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_take (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ','#) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500)) x3500) x1
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x2 x1002 x3000 x3500) (nd_OP__case_1 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_PComb (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_5 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 1#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x2 x1002 x3500) (d_OP__case_6 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_PComb (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 1#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1 x2 x1002 x3000 x3500) (nd_OP__case_6 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_head x1 x3500
     Curry_Prelude.C_False -> d_OP__case_4 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x2 x1002 x3500) (d_OP__case_5 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_head x1 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1 x2 x1002 x3000 x3500) (nd_OP__case_5 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_PComb (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_take (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ','#) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500)) x3500) x1
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x2 x1002 x3500) (d_OP__case_4 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_PComb (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_take (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ','#) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500)) x3500) x1
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x2 x1002 x3000 x3500) (nd_OP__case_4 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> d_C_constF (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_8 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 1#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x2 x1002 x3500) (d_OP__case_9 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> d_C_constF (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 1#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x2 x1002 x3000 x3500) (nd_OP__case_9 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_head x1 x3500
     Curry_Prelude.C_False -> d_OP__case_7 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x2 x1002 x3500) (d_OP__case_8 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_head x1 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1 x2 x1002 x3000 x3500) (nd_OP__case_8 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> d_C_applyF (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_take (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ','#) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500)) x3500) x1 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x2 x1002 x3500) (d_OP__case_7 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> d_C_applyF (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_take (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ','#) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500)) x3500) x1 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1 x2 x1002 x3000 x3500) (nd_OP__case_7 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_PComb (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_11 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 1#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1 x2 x1002 x3500) (d_OP__case_12 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_PComb (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_11 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 1#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1 x2 x1002 x3000 x3500) (nd_OP__case_12 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_head x1 x3500
     Curry_Prelude.C_False -> d_OP__case_10 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1 x2 x1002 x3500) (d_OP__case_11 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_head x1 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1 x2 x1002 x3000 x3500) (nd_OP__case_11 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_PComb (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_take (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ','#) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500)) x3500) x1
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1 x2 x1002 x3500) (d_OP__case_10 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_PComb (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_take (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ','#) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500)) x3500) x1
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x1 x2 x1002 x3000 x3500) (nd_OP__case_10 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
