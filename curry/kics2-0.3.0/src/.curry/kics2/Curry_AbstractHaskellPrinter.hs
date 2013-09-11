{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AbstractHaskellPrinter (C_Options, d_C_showProg, d_C_showModuleHeader, d_C_showDecls, d_C_showTypeDecls, d_C_showTypeDecl, d_C_showTypeExpr, d_C_showFuncDecl, nd_C_showFuncDecl, d_C_showExpr, nd_C_showExpr, d_C_showPattern, d_C_showLiteral, d_C_showInt, d_C_showFloat) where

import Basics
import qualified Curry_AbstractHaskell
import qualified Curry_Char
import qualified Curry_List
import qualified Curry_Names
import qualified Curry_Prelude
import qualified Curry_Maybe
import qualified Curry_Read
data C_Options
     = C_Options (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_Options Cover ID C_Options C_Options
     | Choices_C_Options Cover ID ([C_Options])
     | Fail_C_Options Cover FailInfo
     | Guard_C_Options Cover Constraints C_Options

instance Show C_Options where
  showsPrec d (Choice_C_Options cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Options cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Options cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Options cd info) = showChar '!'
  showsPrec _ (C_Options x1) = (showString "(Options") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Options where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Options x1,r1) | (_,r0) <- readQualified "AbstractHaskellPrinter" "Options" r, (x1,r1) <- readsPrec 11 r0]) s


instance NonDet C_Options where
  choiceCons = Choice_C_Options
  choicesCons = Choices_C_Options
  failCons = Fail_C_Options
  guardCons = Guard_C_Options
  try (Choice_C_Options cd i x y) = tryChoice cd i x y
  try (Choices_C_Options cd i xs) = tryChoices cd i xs
  try (Fail_C_Options cd info) = Fail cd info
  try (Guard_C_Options cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Options cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Options cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Options cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Options cd i _) = error ("AbstractHaskellPrinter.Options.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Options cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Options cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Options where
  generate s c = Choices_C_Options c (freeID [1] s) [(C_Options (generate (leftSupply s) c))]


instance NormalForm C_Options where
  ($!!) cont (C_Options x1) d cs = (((\y1 d cs -> cont (C_Options y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Options cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Options cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Options cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Options cd info) _ _ = failCons cd info
  ($##) cont (C_Options x1) d cs = (((\y1 d cs -> cont (C_Options y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Options cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Options cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Options cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Options cd info) _ _ = failCons cd info
  searchNF search cont (C_Options x1) = search (\y1 -> cont (C_Options y1)) x1
  searchNF _ _ x = error ("AbstractHaskellPrinter.Options.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Options where
  (=.=) (C_Options x1) (C_Options y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Options x1) (C_Options y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Options x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Options cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Options cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Options cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Options cd i _) = error ("AbstractHaskellPrinter.Options.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Options cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Options cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Options x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Options cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Options cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Options cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Options cd i _) = error ("AbstractHaskellPrinter.Options.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Options cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Options cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Options where
  (=?=) (Choice_C_Options cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Options cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Options cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Options cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Options cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Options cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Options cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Options cd info) _ _ = failCons cd info
  (=?=) (C_Options x1) (C_Options y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (<?=) (Choice_C_Options cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Options cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Options cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Options cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Options cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Options cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Options cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Options cd info) _ _ = failCons cd info
  (<?=) (C_Options x1) (C_Options y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs


d_OP___hash_selR_at_Options_dot_currentModule :: C_Options -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP___hash_selR_at_Options_dot_currentModule x1 x3250 x3500 = case x1 of
     (C_Options x2) -> x2
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_Options_dot_currentModule x1002 x3250 x3500) (d_OP___hash_selR_at_Options_dot_currentModule x1003 x3250 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_Options_dot_currentModule z x3250 x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_Options_dot_currentModule x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_Options_dot_currentModule :: C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_Options
d_OP___hash_updR_at_Options_dot_currentModule x1 x2 x3250 x3500 = case x1 of
     (C_Options x3) -> C_Options x2
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_Options_dot_currentModule x1002 x2 x3250 x3500) (d_OP___hash_updR_at_Options_dot_currentModule x1003 x2 x3250 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_Options_dot_currentModule z x2 x3250 x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_Options_dot_currentModule x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_defaultOptions :: Cover -> ConstStore -> C_Options
d_C_defaultOptions x3250 x3500 = C_Options Curry_Prelude.OP_List

d_C_showProg :: Curry_AbstractHaskell.C_Prog -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showProg x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Prog x2 x3 x4 x5 x6) -> Curry_Prelude.d_OP_dollar (d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_not Curry_Prelude.d_C_null x3250 x3500)) (Curry_Prelude.OP_Cons (d_C_showModuleHeader x2 x4 x5 x3 x3250 x3500) (Curry_Prelude.OP_Cons (d_C_showDecls x2 x6 x4 x5 x3250 x3500) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showProg x1002 x3250 x3500) (d_C_showProg x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showProg z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showProg x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showModuleHeader :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeDecl -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_FuncDecl -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showModuleHeader x1 x2 x3 x4 x3250 x3500 = let
     x5 = d_C_showExports x2 x3 x3250 x3500
     x6 = d_C_showImports x4 x3250 x3500
      in (Curry_Prelude.d_C_concat (Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (d_OP__case_175 x6 (Curry_Prelude.d_C_null x6 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List)))) x3250 x3500)

d_C_showDecls :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_OpDecl -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeDecl -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showDecls x1 x2 x3 x4 x3250 x3500 = let
     x5 = C_Options x1
      in (Curry_Prelude.d_OP_dollar (d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_not Curry_Prelude.d_C_null x3250 x3500) (Curry_Prelude.OP_Cons (d_C_showOpDecls x2 x3250 x3500) (Curry_Prelude.OP_Cons (d_C_showTypeDecls x5 x3 x3250 x3500) (Curry_Prelude.OP_Cons (d_C_showFuncDecls x5 x4 x3250 x3500) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500)

d_C_showExports :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeDecl -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showExports x1 x2 x3250 x3500 = let
     x3 = Curry_Prelude.d_C_filter d_OP_showExports_dot_isPublicType_dot_13 x1 x3250 x3500
     x4 = Curry_List.d_C_partition d_OP_showExports_dot_allPublicCons_dot_13 x3 x3250 x3500
     x5 = d_OP_showExports_dot___hash_selFP2_hash_withCons x4 x3250 x3500
     x6 = d_OP_showExports_dot___hash_selFP3_hash_withoutCons x4 x3250 x3500
      in (Curry_Prelude.d_OP_dollar (d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)))))) d_OP_showExports_dot_getTypeName_dot_13 x3250 x3500) x5 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_map d_OP_showExports_dot_getTypeName_dot_13 x6 x3250 x3500) (Curry_Prelude.d_C_map d_OP_showExports_dot_getFuncName_dot_13 (Curry_Prelude.d_C_filter d_OP_showExports_dot_isPublicFunc_dot_13 x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)

d_OP_showExports_dot_isPublicType_dot_13 :: Curry_AbstractHaskell.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_showExports_dot_isPublicType_dot_13 x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Type x2 x3 x4 x5) -> Curry_Prelude.d_OP_eq_eq x3 Curry_AbstractHaskell.C_Public x3250 x3500
     (Curry_AbstractHaskell.C_TypeSyn x6 x7 x8 x9) -> Curry_Prelude.d_OP_eq_eq x7 Curry_AbstractHaskell.C_Public x3250 x3500
     (Curry_AbstractHaskell.C_Instance x10 x11 x12 x13) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot_isPublicType_dot_13 x1002 x3250 x3500) (d_OP_showExports_dot_isPublicType_dot_13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot_isPublicType_dot_13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot_isPublicType_dot_13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showExports_dot_isPublicFunc_dot_13 :: Curry_AbstractHaskell.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_showExports_dot_isPublicFunc_dot_13 x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Func x2 x3 x4 x5 x6 x7) -> Curry_Prelude.d_OP_eq_eq x5 Curry_AbstractHaskell.C_Public x3250 x3500
     (Curry_AbstractHaskell.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot_isPublicFunc_dot_13 x1002 x3250 x3500) (d_OP_showExports_dot_isPublicFunc_dot_13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot_isPublicFunc_dot_13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot_isPublicFunc_dot_13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showExports_dot_getTypeName_dot_13 :: Curry_AbstractHaskell.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showExports_dot_getTypeName_dot_13 x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Type x2 x3 x4 x5) -> d_OP__case_174 x2 x3250 x3500
     (Curry_AbstractHaskell.C_TypeSyn x8 x9 x10 x11) -> d_OP__case_173 x8 x3250 x3500
     (Curry_AbstractHaskell.C_Instance x14 x15 x16 x17) -> d_OP__case_172 x14 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot_getTypeName_dot_13 x1002 x3250 x3500) (d_OP_showExports_dot_getTypeName_dot_13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot_getTypeName_dot_13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot_getTypeName_dot_13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showExports_dot_allPublicCons_dot_13 :: Curry_AbstractHaskell.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_showExports_dot_allPublicCons_dot_13 x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Type x2 x3 x4 x5) -> Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length (Curry_Prelude.d_C_filter d_OP_showExports_dot_allPublicCons_dot_13_dot_isPublicCons_dot_59 x5 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_length x5 x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_TypeSyn x6 x7 x8 x9) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Instance x10 x11 x12 x13) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot_allPublicCons_dot_13 x1002 x3250 x3500) (d_OP_showExports_dot_allPublicCons_dot_13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot_allPublicCons_dot_13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot_allPublicCons_dot_13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showExports_dot_allPublicCons_dot_13_dot_isPublicCons_dot_59 :: Curry_AbstractHaskell.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_showExports_dot_allPublicCons_dot_13_dot_isPublicCons_dot_59 x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Cons x2 x3 x4 x5) -> Curry_Prelude.d_OP_eq_eq x4 Curry_AbstractHaskell.C_Public x3250 x3500
     (Curry_AbstractHaskell.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot_allPublicCons_dot_13_dot_isPublicCons_dot_59 x1002 x3250 x3500) (d_OP_showExports_dot_allPublicCons_dot_13_dot_isPublicCons_dot_59 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot_allPublicCons_dot_13_dot_isPublicCons_dot_59 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot_allPublicCons_dot_13_dot_isPublicCons_dot_59 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showExports_dot_getFuncName_dot_13 :: Curry_AbstractHaskell.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showExports_dot_getFuncName_dot_13 x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Func x2 x3 x4 x5 x6 x7) -> d_OP__case_171 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot_getFuncName_dot_13 x1002 x3250 x3500) (d_OP_showExports_dot_getFuncName_dot_13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot_getFuncName_dot_13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot_getFuncName_dot_13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showExports_dot___hash_selFP2_hash_withCons :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeDecl) (Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeDecl) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeDecl
d_OP_showExports_dot___hash_selFP2_hash_withCons x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot___hash_selFP2_hash_withCons x1002 x3250 x3500) (d_OP_showExports_dot___hash_selFP2_hash_withCons x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot___hash_selFP2_hash_withCons z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot___hash_selFP2_hash_withCons x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showExports_dot___hash_selFP3_hash_withoutCons :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeDecl) (Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeDecl) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeDecl
d_OP_showExports_dot___hash_selFP3_hash_withoutCons x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showExports_dot___hash_selFP3_hash_withoutCons x1002 x3250 x3500) (d_OP_showExports_dot___hash_selFP3_hash_withoutCons x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showExports_dot___hash_selFP3_hash_withoutCons z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showExports_dot___hash_selFP3_hash_withoutCons x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showImports :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showImports x1 x3250 x3500 = d_C_prefixInter d_OP_showImports_dot_showImport_dot_89 x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500

d_OP_showImports_dot_showImport_dot_89 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showImports_dot_showImport_dot_89 x1 x3250 x3500 = d_OP__case_169 x1 (Curry_Prelude.d_C_apply (Curry_Names.d_C_isHaskellModule x3250 x3500) x1 x3250 x3500) x3250 x3500

d_C_showOpDecls :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_OpDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showOpDecls x1 x3250 x3500 = d_C_prefixInter d_C_showOpDecl x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500

d_C_showOpDecl :: Curry_AbstractHaskell.C_OpDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showOpDecl x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Op x2 x3 x4) -> d_OP__case_167 x4 x3 x2 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_OpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showOpDecl x1002 x3250 x3500) (d_C_showOpDecl x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_OpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showOpDecl z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_OpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showOpDecl x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_OpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showFixity :: Curry_AbstractHaskell.C_Fixity -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showFixity x1 x3250 x3500 = case x1 of
     Curry_AbstractHaskell.C_InfixOp -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List))))
     Curry_AbstractHaskell.C_InfixlOp -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))
     Curry_AbstractHaskell.C_InfixrOp -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))
     (Curry_AbstractHaskell.Choice_C_Fixity x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showFixity x1002 x3250 x3500) (d_C_showFixity x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Fixity x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showFixity z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Fixity x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showFixity x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Fixity x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showTypeDecls :: C_Options -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTypeDecls x1 x2 x3250 x3500 = d_C_prefixInter (d_C_showTypeDecl x1) x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3250 x3500

d_C_showTypeDecl :: C_Options -> Curry_AbstractHaskell.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTypeDecl x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_TypeSyn x3 x4 x5 x6) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_prefixMap (d_C_showTypeExpr x1 Curry_Prelude.C_False) (Curry_Prelude.d_C_map (acceptCs id Curry_AbstractHaskell.C_TVar) x5 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (d_C_showTypeExpr x1 Curry_Prelude.C_False x6 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Type x7 x8 x9 x10) -> d_OP__case_165 x10 x1 x9 x7 (Curry_Prelude.d_C_null x10 x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Instance x11 x12 x13 x14) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showContext x1 x13 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x11 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExpr x1 Curry_Prelude.C_True x12 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_OP_showTypeDecl_dot_showInstRule_dot_112 x1) x3250 x3500) x14 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showTypeDecl x1 x1002 x3250 x3500) (d_C_showTypeDecl x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showTypeDecl x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showTypeDecl x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showTypeDecl_dot_showInstRule_dot_112 :: Curry_Prelude.Curry t0 => C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_AbstractHaskell.C_Rule -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showTypeDecl_dot_showInstRule_dot_112 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_163 x4 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showTypeDecl_dot_showInstRule_dot_112 x1 x1002 x3250 x3500) (d_OP_showTypeDecl_dot_showInstRule_dot_112 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showTypeDecl_dot_showInstRule_dot_112 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showTypeDecl_dot_showInstRule_dot_112 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showContext :: C_Options -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Context -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showContext x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_161 x2 x1 x3 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showContext x1 x1002 x3250 x3500) (d_C_showContext x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showContext x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showContext x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showClass :: C_Options -> Curry_AbstractHaskell.C_Context -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showClass x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Context x3 x4) -> d_C_showTypeExpr x1 Curry_Prelude.C_False (Curry_AbstractHaskell.C_TCons x3 (Curry_Prelude.d_C_map (acceptCs id Curry_AbstractHaskell.C_TVar) x4 x3250 x3500)) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Context x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showClass x1 x1002 x3250 x3500) (d_C_showClass x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Context x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showClass x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Context x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showClass x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Context x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showConsDecl :: C_Options -> Curry_AbstractHaskell.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showConsDecl x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Cons x3 x4 x5 x6) -> Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x3 x3250 x3500) (d_C_prefixMap (d_C_showTypeExpr x1 Curry_Prelude.C_True) x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showConsDecl x1 x1002 x3250 x3500) (d_C_showConsDecl x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showConsDecl x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showConsDecl x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showTypeExpr :: C_Options -> Curry_Prelude.C_Bool -> Curry_AbstractHaskell.C_TypeExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTypeExpr x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_AbstractHaskell.C_TVar x4) -> d_OP__case_160 x4 x3250 x3500
     (Curry_AbstractHaskell.C_FuncType x7 x8) -> Curry_Prelude.d_OP_dollar (d_C_maybeShowBrackets x2) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExpr x1 (d_C_isFuncType x7 x3250 x3500) x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (d_C_showTypeExpr x1 Curry_Prelude.C_False x8 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_TCons x9 x10) -> d_OP__case_159 x10 x1 x2 x9 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showTypeExpr x1 x2 x1002 x3250 x3500) (d_C_showTypeExpr x1 x2 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showTypeExpr x1 x2 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showTypeExpr x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showTypeSig :: C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_TypeSig -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTypeSig x1 x2 x3 x3250 x3500 = case x3 of
     Curry_AbstractHaskell.C_Untyped -> Curry_Prelude.OP_List
     (Curry_AbstractHaskell.C_FType x4) -> Curry_Prelude.d_OP_plus_plus (d_OP__case_156 x2 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExpr x1 Curry_Prelude.C_False x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_CType x5 x6) -> Curry_Prelude.d_OP_plus_plus (d_OP__case_155 x2 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (d_C_showContext x1 x5 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExpr x1 Curry_Prelude.C_False x6 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_TypeSig x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showTypeSig x1 x2 x1002 x3250 x3500) (d_C_showTypeSig x1 x2 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeSig x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showTypeSig x1 x2 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeSig x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showTypeSig x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeSig x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showTypeVar :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTypeVar x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_154 x3 x2 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char 'a'#) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x3 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Char.d_C_isDigit x3250 x3500) x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showTypeVar x1002 x3250 x3500) (d_C_showTypeVar x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showTypeVar z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showTypeVar x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showIdentifier :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showIdentifier x3250 x3500 = Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_not (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))) x3250 x3500)

nd_C_showIdentifier :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_showIdentifier x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (Curry_Prelude.nd_C_filter (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_not) (wrapNX id (Curry_Prelude.nd_C_flip (wrapNX id Curry_Prelude.nd_C_elem) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)))) x2000 x3250 x3500))))

d_C_showFuncDecl :: Cover -> ConstStore -> Curry_AbstractHaskell.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showFuncDecl x3250 x3500 = d_C_showFuncDeclOpt (d_C_defaultOptions x3250 x3500)

nd_C_showFuncDecl :: IDSupply -> Cover -> ConstStore -> Func Curry_AbstractHaskell.C_FuncDecl (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_showFuncDecl x3000 x3250 x3500 = wrapDX id (d_C_showFuncDeclOpt (d_C_defaultOptions x3250 x3500))

d_C_showFuncDecls :: C_Options -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showFuncDecls x1 x2 x3250 x3500 = d_C_prefixInter (d_C_showFuncDeclOpt x1) x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3250 x3500

d_C_showFuncDeclOpt :: C_Options -> Curry_AbstractHaskell.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showFuncDeclOpt x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Func x3 x4 x5 x6 x7 x8) -> d_OP__case_153 x8 x7 x1 x3 x5 x4 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showFuncDeclOpt x1 x1002 x3250 x3500) (d_C_showFuncDeclOpt x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showFuncDeclOpt x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showFuncDeclOpt x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showFuncDeclOpt_dot_insertName_dot_160 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showFuncDeclOpt_dot_insertName_dot_160 x1 x2 x3 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_148 x3 x6 x5 x1 x2 (Curry_Prelude.d_OP_slash_eq x3 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showFuncDeclOpt_dot_insertName_dot_160 x1 x2 x3 x1002 x3250 x3500) (d_OP_showFuncDeclOpt_dot_insertName_dot_160 x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showFuncDeclOpt_dot_insertName_dot_160 x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showFuncDeclOpt_dot_insertName_dot_160 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showFuncDeclOpt_dot_rulePrints_dot_160 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Options -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Rule -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showFuncDeclOpt_dot_rulePrints_dot_160 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_OP_dollar (d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (d_OP_showFuncDeclOpt_dot_insertName_dot_160 x1 x2 x5) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_span (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Prelude.C_Char ' '#))) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_tail (d_C_showRule x3) x3250 x3500) x3250 x3500) x3250 x3500) x4 x3250 x3500) x3250 x3500

d_C_funcComment :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_funcComment x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_unlines (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) Curry_Prelude.d_C_lines x3250 x3500) x3250 x3500

nd_C_funcComment :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_funcComment x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_unlines) (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_map (wrapDX id (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) (wrapDX id Curry_Prelude.d_C_lines) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_showLocalFuncDecl :: C_Options -> Cover -> ConstStore -> Curry_AbstractHaskell.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showLocalFuncDecl x1 x3250 x3500 = d_C_showFuncDeclOpt x1

nd_C_showLocalFuncDecl :: C_Options -> IDSupply -> Cover -> ConstStore -> Func Curry_AbstractHaskell.C_FuncDecl (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_showLocalFuncDecl x1 x3000 x3250 x3500 = wrapDX id (d_C_showFuncDeclOpt x1)

d_C_showRule :: C_Options -> Curry_AbstractHaskell.C_Rule -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showRule x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Rule x3 x4 x5) -> Curry_Prelude.d_OP_plus_plus (d_C_prefixMap (d_C_showPattern x1) x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_showCrhsList x1 x4 x3250 x3500) (d_OP__case_147 x5 x1 (Curry_Prelude.d_C_null x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showRule x1 x1002 x3250 x3500) (d_C_showRule x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showRule x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showRule x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showCrhsList :: C_Options -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_AbstractHaskell.C_Expr Curry_AbstractHaskell.C_Expr) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCrhsList x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_146 x4 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCrhsList x1 x1002 x3250 x3500) (d_C_showCrhsList x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCrhsList x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCrhsList x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showCrhs :: C_Options -> Curry_Prelude.OP_Tuple2 Curry_AbstractHaskell.C_Expr Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCrhs x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (d_C_showExprOpt x1 x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (d_C_showExprOpt x1 x4 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCrhs x1 x1002 x3250 x3500) (d_C_showCrhs x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCrhs x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCrhs x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showLocalDecl :: C_Options -> Curry_AbstractHaskell.C_LocalDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showLocalDecl x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_LocalFunc x3) -> Curry_Prelude.d_C_apply (d_C_showLocalFuncDecl x1 x3250 x3500) x3 x3250 x3500
     (Curry_AbstractHaskell.C_LocalPat x4 x5 x6) -> Curry_Prelude.d_OP_plus_plus (d_C_showPattern x1 x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (d_C_showExprOpt x1 x5 x3250 x3500) (d_OP__case_143 x6 x1 (Curry_Prelude.d_C_null x6 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_LocalVar x7) -> Curry_Prelude.d_OP_plus_plus (d_C_showPattern x1 (Curry_AbstractHaskell.C_PVar x7) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_LocalDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showLocalDecl x1 x1002 x3250 x3500) (d_C_showLocalDecl x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_LocalDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showLocalDecl x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_LocalDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showLocalDecl x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_LocalDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showExpr :: Cover -> ConstStore -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showExpr x3250 x3500 = d_C_showExprOpt (d_C_defaultOptions x3250 x3500)

nd_C_showExpr :: IDSupply -> Cover -> ConstStore -> Func Curry_AbstractHaskell.C_Expr (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_showExpr x3000 x3250 x3500 = wrapDX id (d_C_showExprOpt (d_C_defaultOptions x3250 x3500))

d_C_showExprOpt :: C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showExprOpt x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Var x3) -> d_OP__case_142 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lit x6) -> d_C_showLiteral x6 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x7) -> d_OP__case_141 x7 x1 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) (Curry_Prelude.d_C_snd x7 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Apply x8 x9) -> d_C_showApplication x1 (Curry_AbstractHaskell.C_Apply x8 x9) x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x10 x11) -> d_C_showLambdaOrSection x1 x10 x11 x3250 x3500
     (Curry_AbstractHaskell.C_Let x12 x13) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (d_C_showBlock (Curry_Prelude.d_OP_plus_plus (d_C_combineMap (d_C_showLocalDecl x1) x12 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (d_C_showBoxedExpr x1 x13 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x14) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))) (d_C_showBlock (d_C_combineMap (d_C_showStatement x1) x14 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x15 x16) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x15 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (d_C_combineMap (d_C_showStatement x1) x16 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Case x17 x18) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x17 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (d_C_showBlock (d_C_combineMap (d_C_showBranchExpr x1) x18 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Typed x19 x20) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (d_C_showExprOpt x1 x19 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExpr x1 Curry_Prelude.C_False x20 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500)
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showExprOpt x1 x1002 x3250 x3500) (d_C_showExprOpt x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showExprOpt x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showExprOpt x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showSymbol :: C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSymbol x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_140 x3 x1 x4 (Curry_Prelude.d_C_apply (Curry_Names.d_C_isHaskellModule x3250 x3500) x3 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showSymbol x1 x1002 x3250 x3500) (d_C_showSymbol x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showSymbol x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showSymbol x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showLambdaOrSection :: C_Options -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showLambdaOrSection x1 x2 x3 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_137 x3 x2 x1 x5 x4 x3250 x3500
     Curry_Prelude.OP_List -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showLambdaOrSection x1 x1002 x3 x3250 x3500) (d_C_showLambdaOrSection x1 x1003 x3 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showLambdaOrSection x1 z x3 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showLambdaOrSection x1 x1002 x3 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showLambda :: C_Options -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showLambda x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_combineMap (d_C_showPattern x1) x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (d_C_showExprOpt x1 x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_C_showStatement :: C_Options -> Curry_AbstractHaskell.C_Statement -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showStatement x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_SExpr x3) -> d_C_showExprOpt x1 x3 x3250 x3500
     (Curry_AbstractHaskell.C_SPat x4 x5) -> Curry_Prelude.d_OP_plus_plus (d_C_showPattern x1 x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (d_C_showExprOpt x1 x5 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_SLet x6) -> d_OP__case_109 x1 x6 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Statement x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showStatement x1 x1002 x3250 x3500) (d_C_showStatement x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Statement x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showStatement x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Statement x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showStatement x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Statement x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showPattern :: C_Options -> Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showPattern x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_PVar x3) -> d_OP__case_107 x3 x3250 x3500
     (Curry_AbstractHaskell.C_PLit x6) -> d_C_showLitPattern x1 x6 x3250 x3500
     (Curry_AbstractHaskell.C_PComb x7 x8) -> d_OP__case_106 x7 x1 x8 x3250 x3500
     (Curry_AbstractHaskell.C_PAs x13 x14) -> d_OP__case_102 x14 x1 x13 x3250 x3500
     (Curry_AbstractHaskell.C_PFuncComb x17 x18) -> d_C_showPattern x1 (Curry_AbstractHaskell.C_PComb x17 x18) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showPattern x1 x1002 x3250 x3500) (d_C_showPattern x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showPattern x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showPattern x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showLitPattern :: C_Options -> Curry_AbstractHaskell.C_Literal -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showLitPattern x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Intc x3) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 (Curry_Prelude.OP_Tuple2 (Curry_Names.d_C_curryPrelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showInt x3 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500)
     (Curry_AbstractHaskell.C_Floatc x4) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 (Curry_Prelude.OP_Tuple2 (Curry_Names.d_C_curryPrelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showFloat x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500)
     (Curry_AbstractHaskell.C_Charc x5) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 (Curry_Prelude.OP_Tuple2 (Curry_Names.d_C_curryPrelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (d_C_showCharc x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500) x3250 x3500)
     (Curry_AbstractHaskell.Choice_C_Literal x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showLitPattern x1 x1002 x3250 x3500) (d_C_showLitPattern x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Literal x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showLitPattern x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Literal x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showLitPattern x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Literal x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showPreludeCons :: C_Options -> Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showPreludeCons x1 x2 x3250 x3500 = let
     x3 = d_OP_showPreludeCons_dot___hash_selFP5_hash_qname x2 x3250 x3500
     x4 = d_OP_showPreludeCons_dot___hash_selFP6_hash_name x2 x3250 x3500
     x5 = d_OP_showPreludeCons_dot___hash_selFP7_hash_pattlist x2 x3250 x3500
      in (d_OP__case_101 x4 x5 x1 x3 x2 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500)

d_OP_showPreludeCons_dot___hash_selFP5_hash_qname :: Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_showPreludeCons_dot___hash_selFP5_hash_qname x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_PComb x2 x3) -> d_OP__case_98 x2 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showPreludeCons_dot___hash_selFP5_hash_qname x1002 x3250 x3500) (d_OP_showPreludeCons_dot___hash_selFP5_hash_qname x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showPreludeCons_dot___hash_selFP5_hash_qname z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showPreludeCons_dot___hash_selFP5_hash_qname x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showPreludeCons_dot___hash_selFP6_hash_name :: Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showPreludeCons_dot___hash_selFP6_hash_name x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_PComb x2 x3) -> d_OP__case_97 x2 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showPreludeCons_dot___hash_selFP6_hash_name x1002 x3250 x3500) (d_OP_showPreludeCons_dot___hash_selFP6_hash_name x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showPreludeCons_dot___hash_selFP6_hash_name z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showPreludeCons_dot___hash_selFP6_hash_name x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showPreludeCons_dot___hash_selFP7_hash_pattlist :: Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern
d_OP_showPreludeCons_dot___hash_selFP7_hash_pattlist x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_PComb x2 x3) -> d_OP__case_96 x3 x2 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showPreludeCons_dot___hash_selFP7_hash_pattlist x1002 x3250 x3500) (d_OP_showPreludeCons_dot___hash_selFP7_hash_pattlist x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showPreludeCons_dot___hash_selFP7_hash_pattlist z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showPreludeCons_dot___hash_selFP7_hash_pattlist x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showPatternList :: C_Options -> Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showPatternList x1 x2 x3250 x3500 = d_OP__case_95 x2 x1 (d_C_isClosedStringPattern x2 x3250 x3500) x3250 x3500

d_C_showPatListElems :: C_Options -> Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_showPatListElems x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_PComb x3 x4) -> d_OP__case_91 x4 x1 x3 x3250 x3500
     (Curry_AbstractHaskell.C_PVar x15) -> Curry_Prelude.OP_Cons (d_C_showPattern x1 (Curry_AbstractHaskell.C_PVar x15) x3250 x3500) Curry_Prelude.OP_List
     (Curry_AbstractHaskell.C_PAs x16 x17) -> Curry_Prelude.OP_Cons (d_C_showPattern x1 (Curry_AbstractHaskell.C_PAs x16 x17) x3250 x3500) Curry_Prelude.OP_List
     (Curry_AbstractHaskell.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showPatListElems x1 x1002 x3250 x3500) (d_C_showPatListElems x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showPatListElems x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showPatListElems x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showAsPatternList :: C_Options -> Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showAsPatternList x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_PAs x3 x4) -> d_OP__case_80 x4 x1 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showAsPatternList x1 x1002 x3250 x3500) (d_C_showAsPatternList x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showAsPatternList x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showAsPatternList x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showBranchExpr :: C_Options -> Curry_AbstractHaskell.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showBranchExpr x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Branch x3 x4) -> Curry_Prelude.d_OP_plus_plus (d_C_showPattern x1 x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (d_C_showExprOpt x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showBranchExpr x1 x1002 x3250 x3500) (d_C_showBranchExpr x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showBranchExpr x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showBranchExpr x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showLiteral :: Curry_AbstractHaskell.C_Literal -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showLiteral x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Intc x2) -> d_C_showInt x2 x3250 x3500
     (Curry_AbstractHaskell.C_Floatc x3) -> d_C_showFloat x3 x3250 x3500
     (Curry_AbstractHaskell.C_Charc x4) -> Curry_Prelude.d_C_show x4 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Literal x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showLiteral x1002 x3250 x3500) (d_C_showLiteral x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Literal x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showLiteral z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Literal x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showLiteral x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Literal x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showInt :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showInt x1 x3250 x3500 = d_OP__case_79 x1 (Curry_Prelude.d_OP_gt_eq x1 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500

d_C_showFloat :: Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showFloat x1 x3250 x3500 = d_OP__case_78 x1 (Curry_Prelude.d_OP_gt_eq x1 (Curry_Prelude.C_Float 0.0#) x3250 x3500) x3250 x3500

d_C_showCharc :: Curry_AbstractHaskell.C_Literal -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCharc x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Charc x2) -> d_OP__case_77 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\n'#) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Literal x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCharc x1002 x3250 x3500) (d_C_showCharc x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Literal x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCharc z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Literal x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCharc x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Literal x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showBlock :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showBlock x1 x3250 x3500 = d_C_combineMap Curry_Prelude.d_C_id (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_slash_eq Curry_Prelude.OP_List) (Curry_Prelude.d_C_lines x1 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500

d_C_showTypeCons :: C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTypeCons x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> d_C_showSymbol x1 x2 x3250 x3500
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_70 x5 x4 x1 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showTypeCons x1 x2 x1002 x3250 x3500) (d_C_showTypeCons x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showTypeCons x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showTypeCons x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showPreludeTypeCons :: C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showPreludeTypeCons x1 x2 x3 x3250 x3500 = d_OP__case_67 x3 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x3 x3250 x3500) (Curry_AbstractHaskell.C_TCons (Curry_Prelude.OP_Tuple2 (Curry_Names.d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500

d_C_showApplication :: C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showApplication x1 x2 x3250 x3500 = d_OP__case_63 x2 x1 (d_C_applicationHead x2 x3250 x3500) x3250 x3500

d_C_applicationHead :: Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_AbstractHaskell.C_Expr
d_C_applicationHead x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Apply x2 x3) -> d_C_applicationHead x2 x3250 x3500
     (Curry_AbstractHaskell.C_Var x4) -> x1
     (Curry_AbstractHaskell.C_Lit x5) -> x1
     (Curry_AbstractHaskell.C_Symbol x6) -> x1
     (Curry_AbstractHaskell.C_Lambda x7 x8) -> x1
     (Curry_AbstractHaskell.C_Let x9 x10) -> x1
     (Curry_AbstractHaskell.C_DoExpr x11) -> x1
     (Curry_AbstractHaskell.C_ListComp x12 x13) -> x1
     (Curry_AbstractHaskell.C_Case x14 x15) -> x1
     (Curry_AbstractHaskell.C_Typed x16 x17) -> x1
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_applicationHead x1002 x3250 x3500) (d_C_applicationHead x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_applicationHead z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_applicationHead x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showSymbolApplication :: C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSymbolApplication x1 x2 x3 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_62 x5 x4 x3 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (Curry_Names.d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showSymbolApplication x1 x1002 x3 x3250 x3500) (d_C_showSymbolApplication x1 x1003 x3 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showSymbolApplication x1 z x3 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showSymbolApplication x1 x1002 x3 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showListApplication :: C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showListApplication x1 x2 x3250 x3500 = d_OP__case_57 x2 x1 (d_C_isStringList x2 x3250 x3500) x3250 x3500

d_C_showCharListApplication :: C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCharListApplication x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Apply x3 x4) -> d_OP__case_54 x4 x1 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCharListApplication x1 x1002 x3250 x3500) (d_C_showCharListApplication x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCharListApplication x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCharListApplication x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showConsListApplication :: C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showConsListApplication x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Apply x3 x4) -> d_OP__case_51 x4 x1 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showConsListApplication x1 x1002 x3250 x3500) (d_C_showConsListApplication x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showConsListApplication x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showConsListApplication x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showSimpleListApplication :: C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSimpleListApplication x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Apply x3 x4) -> d_OP__case_49 x4 x1 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showSimpleListApplication x1 x1002 x3250 x3500) (d_C_showSimpleListApplication x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showSimpleListApplication x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showSimpleListApplication x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showInfixApplication :: C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showInfixApplication x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_AbstractHaskell.C_Apply x4 x5) -> d_OP__case_46 x5 x1 x2 x4 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showInfixApplication x1 x2 x1002 x3250 x3500) (d_C_showInfixApplication x1 x2 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showInfixApplication x1 x2 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showInfixApplication x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showITEApplication :: C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showITEApplication x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Apply x3 x4) -> d_OP__case_44 x4 x1 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showITEApplication x1 x1002 x3250 x3500) (d_C_showITEApplication x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showITEApplication x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showITEApplication x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showTupleApplication :: C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTupleApplication x1 x2 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_OP_showTupleApplication_dot_p_showTuple_dot_390 x1 x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500

d_OP_showTupleApplication_dot_p_showTuple_dot_390 :: C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showTupleApplication_dot_p_showTuple_dot_390 x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Apply x3 x4) -> d_OP__case_41 x4 x1 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showTupleApplication_dot_p_showTuple_dot_390 x1 x1002 x3250 x3500) (d_OP_showTupleApplication_dot_p_showTuple_dot_390 x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showTupleApplication_dot_p_showTuple_dot_390 x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showTupleApplication_dot_p_showTuple_dot_390 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showSimpleApplication :: C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSimpleApplication x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Apply x3 x4) -> Curry_Prelude.d_OP_plus_plus (d_C_showSimpleApplication x1 x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Var x5) -> d_C_showBoxedExpr x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Lit x6) -> d_C_showBoxedExpr x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x7) -> d_C_showBoxedExpr x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x8 x9) -> d_C_showBoxedExpr x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Let x10 x11) -> d_C_showBoxedExpr x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x12) -> d_C_showBoxedExpr x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x13 x14) -> d_C_showBoxedExpr x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Case x15 x16) -> d_C_showBoxedExpr x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x17 x18) -> d_C_showBoxedExpr x1 x2 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showSimpleApplication x1 x1002 x3250 x3500) (d_C_showSimpleApplication x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showSimpleApplication x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showSimpleApplication x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showBoxedExpr :: C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showBoxedExpr x1 x2 x3250 x3500 = d_OP__case_40 x2 x1 (d_C_isSimpleExpr x2 x3250 x3500) x3250 x3500

d_C_intercalate :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_intercalate x1 x2 x3250 x3500 = Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse x1 x2 x3250 x3500) x3250 x3500

d_C_prefixMap :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_prefixMap x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_plus_plus x3) x3250 x3500) (Curry_Prelude.d_C_map x1 x2 x3250 x3500) x3250 x3500

nd_C_prefixMap :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_prefixMap x1 x2 x3 x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id (Curry_Prelude.d_OP_plus_plus x3)) x2000 x3250 x3500) (Curry_Prelude.nd_C_map x1 x2 x2001 x3250 x3500) x2002 x3250 x3500))))))))

d_C_prefixInter :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_prefixInter x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse x3 (Curry_Prelude.d_C_map x1 x2 x3250 x3500) x3250 x3500) x3250 x3500

nd_C_prefixInter :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_prefixInter x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_concat) (Curry_List.d_C_intersperse x3 (Curry_Prelude.nd_C_map x1 x2 x2000 x3250 x3500) x3250 x3500) x2001 x3250 x3500)))))

d_C_combineMap :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_combineMap x1 x2 x3 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x4 x3250 x3500) (d_C_prefixMap x1 x5 x3 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_combineMap x1 x1002 x3 x3250 x3500) (d_C_combineMap x1 x1003 x3 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_combineMap x1 z x3 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_combineMap x1 x1002 x3 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_combineMap :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_combineMap x1 x2 x3 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x4 x2000 x3250 x3500) (nd_C_prefixMap x1 x5 x3 x2001 x3250 x3500) x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_combineMap x1 x1002 x3 x3000 x3250 x3500) (nd_C_combineMap x1 x1003 x3 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_combineMap x1 z x3 x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_combineMap x1 x1002 x3 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_dropTags :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_dropTags x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x4 = x2
           in (d_OP__case_38 x4 x3 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_dropTags x1002 x3250 x3500) (d_C_dropTags x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_dropTags z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_dropTags x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isClosedPatternList :: Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isClosedPatternList x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_PComb x2 x3) -> d_OP__case_36 x3 x2 x3250 x3500
     (Curry_AbstractHaskell.C_PVar x14) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_PAs x15 x16) -> d_C_isClosedPatternList x16 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isClosedPatternList x1002 x3250 x3500) (d_C_isClosedPatternList x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isClosedPatternList z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isClosedPatternList x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isClosedStringPattern :: Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isClosedStringPattern x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_PComb x2 x3) -> d_OP__case_25 x3 x2 x3250 x3500
     (Curry_AbstractHaskell.C_PVar x14) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_PAs x15 x16) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isClosedStringPattern x1002 x3250 x3500) (d_C_isClosedStringPattern x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isClosedStringPattern z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isClosedStringPattern x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isCharPattern :: Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isCharPattern x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_PLit x2) -> d_OP__case_14 x2 x3250 x3500
     (Curry_AbstractHaskell.C_PVar x6) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_PComb x7 x8) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_PAs x9 x10) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_PFuncComb x11 x12) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isCharPattern x1002 x3250 x3500) (d_C_isCharPattern x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isCharPattern z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isCharPattern x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isAsPattern :: Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isAsPattern x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_PAs x2 x3) -> Curry_Prelude.C_True
     (Curry_AbstractHaskell.C_PVar x4) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_PLit x5) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_PComb x6 x7) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_PFuncComb x8 x9) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isAsPattern x1002 x3250 x3500) (d_C_isAsPattern x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isAsPattern z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isAsPattern x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isInfixOpName :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isInfixOpName x3250 x3500 = Curry_Prelude.d_C_all (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem (d_C_infixIDs x3250 x3500)) x3250 x3500

nd_C_isInfixOpName :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool
nd_C_isInfixOpName x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_all (wrapNX id (Curry_Prelude.nd_C_flip (wrapNX id Curry_Prelude.nd_C_elem) (d_C_infixIDs x3250 x3500))) x2000 x3250 x3500))

d_C_isStringList :: Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isStringList x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Symbol x2) -> d_OP__case_13 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Var x5) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Apply x6 x7) -> d_OP__case_12 x7 x6 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isStringList x1002 x3250 x3500) (d_C_isStringList x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isStringList z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isStringList x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isClosedList :: Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isClosedList x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Apply x2 x3) -> d_OP__case_9 x3 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x38) -> d_OP__case_6 x38 x3250 x3500
     (Curry_AbstractHaskell.C_Var x41) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Lit x42) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Lambda x43 x44) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Let x45 x46) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_DoExpr x47) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_ListComp x48 x49) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Case x50 x51) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Typed x52 x53) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isClosedList x1002 x3250 x3500) (d_C_isClosedList x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isClosedList z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isClosedList x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isSimpleExpr :: Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isSimpleExpr x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Var x2) -> Curry_Prelude.C_True
     (Curry_AbstractHaskell.C_Lit x3) -> Curry_Prelude.C_True
     (Curry_AbstractHaskell.C_Symbol x4) -> d_OP__case_5 x4 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x7 x8) -> d_OP__case_4 x7 (d_C_applicationHead x7 x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x27 x28) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Let x29 x30) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_DoExpr x31) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_ListComp x32 x33) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Case x34 x35) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Typed x36 x37) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isSimpleExpr x1002 x3250 x3500) (d_C_isSimpleExpr x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isSimpleExpr z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isSimpleExpr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isFuncType :: Curry_AbstractHaskell.C_TypeExpr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isFuncType x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_FuncType x2 x3) -> Curry_Prelude.C_True
     (Curry_AbstractHaskell.C_TVar x4) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_TCons x5 x6) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isFuncType x1002 x3250 x3500) (d_C_isFuncType x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isFuncType z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isFuncType x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isAtom :: Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isAtom x1 x3250 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Var x2) -> Curry_Prelude.C_True
     (Curry_AbstractHaskell.C_Lit x3) -> Curry_Prelude.C_True
     (Curry_AbstractHaskell.C_Symbol x4) -> d_OP__case_2 x4 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x7 x8) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Lambda x9 x10) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Let x11 x12) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_DoExpr x13) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_ListComp x14 x15) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Case x16 x17) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Typed x18 x19) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isAtom x1002 x3250 x3500) (d_C_isAtom x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isAtom z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isAtom x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isTuple :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isTuple x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '('#) x3250 x3500) (d_OP_isTuple_dot_p1_isTuple_dot_539 x3 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isTuple x1002 x3250 x3500) (d_C_isTuple x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isTuple z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isTuple x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_isTuple_dot_p1_isTuple_dot_539 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isTuple_dot_p1_isTuple_dot_539 x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isTuple_dot_p1_isTuple_dot_539 x1002 x3250 x3500) (d_OP_isTuple_dot_p1_isTuple_dot_539 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isTuple_dot_p1_isTuple_dot_539 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isTuple_dot_p1_isTuple_dot_539 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_infixIDs :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_infixIDs x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '~'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '%'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '^'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)))))))))))))))))))

d_C_maybeShowBrackets :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_maybeShowBrackets x1 x2 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_0 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_maybeShowBrackets x1002 x2 x3250 x3500) (d_C_maybeShowBrackets x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_maybeShowBrackets z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_maybeShowBrackets x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x1002 x3250 x3500) (d_OP__case_0 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char ')'#) x3250 x3500
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char ','#) x3250 x3500) (d_OP_isTuple_dot_p1_isTuple_dot_539 (Curry_Prelude.OP_Cons x4 x5) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x2 x1002 x3250 x3500) (d_OP__case_1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_2 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_not (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1002 x3250 x3500) (d_OP__case_2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_4 x7 x27 x3250 x3500 = case x27 of
     (Curry_AbstractHaskell.C_Symbol x9) -> d_OP__case_3 x9 x3250 x3500
     (Curry_AbstractHaskell.C_Var x12) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Lit x13) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Apply x14 x15) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Lambda x16 x17) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Let x18 x19) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_DoExpr x20) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_ListComp x21 x22) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Case x23 x24) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Typed x25 x26) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x7 x1002 x3250 x3500) (d_OP__case_4 x7 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x7 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_3 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x10 x11) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x10 (Curry_Names.d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3250 x3500) (d_C_isTuple x11 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1002 x3250 x3500) (d_OP__case_3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_5 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_not (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1002 x3250 x3500) (d_OP__case_5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_6 x38 x3250 x3500 = case x38 of
     (Curry_Prelude.OP_Tuple2 x39 x40) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x39 (Curry_Names.d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x40 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1002 x3250 x3500) (d_OP__case_6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_9 x3 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Apply x4 x5) -> d_OP__case_8 x3 x4 x3250 x3500
     (Curry_AbstractHaskell.C_Var x24) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Lit x25) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Symbol x26) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Lambda x27 x28) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Let x29 x30) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_DoExpr x31) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_ListComp x32 x33) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Case x34 x35) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Typed x36 x37) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x3 x1002 x3250 x3500) (d_OP__case_9 x3 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x3 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_8 x3 x4 x3250 x3500 = case x4 of
     (Curry_AbstractHaskell.C_Symbol x6) -> d_OP__case_7 x3 x6 x3250 x3500
     (Curry_AbstractHaskell.C_Var x9) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Lit x10) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Apply x11 x12) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Lambda x13 x14) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Let x15 x16) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_DoExpr x17) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_ListComp x18 x19) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Case x20 x21) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Typed x22 x23) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x3 x1002 x3250 x3500) (d_OP__case_8 x3 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x3 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_7 x3 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x7 (Curry_Names.d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x3250 x3500) (d_C_isClosedList x3 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x3 x1002 x3250 x3500) (d_OP__case_7 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_12 x7 x6 x3250 x3500 = case x6 of
     (Curry_AbstractHaskell.C_Apply x8 x9) -> d_OP__case_11 x7 x9 x3250 x3500
     (Curry_AbstractHaskell.C_Var x29) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Lit x30) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Symbol x31) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Lambda x32 x33) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Let x34 x35) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_DoExpr x36) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_ListComp x37 x38) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Case x39 x40) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Typed x41 x42) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x7 x1002 x3250 x3500) (d_OP__case_12 x7 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x7 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_11 x7 x9 x3250 x3500 = case x9 of
     (Curry_AbstractHaskell.C_Lit x10) -> d_OP__case_10 x7 x10 x3250 x3500
     (Curry_AbstractHaskell.C_Var x14) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Symbol x15) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Apply x16 x17) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Lambda x18 x19) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Let x20 x21) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_DoExpr x22) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_ListComp x23 x24) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Case x25 x26) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Typed x27 x28) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x7 x1002 x3250 x3500) (d_OP__case_11 x7 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x7 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Literal -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_10 x7 x10 x3250 x3500 = case x10 of
     (Curry_AbstractHaskell.C_Charc x11) -> d_C_isStringList x7 x3250 x3500
     (Curry_AbstractHaskell.C_Intc x12) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Floatc x13) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_Literal x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x7 x1002 x3250 x3500) (d_OP__case_10 x7 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Literal x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x7 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Literal x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Literal x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_13 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x3 (Curry_Names.d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1002 x3250 x3500) (d_OP__case_13 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_AbstractHaskell.C_Literal -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_14 x2 x3250 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Charc x3) -> Curry_Prelude.C_True
     (Curry_AbstractHaskell.C_Intc x4) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_Floatc x5) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.Choice_C_Literal x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1002 x3250 x3500) (d_OP__case_14 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Literal x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Literal x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Literal x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_25 x3 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_24 x3 x4 x5 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x3 x1002 x3250 x3500) (d_OP__case_25 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_24 x3 x4 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_23 x7 x3 x4 x6 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x3 x4 x1002 x3250 x3500) (d_OP__case_24 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_23 x7 x3 x4 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.C_Char ':'#) -> d_OP__case_22 x3 x4 x7 x3250 x3500
     (Curry_Prelude.C_Char '['#) -> d_OP__case_18 x3 x4 x7 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [(':',d_OP__case_22 x3 x4 x7 x3250 x3500),('[',d_OP__case_18 x3 x4 x7 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x7 x3 x4 x1002 x3250 x3500) (d_OP__case_23 x7 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x7 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x7 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_18 x3 x4 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x12 x13) -> d_OP__case_17 x13 x3 x4 x12 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x3 x4 x1002 x3250 x3500) (d_OP__case_18 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_17 x13 x3 x4 x12 x3250 x3500 = case x12 of
     (Curry_Prelude.C_Char ']'#) -> d_OP__case_16 x3 x4 x13 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [(']',d_OP__case_16 x3 x4 x13 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x13 x3 x4 x1002 x3250 x3500) (d_OP__case_17 x13 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x13 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x13 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_16 x3 x4 x13 x3250 x3500 = case x13 of
     Curry_Prelude.OP_List -> d_OP__case_15 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x3 x4 x1002 x3250 x3500) (d_OP__case_16 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_15 x4 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_eq_eq x4 (Curry_Names.d_C_prelude x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x4 x1002 x3250 x3500) (d_OP__case_15 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_22 x3 x4 x7 x3250 x3500 = case x7 of
     Curry_Prelude.OP_List -> d_OP__case_21 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x3 x4 x1002 x3250 x3500) (d_OP__case_22 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_21 x4 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_20 x8 x4 x9 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x4 x1002 x3250 x3500) (d_OP__case_21 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_20 x8 x4 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_19 x10 x8 x4 x11 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x8 x4 x1002 x3250 x3500) (d_OP__case_20 x8 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x8 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x8 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_AbstractHaskell.C_Pattern -> Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_19 x10 x8 x4 x11 x3250 x3500 = case x11 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (Curry_Names.d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isCharPattern x8 x3250 x3500) (d_C_isClosedStringPattern x10 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x10 x8 x4 x1002 x3250 x3500) (d_OP__case_19 x10 x8 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x10 x8 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x10 x8 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_36 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_36 x3 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_35 x3 x4 x5 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x3 x1002 x3250 x3500) (d_OP__case_36 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_35 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_35 x3 x4 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_34 x7 x3 x4 x6 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x3 x4 x1002 x3250 x3500) (d_OP__case_35 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_34 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_34 x7 x3 x4 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.C_Char ':'#) -> d_OP__case_33 x3 x4 x7 x3250 x3500
     (Curry_Prelude.C_Char '['#) -> d_OP__case_29 x3 x4 x7 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [(':',d_OP__case_33 x3 x4 x7 x3250 x3500),('[',d_OP__case_29 x3 x4 x7 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x7 x3 x4 x1002 x3250 x3500) (d_OP__case_34 x7 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x7 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x7 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_29 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_29 x3 x4 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x12 x13) -> d_OP__case_28 x13 x3 x4 x12 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x3 x4 x1002 x3250 x3500) (d_OP__case_29 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_28 x13 x3 x4 x12 x3250 x3500 = case x12 of
     (Curry_Prelude.C_Char ']'#) -> d_OP__case_27 x3 x4 x13 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [(']',d_OP__case_27 x3 x4 x13 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x13 x3 x4 x1002 x3250 x3500) (d_OP__case_28 x13 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x13 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x13 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_27 x3 x4 x13 x3250 x3500 = case x13 of
     Curry_Prelude.OP_List -> d_OP__case_26 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x3 x4 x1002 x3250 x3500) (d_OP__case_27 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_26 x4 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_eq_eq x4 (Curry_Names.d_C_prelude x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x4 x1002 x3250 x3500) (d_OP__case_26 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_33 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_33 x3 x4 x7 x3250 x3500 = case x7 of
     Curry_Prelude.OP_List -> d_OP__case_32 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x3 x4 x1002 x3250 x3500) (d_OP__case_33 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_32 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_32 x4 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_31 x4 x9 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x4 x1002 x3250 x3500) (d_OP__case_32 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_31 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_31 x4 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_30 x10 x4 x11 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x4 x1002 x3250 x3500) (d_OP__case_31 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_30 :: Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_30 x10 x4 x11 x3250 x3500 = case x11 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (Curry_Names.d_C_prelude x3250 x3500) x3250 x3500) (d_C_isClosedPatternList x10 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x10 x4 x1002 x3250 x3500) (d_OP__case_30 x10 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x10 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x10 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_38 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_38 x4 x3 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar d_C_dropTags (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_tail (Curry_Prelude.d_C_dropWhile (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Prelude.C_Char '"'#)) x3 x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_37 x4 x3 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Char '>'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x4 x3 x1002 x3250 x3500) (d_OP__case_38 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_37 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_37 x4 x3 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> d_C_dropTags x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x4 x3 x1002 x3250 x3500) (d_OP__case_37 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_40 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_40 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> d_C_showExprOpt x1 x2 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_39 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x2 x1 x1002 x3250 x3500) (d_OP__case_40 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_39 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_39 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showExprOpt x1 x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x2 x1 x1002 x3250 x3500) (d_OP__case_39 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_41 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_41 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_AbstractHaskell.C_Symbol x5) -> d_C_showExprOpt x1 x4 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x6 x7) -> Curry_Prelude.d_OP_plus_plus (d_OP_showTupleApplication_dot_p_showTuple_dot_390 x1 (Curry_AbstractHaskell.C_Apply x6 x7) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showExprOpt x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x4 x1 x1002 x3250 x3500) (d_OP__case_41 x4 x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x4 x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_44 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_44 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_AbstractHaskell.C_Apply x5 x6) -> d_OP__case_43 x4 x1 x3 x6 x5 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x4 x1 x1002 x3250 x3500) (d_OP__case_44 x4 x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 x4 x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_43 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_43 x4 x1 x3 x6 x5 x3250 x3500 = case x5 of
     (Curry_AbstractHaskell.C_Apply x7 x8) -> d_OP__case_42 x4 x1 x3 x6 x8 x7 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x4 x1 x3 x6 x1002 x3250 x3500) (d_OP__case_43 x4 x1 x3 x6 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x4 x1 x3 x6 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x4 x1 x3 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_42 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_42 x4 x1 x3 x6 x8 x7 x3250 x3500 = case x7 of
     (Curry_AbstractHaskell.C_Symbol x9) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (d_C_showExprOpt x1 x8 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (d_C_showExprOpt x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (d_C_showExprOpt x1 x4 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Apply x10 x11) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showITEApplication x1 x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x4 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x4 x1 x3 x6 x8 x1002 x3250 x3500) (d_OP__case_42 x4 x1 x3 x6 x8 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x4 x1 x3 x6 x8 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x4 x1 x3 x6 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_46 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_46 x5 x1 x2 x4 x3250 x3500 = case x4 of
     (Curry_AbstractHaskell.C_Apply x6 x7) -> d_OP__case_45 x5 x1 x2 x7 x4 x6 x3250 x3500
     (Curry_AbstractHaskell.C_Var x24) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x25) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x26) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x27 x28) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Let x29 x30) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x31) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x32 x33) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Case x34 x35) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Typed x36 x37) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_46 x5 x1 x2 x1002 x3250 x3500) (d_OP__case_46 x5 x1 x2 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_46 x5 x1 x2 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_46 x5 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_45 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_45 x5 x1 x2 x7 x4 x6 x3250 x3500 = case x6 of
     (Curry_AbstractHaskell.C_Apply x8 x9) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showInfixApplication x1 x2 x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Var x10) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x11) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x12) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x13 x14) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Let x15 x16) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x17) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x18 x19) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Case x20 x21) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Typed x22 x23) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x5 x1 x2 x7 x4 x1002 x3250 x3500) (d_OP__case_45 x5 x1 x2 x7 x4 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 x5 x1 x2 x7 x4 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x5 x1 x2 x7 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_49 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_49 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_AbstractHaskell.C_Apply x5 x6) -> d_OP__case_48 x1 x6 x4 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x23) -> d_OP__case_47 x4 x1 x23 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_49 x4 x1 x1002 x3250 x3500) (d_OP__case_49 x4 x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_49 x4 x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_49 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_47 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_47 x4 x1 x23 x3250 x3500 = case x23 of
     (Curry_Prelude.OP_Tuple2 x24 x25) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x4 x3250 x3500) x25 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_47 x4 x1 x1002 x3250 x3500) (d_OP__case_47 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_47 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_47 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_48 :: C_Options -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_48 x1 x6 x4 x3250 x3500 = case x4 of
     (Curry_AbstractHaskell.C_Symbol x7) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List))) x3250 x3500
     (Curry_AbstractHaskell.C_Var x8) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x9) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Apply x10 x11) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x12 x13) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Let x14 x15) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x16) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x17 x18) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Case x19 x20) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Typed x21 x22) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showBoxedExpr x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_48 x1 x6 x1002 x3250 x3500) (d_OP__case_48 x1 x6 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_48 x1 x6 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_48 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_51 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_51 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_AbstractHaskell.C_Apply x5 x6) -> d_OP__case_50 x1 x6 x4 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_51 x4 x1 x1002 x3250 x3500) (d_OP__case_51 x4 x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_51 x4 x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_51 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_50 :: C_Options -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_50 x1 x6 x4 x3250 x3500 = case x4 of
     (Curry_AbstractHaskell.C_Symbol x7) -> d_C_showBoxedExpr x1 x6 x3250 x3500
     (Curry_AbstractHaskell.C_Var x8) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x9) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Apply x10 x11) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x12 x13) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Let x14 x15) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x16) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x17 x18) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Case x19 x20) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Typed x21 x22) -> Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showConsListApplication x1 x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_50 x1 x6 x1002 x3250 x3500) (d_OP__case_50 x1 x6 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_50 x1 x6 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_50 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_54 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_54 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_AbstractHaskell.C_Apply x5 x6) -> d_OP__case_53 x4 x1 x6 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_54 x4 x1 x1002 x3250 x3500) (d_OP__case_54 x4 x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_54 x4 x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_54 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_53 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_53 x4 x1 x6 x3250 x3500 = case x6 of
     (Curry_AbstractHaskell.C_Lit x7) -> d_OP__case_52 x1 x7 x4 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_53 x4 x1 x1002 x3250 x3500) (d_OP__case_53 x4 x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_53 x4 x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_53 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_52 :: C_Options -> Curry_AbstractHaskell.C_Literal -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_52 x1 x7 x4 x3250 x3500 = case x4 of
     (Curry_AbstractHaskell.C_Symbol x8) -> d_C_showCharc x7 x3250 x3500
     (Curry_AbstractHaskell.C_Var x9) -> Curry_Prelude.d_OP_plus_plus (d_C_showCharc x7 x3250 x3500) (d_C_showCharListApplication x1 x4 x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x10) -> Curry_Prelude.d_OP_plus_plus (d_C_showCharc x7 x3250 x3500) (d_C_showCharListApplication x1 x4 x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Apply x11 x12) -> Curry_Prelude.d_OP_plus_plus (d_C_showCharc x7 x3250 x3500) (d_C_showCharListApplication x1 x4 x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x13 x14) -> Curry_Prelude.d_OP_plus_plus (d_C_showCharc x7 x3250 x3500) (d_C_showCharListApplication x1 x4 x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Let x15 x16) -> Curry_Prelude.d_OP_plus_plus (d_C_showCharc x7 x3250 x3500) (d_C_showCharListApplication x1 x4 x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x17) -> Curry_Prelude.d_OP_plus_plus (d_C_showCharc x7 x3250 x3500) (d_C_showCharListApplication x1 x4 x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x18 x19) -> Curry_Prelude.d_OP_plus_plus (d_C_showCharc x7 x3250 x3500) (d_C_showCharListApplication x1 x4 x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Case x20 x21) -> Curry_Prelude.d_OP_plus_plus (d_C_showCharc x7 x3250 x3500) (d_C_showCharListApplication x1 x4 x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Typed x22 x23) -> Curry_Prelude.d_OP_plus_plus (d_C_showCharc x7 x3250 x3500) (d_C_showCharListApplication x1 x4 x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_52 x1 x7 x1002 x3250 x3500) (d_OP__case_52 x1 x7 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_52 x1 x7 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_52 x1 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_57 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_57 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showCharListApplication x1 x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_56 x2 x1 (d_C_isClosedList x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_57 x2 x1 x1002 x3250 x3500) (d_OP__case_57 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_57 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_57 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_56 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_56 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showConsListApplication x1 x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_55 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_56 x2 x1 x1002 x3250 x3500) (d_OP__case_56 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_56 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_56 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_55 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_55 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSimpleListApplication x1 x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_55 x2 x1 x1002 x3250 x3500) (d_OP__case_55 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_55 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_55 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_62 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_62 x5 x4 x3 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_showListApplication x1 x3 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_61 x5 x4 x3 x1 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x5 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_62 x5 x4 x3 x1 x1002 x3250 x3500) (d_OP__case_62 x5 x4 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_62 x5 x4 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_62 x5 x4 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_61 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_61 x5 x4 x3 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_showInfixApplication x1 (Curry_Prelude.OP_Tuple2 x4 x5) x3 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_60 x5 x4 x3 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (Curry_Names.d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_61 x5 x4 x3 x1 x1002 x3250 x3500) (d_OP__case_61 x5 x4 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_61 x5 x4 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_61 x5 x4 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_60 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_60 x5 x4 x3 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_showITEApplication x1 x3 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_59 x5 x3 x1 (d_C_isTuple x5 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_60 x5 x4 x3 x1 x1002 x3250 x3500) (d_OP__case_60 x5 x4 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_60 x5 x4 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_60 x5 x4 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_59 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_59 x5 x3 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_showTupleApplication x1 x3 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_58 x3 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_59 x5 x3 x1 x1002 x3250 x3500) (d_OP__case_59 x5 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_59 x5 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_59 x5 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_58 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_58 x3 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_showSimpleApplication x1 x3 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_58 x3 x1 x1002 x3250 x3500) (d_OP__case_58 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_58 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_58 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_63 :: Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_63 x2 x1 x19 x3250 x3500 = case x19 of
     (Curry_AbstractHaskell.C_Symbol x3) -> d_C_showSymbolApplication x1 x3 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Var x4) -> d_C_showSimpleApplication x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Lit x5) -> d_C_showSimpleApplication x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x6 x7) -> d_C_showSimpleApplication x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x8 x9) -> d_C_showSimpleApplication x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Let x10 x11) -> d_C_showSimpleApplication x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x12) -> d_C_showSimpleApplication x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x13 x14) -> d_C_showSimpleApplication x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Case x15 x16) -> d_C_showSimpleApplication x1 x2 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x17 x18) -> d_C_showSimpleApplication x1 x2 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_63 x2 x1 x1002 x3250 x3500) (d_OP__case_63 x2 x1 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_63 x2 x1 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_63 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_67 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_67 x3 x2 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List)))))
     Curry_Prelude.C_False -> d_OP__case_66 x2 x3 x1 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_67 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_67 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_67 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_67 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_66 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeExpr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_66 x2 x3 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExpr x1 Curry_Prelude.C_False (Curry_Prelude.d_C_head x3 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_65 x2 x3 x1 (d_C_isTuple x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_66 x2 x3 x1 x1002 x3250 x3500) (d_OP__case_66 x2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_66 x2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_66 x2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_65 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeExpr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_65 x2 x3 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_combineMap (d_C_showTypeExpr x1 Curry_Prelude.C_False) x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_64 x3 x1 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_65 x2 x3 x1 x1002 x3250 x3500) (d_OP__case_65 x2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_65 x2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_65 x2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_64 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeExpr -> C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_64 x3 x1 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x2 (d_C_prefixMap (d_C_showTypeExpr x1 Curry_Prelude.C_True) x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_64 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_64 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_64 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_64 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_70 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeExpr -> Curry_AbstractHaskell.C_TypeExpr -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_70 x5 x4 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_69 x6 x5 x4 x1 x2 x7 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Names.d_C_prelude x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_70 x5 x4 x1 x1002 x3250 x3500) (d_OP__case_70 x5 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_70 x5 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_70 x5 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_69 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeExpr -> Curry_AbstractHaskell.C_TypeExpr -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_69 x6 x5 x4 x1 x2 x7 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_showPreludeTypeCons x1 x7 (Curry_Prelude.OP_Cons x4 x5) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_68 x5 x4 x1 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_69 x6 x5 x4 x1 x2 x7 x1002 x3250 x3500) (d_OP__case_69 x6 x5 x4 x1 x2 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_69 x6 x5 x4 x1 x2 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_69 x6 x5 x4 x1 x2 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_68 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeExpr -> Curry_AbstractHaskell.C_TypeExpr -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_68 x5 x4 x1 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x2 x3250 x3500) (d_C_prefixMap (d_C_showTypeExpr x1 Curry_Prelude.C_True) (Curry_Prelude.OP_Cons x4 x5) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_68 x5 x4 x1 x2 x1002 x3250 x3500) (d_OP__case_68 x5 x4 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_68 x5 x4 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_68 x5 x4 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_77 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_77 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> d_OP__case_76 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\r'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_77 x2 x1002 x3250 x3500) (d_OP__case_77 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_77 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_77 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_76 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_76 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> d_OP__case_75 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\t'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_76 x2 x1002 x3250 x3500) (d_OP__case_76 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_76 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_76 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_75 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_75 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> d_OP__case_74 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\\'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_75 x2 x1002 x3250 x3500) (d_OP__case_75 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_75 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_75 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_74 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_74 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> d_OP__case_73 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_74 x2 x1002 x3250 x3500) (d_OP__case_74 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_74 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_74 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_73 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_73 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> d_OP__case_72 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_73 x2 x1002 x3250 x3500) (d_OP__case_73 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_73 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_73 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_72 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_72 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> d_OP__case_71 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_72 x2 x1002 x3250 x3500) (d_OP__case_72 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_72 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_72 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_71 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_71 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_71 x2 x1002 x3250 x3500) (d_OP__case_71 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_71 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_71 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_78 :: Curry_Prelude.C_Float -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_78 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_show x1 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.d_C_show (Curry_Prelude.d_C_negateFloat x1 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_78 x1 x1002 x3250 x3500) (d_OP__case_78 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_78 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_78 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_79 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_79 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_show x1 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.d_C_show (Curry_Prelude.d_C_negate x1 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_79 x1 x1002 x3250 x3500) (d_OP__case_79 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_79 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_79 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_80 :: Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_80 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showPatListElems x1 x4 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_80 x4 x1 x1002 x3250 x3500) (d_OP__case_80 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_80 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_80 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_91 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_91 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_90 x4 x1 x6 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_91 x4 x1 x1002 x3250 x3500) (d_OP__case_91 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_91 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_91 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_90 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_90 x4 x1 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x7 x8) -> d_OP__case_89 x8 x4 x1 x7 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_90 x4 x1 x1002 x3250 x3500) (d_OP__case_90 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_90 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_90 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_89 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_89 x8 x4 x1 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.C_Char ':'#) -> d_OP__case_88 x4 x1 x8 x3250 x3500
     (Curry_Prelude.C_Char '['#) -> d_OP__case_84 x4 x8 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [(':',d_OP__case_88 x4 x1 x8 x3250 x3500),('[',d_OP__case_84 x4 x8 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_89 x8 x4 x1 x1002 x3250 x3500) (d_OP__case_89 x8 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_89 x8 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_89 x8 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_84 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_84 x4 x8 x3250 x3500 = case x8 of
     (Curry_Prelude.OP_Cons x13 x14) -> d_OP__case_83 x14 x4 x13 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_84 x4 x1002 x3250 x3500) (d_OP__case_84 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_84 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_84 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_83 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_83 x14 x4 x13 x3250 x3500 = case x13 of
     (Curry_Prelude.C_Char ']'#) -> d_OP__case_82 x4 x14 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [(']',d_OP__case_82 x4 x14 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_83 x14 x4 x1002 x3250 x3500) (d_OP__case_83 x14 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_83 x14 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_83 x14 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_82 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_82 x4 x14 x3250 x3500 = case x14 of
     Curry_Prelude.OP_List -> d_OP__case_81 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_82 x4 x1002 x3250 x3500) (d_OP__case_82 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_82 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_82 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_81 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_81 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_81 x1002 x3250 x3500) (d_OP__case_81 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_81 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_81 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_88 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_88 x4 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.OP_List -> d_OP__case_87 x1 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_88 x4 x1 x1002 x3250 x3500) (d_OP__case_88 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_88 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_88 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_87 :: C_Options -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_87 x1 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x9 x10) -> d_OP__case_86 x1 x9 x10 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_87 x1 x1002 x3250 x3500) (d_OP__case_87 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_87 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_87 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_86 :: C_Options -> Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_86 x1 x9 x10 x3250 x3500 = case x10 of
     (Curry_Prelude.OP_Cons x11 x12) -> d_OP__case_85 x11 x1 x9 x12 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_86 x1 x9 x1002 x3250 x3500) (d_OP__case_86 x1 x9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_86 x1 x9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_86 x1 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_85 :: Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_85 x11 x1 x9 x12 x3250 x3500 = case x12 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_showPattern x1 x9 x3250 x3500) (d_C_showPatListElems x1 x11 x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_85 x11 x1 x9 x1002 x3250 x3500) (d_OP__case_85 x11 x1 x9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_85 x11 x1 x9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_85 x11 x1 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_95 :: Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_95 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Prelude.C_Char '\''#)) (Curry_Prelude.d_C_concat (d_C_showPatListElems x1 x2 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_94 x2 x1 (d_C_isClosedPatternList x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_95 x2 x1 x1002 x3250 x3500) (d_OP__case_95 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_95 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_95 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_94 :: Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_94 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_showPatListElems x1 x2 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_93 x2 x1 (d_C_isAsPattern x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_94 x2 x1 x1002 x3250 x3500) (d_OP__case_94 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_94 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_94 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_93 :: Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_93 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> d_C_showAsPatternList x1 x2 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_92 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_93 x2 x1 x1002 x3250 x3500) (d_OP__case_93 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_93 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_93 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_92 :: Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_92 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_C_showPatListElems x1 x2 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_92 x2 x1 x1002 x3250 x3500) (d_OP__case_92 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_92 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_92 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_96 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern
d_OP__case_96 x3 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_96 x3 x1002 x3250 x3500) (d_OP__case_96 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_96 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_96 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_97 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_97 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x5
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_97 x1002 x3250 x3500) (d_OP__case_97 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_97 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_97 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_98 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_98 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_98 x1002 x3250 x3500) (d_OP__case_98 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_98 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_98 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_101 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_101 x4 x5 x1 x3 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_showPatternList x1 x2 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_100 x4 x5 x1 x3 (d_C_isTuple x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_101 x4 x5 x1 x3 x2 x1002 x3250 x3500) (d_OP__case_101 x4 x5 x1 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_101 x4 x5 x1 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_101 x4 x5 x1 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_100 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_100 x4 x5 x1 x3 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_combineMap (d_C_showPattern x1) x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_99 x5 x1 x3 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_100 x4 x5 x1 x3 x1002 x3250 x3500) (d_OP__case_100 x4 x5 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_100 x4 x5 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_100 x4 x5 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_99 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_99 x5 x1 x3 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_prefixMap (d_C_showPattern x1) x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_99 x5 x1 x3 x1002 x3250 x3500) (d_OP__case_99 x5 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_99 x5 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_99 x5 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_102 :: Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_102 x14 x1 x13 x3250 x3500 = case x13 of
     (Curry_Prelude.OP_Tuple2 x15 x16) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_showIdentifier x3250 x3500) x16 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) Curry_Prelude.OP_List) (d_C_showPattern x1 x14 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_102 x14 x1 x1002 x3250 x3500) (d_OP__case_102 x14 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_102 x14 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_102 x14 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_106 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_Options -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_106 x7 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.OP_List -> d_C_showSymbol x1 x7 x3250 x3500
     (Curry_Prelude.OP_Cons x9 x10) -> d_OP__case_105 x10 x9 x1 x7 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_106 x7 x1 x1002 x3250 x3500) (d_OP__case_106 x7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_106 x7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_106 x7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_105 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_105 x10 x9 x1 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> d_OP__case_104 x11 x10 x9 x1 x7 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Names.d_C_prelude x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_105 x10 x9 x1 x1002 x3250 x3500) (d_OP__case_105 x10 x9 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_105 x10 x9 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_105 x10 x9 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_104 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_104 x11 x10 x9 x1 x7 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> d_C_showPreludeCons x1 (Curry_AbstractHaskell.C_PComb x7 (Curry_Prelude.OP_Cons x9 x10)) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_103 x10 x9 x1 x7 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_104 x11 x10 x9 x1 x7 x1002 x3250 x3500) (d_OP__case_104 x11 x10 x9 x1 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_104 x11 x10 x9 x1 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_104 x11 x10 x9 x1 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_103 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_103 x10 x9 x1 x7 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_prefixMap (d_C_showPattern x1) (Curry_Prelude.OP_Cons x9 x10) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_103 x10 x9 x1 x7 x1002 x3250 x3500) (d_OP__case_103 x10 x9 x1 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_103 x10 x9 x1 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_103 x10 x9 x1 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_107 :: Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_107 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_C_apply (d_C_showIdentifier x3250 x3500) x5 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_107 x1002 x3250 x3500) (d_OP__case_107 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_107 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_107 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_109 :: C_Options -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_LocalDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_109 x1 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x7 x8) -> d_OP__case_108 x6 x1 x7 x8 x3250 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (d_C_showBlock (d_C_combineMap (d_C_showLocalDecl x1) x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_109 x1 x1002 x3250 x3500) (d_OP__case_109 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_109 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_109 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_108 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_LocalDecl -> C_Options -> Curry_AbstractHaskell.C_LocalDecl -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_LocalDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_108 x6 x1 x7 x8 x3250 x3500 = case x8 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (d_C_showLocalDecl x1 x7 x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x9 x10) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (d_C_showBlock (d_C_combineMap (d_C_showLocalDecl x1) x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_108 x6 x1 x7 x1002 x3250 x3500) (d_OP__case_108 x6 x1 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_108 x6 x1 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_108 x6 x1 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_137 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_137 x3 x2 x1 x5 x4 x3250 x3500 = case x4 of
     (Curry_AbstractHaskell.C_PVar x6) -> d_OP__case_136 x3 x2 x1 x6 x5 x3250 x3500
     (Curry_AbstractHaskell.C_PLit x219) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_PComb x220 x221) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_PAs x222 x223) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_PFuncComb x224 x225) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_137 x3 x2 x1 x5 x1002 x3250 x3500) (d_OP__case_137 x3 x2 x1 x5 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_137 x3 x2 x1 x5 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_137 x3 x2 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_136 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_136 x3 x2 x1 x6 x5 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> d_OP__case_135 x2 x1 x6 x3 x3250 x3500
     (Curry_Prelude.OP_Cons x217 x218) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_136 x3 x2 x1 x6 x1002 x3250 x3500) (d_OP__case_136 x3 x2 x1 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_136 x3 x2 x1 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_136 x3 x2 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_135 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_135 x2 x1 x6 x3 x3250 x3500 = case x3 of
     (Curry_AbstractHaskell.C_Apply x7 x8) -> d_OP__case_134 x3 x2 x1 x8 x6 x7 x3250 x3500
     (Curry_AbstractHaskell.C_Var x203) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lit x204) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x205) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x206 x207) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Let x208 x209) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x210) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x211 x212) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Case x213 x214) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x215 x216) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_135 x2 x1 x6 x1002 x3250 x3500) (d_OP__case_135 x2 x1 x6 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_135 x2 x1 x6 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_135 x2 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_134 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_134 x3 x2 x1 x8 x6 x7 x3250 x3500 = case x7 of
     (Curry_AbstractHaskell.C_Apply x9 x10) -> d_OP__case_133 x3 x2 x1 x8 x10 x6 x9 x3250 x3500
     (Curry_AbstractHaskell.C_Var x189) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lit x190) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x191) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x192 x193) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Let x194 x195) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x196) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x197 x198) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Case x199 x200) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x201 x202) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_134 x3 x2 x1 x8 x6 x1002 x3250 x3500) (d_OP__case_134 x3 x2 x1 x8 x6 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_134 x3 x2 x1 x8 x6 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_134 x3 x2 x1 x8 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_133 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_133 x3 x2 x1 x8 x10 x6 x9 x3250 x3500 = case x9 of
     (Curry_AbstractHaskell.C_Symbol x11) -> d_OP__case_132 x8 x10 x3 x2 x1 x6 x11 x3250 x3500
     (Curry_AbstractHaskell.C_Var x174) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lit x175) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x176 x177) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x178 x179) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Let x180 x181) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x182) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x183 x184) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Case x185 x186) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x187 x188) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_133 x3 x2 x1 x8 x10 x6 x1002 x3250 x3500) (d_OP__case_133 x3 x2 x1 x8 x10 x6 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_133 x3 x2 x1 x8 x10 x6 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_133 x3 x2 x1 x8 x10 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_132 :: Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_132 x8 x10 x3 x2 x1 x6 x11 x3250 x3500 = case x11 of
     (Curry_Prelude.OP_Tuple2 x12 x13) -> d_OP__case_131 x10 x3 x2 x1 x6 x13 x11 x8 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_132 x8 x10 x3 x2 x1 x6 x1002 x3250 x3500) (d_OP__case_132 x8 x10 x3 x2 x1 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_132 x8 x10 x3 x2 x1 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_132 x8 x10 x3 x2 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_131 :: Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_131 x10 x3 x2 x1 x6 x13 x11 x8 x3250 x3500 = case x8 of
     (Curry_AbstractHaskell.C_Var x14) -> d_OP__case_130 x10 x14 x13 x3 x2 x1 x6 x11 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x13 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom x10 x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractHaskell.C_Var x14) x10 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x15) -> d_OP__case_127 x3 x2 x1 x15 x6 x13 x10 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x32) -> d_OP__case_125 x3 x2 x1 x32 x6 x13 x10 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x49 x50) -> d_OP__case_123 x3 x2 x1 x50 x49 x6 x13 x10 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x67 x68) -> d_OP__case_121 x3 x2 x1 x68 x67 x6 x13 x10 x3250 x3500
     (Curry_AbstractHaskell.C_Let x85 x86) -> d_OP__case_119 x3 x2 x1 x86 x85 x6 x13 x10 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x103) -> d_OP__case_117 x3 x2 x1 x103 x6 x13 x10 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x120 x121) -> d_OP__case_115 x3 x2 x1 x121 x120 x6 x13 x10 x3250 x3500
     (Curry_AbstractHaskell.C_Case x138 x139) -> d_OP__case_113 x3 x2 x1 x139 x138 x6 x13 x10 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x156 x157) -> d_OP__case_111 x3 x2 x1 x157 x156 x6 x13 x10 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_131 x10 x3 x2 x1 x6 x13 x11 x1002 x3250 x3500) (d_OP__case_131 x10 x3 x2 x1 x6 x13 x11 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_131 x10 x3 x2 x1 x6 x13 x11 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_131 x10 x3 x2 x1 x6 x13 x11 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_111 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_AbstractHaskell.C_TypeExpr -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_111 x3 x2 x1 x157 x156 x6 x13 x10 x3250 x3500 = case x10 of
     (Curry_AbstractHaskell.C_Var x158) -> d_OP__case_110 x157 x156 x158 x6 x13 x3 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x13 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x158 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractHaskell.C_Typed x156 x157) x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractHaskell.C_Var x158) (Curry_AbstractHaskell.C_Typed x156 x157) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x159) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x160) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x161 x162) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x163 x164) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Let x165 x166) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x167) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x168 x169) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Case x170 x171) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x172 x173) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_111 x3 x2 x1 x157 x156 x6 x13 x1002 x3250 x3500) (d_OP__case_111 x3 x2 x1 x157 x156 x6 x13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_111 x3 x2 x1 x157 x156 x6 x13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_111 x3 x2 x1 x157 x156 x6 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_110 :: Curry_AbstractHaskell.C_TypeExpr -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_110 x157 x156 x158 x6 x13 x3 x2 x1 x159 x3250 x3500 = case x159 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 (Curry_AbstractHaskell.C_Typed x156 x157) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_110 x157 x156 x158 x6 x13 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_110 x157 x156 x158 x6 x13 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_110 x157 x156 x158 x6 x13 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_110 x157 x156 x158 x6 x13 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_113 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_BranchExpr -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_113 x3 x2 x1 x139 x138 x6 x13 x10 x3250 x3500 = case x10 of
     (Curry_AbstractHaskell.C_Var x140) -> d_OP__case_112 x139 x138 x140 x6 x13 x3 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x13 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x140 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractHaskell.C_Case x138 x139) x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractHaskell.C_Var x140) (Curry_AbstractHaskell.C_Case x138 x139) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x141) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x142) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x143 x144) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x145 x146) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Let x147 x148) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x149) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x150 x151) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Case x152 x153) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x154 x155) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_113 x3 x2 x1 x139 x138 x6 x13 x1002 x3250 x3500) (d_OP__case_113 x3 x2 x1 x139 x138 x6 x13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_113 x3 x2 x1 x139 x138 x6 x13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_113 x3 x2 x1 x139 x138 x6 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_112 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_BranchExpr -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_112 x139 x138 x140 x6 x13 x3 x2 x1 x141 x3250 x3500 = case x141 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 (Curry_AbstractHaskell.C_Case x138 x139) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_112 x139 x138 x140 x6 x13 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_112 x139 x138 x140 x6 x13 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_112 x139 x138 x140 x6 x13 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_112 x139 x138 x140 x6 x13 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_115 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Statement -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_115 x3 x2 x1 x121 x120 x6 x13 x10 x3250 x3500 = case x10 of
     (Curry_AbstractHaskell.C_Var x122) -> d_OP__case_114 x121 x120 x122 x6 x13 x3 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x13 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x122 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractHaskell.C_ListComp x120 x121) x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractHaskell.C_Var x122) (Curry_AbstractHaskell.C_ListComp x120 x121) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x123) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x124) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x125 x126) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x127 x128) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Let x129 x130) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x131) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x132 x133) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Case x134 x135) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x136 x137) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_115 x3 x2 x1 x121 x120 x6 x13 x1002 x3250 x3500) (d_OP__case_115 x3 x2 x1 x121 x120 x6 x13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_115 x3 x2 x1 x121 x120 x6 x13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_115 x3 x2 x1 x121 x120 x6 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_114 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Statement -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_114 x121 x120 x122 x6 x13 x3 x2 x1 x123 x3250 x3500 = case x123 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 (Curry_AbstractHaskell.C_ListComp x120 x121) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_114 x121 x120 x122 x6 x13 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_114 x121 x120 x122 x6 x13 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_114 x121 x120 x122 x6 x13 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_114 x121 x120 x122 x6 x13 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_117 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Statement -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_117 x3 x2 x1 x103 x6 x13 x10 x3250 x3500 = case x10 of
     (Curry_AbstractHaskell.C_Var x104) -> d_OP__case_116 x103 x104 x6 x13 x3 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x13 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x104 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractHaskell.C_DoExpr x103) x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractHaskell.C_Var x104) (Curry_AbstractHaskell.C_DoExpr x103) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x105) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x106) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x107 x108) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x109 x110) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Let x111 x112) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x113) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x114 x115) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Case x116 x117) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x118 x119) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_117 x3 x2 x1 x103 x6 x13 x1002 x3250 x3500) (d_OP__case_117 x3 x2 x1 x103 x6 x13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_117 x3 x2 x1 x103 x6 x13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_117 x3 x2 x1 x103 x6 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_116 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Statement -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_116 x103 x104 x6 x13 x3 x2 x1 x105 x3250 x3500 = case x105 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 (Curry_AbstractHaskell.C_DoExpr x103) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_116 x103 x104 x6 x13 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_116 x103 x104 x6 x13 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_116 x103 x104 x6 x13 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_116 x103 x104 x6 x13 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_119 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_LocalDecl -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_119 x3 x2 x1 x86 x85 x6 x13 x10 x3250 x3500 = case x10 of
     (Curry_AbstractHaskell.C_Var x87) -> d_OP__case_118 x86 x85 x87 x6 x13 x3 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x13 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x87 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractHaskell.C_Let x85 x86) x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractHaskell.C_Var x87) (Curry_AbstractHaskell.C_Let x85 x86) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x88) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x89) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x90 x91) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x92 x93) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Let x94 x95) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x96) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x97 x98) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Case x99 x100) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x101 x102) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_119 x3 x2 x1 x86 x85 x6 x13 x1002 x3250 x3500) (d_OP__case_119 x3 x2 x1 x86 x85 x6 x13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_119 x3 x2 x1 x86 x85 x6 x13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_119 x3 x2 x1 x86 x85 x6 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_118 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_LocalDecl -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_118 x86 x85 x87 x6 x13 x3 x2 x1 x88 x3250 x3500 = case x88 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 (Curry_AbstractHaskell.C_Let x85 x86) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_118 x86 x85 x87 x6 x13 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_118 x86 x85 x87 x6 x13 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_118 x86 x85 x87 x6 x13 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_118 x86 x85 x87 x6 x13 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_121 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_121 x3 x2 x1 x68 x67 x6 x13 x10 x3250 x3500 = case x10 of
     (Curry_AbstractHaskell.C_Var x69) -> d_OP__case_120 x68 x67 x69 x6 x13 x3 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x13 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x69 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractHaskell.C_Lambda x67 x68) x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractHaskell.C_Var x69) (Curry_AbstractHaskell.C_Lambda x67 x68) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x70) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x71) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x72 x73) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x74 x75) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Let x76 x77) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x78) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x79 x80) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Case x81 x82) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x83 x84) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_121 x3 x2 x1 x68 x67 x6 x13 x1002 x3250 x3500) (d_OP__case_121 x3 x2 x1 x68 x67 x6 x13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_121 x3 x2 x1 x68 x67 x6 x13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_121 x3 x2 x1 x68 x67 x6 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_120 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_120 x68 x67 x69 x6 x13 x3 x2 x1 x70 x3250 x3500 = case x70 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 (Curry_AbstractHaskell.C_Lambda x67 x68) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_120 x68 x67 x69 x6 x13 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_120 x68 x67 x69 x6 x13 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_120 x68 x67 x69 x6 x13 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_120 x68 x67 x69 x6 x13 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_123 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_123 x3 x2 x1 x50 x49 x6 x13 x10 x3250 x3500 = case x10 of
     (Curry_AbstractHaskell.C_Var x51) -> d_OP__case_122 x50 x49 x51 x6 x13 x3 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x13 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x51 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractHaskell.C_Apply x49 x50) x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractHaskell.C_Var x51) (Curry_AbstractHaskell.C_Apply x49 x50) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x52) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x53) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x54 x55) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x56 x57) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Let x58 x59) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x60) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x61 x62) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Case x63 x64) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x65 x66) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_123 x3 x2 x1 x50 x49 x6 x13 x1002 x3250 x3500) (d_OP__case_123 x3 x2 x1 x50 x49 x6 x13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_123 x3 x2 x1 x50 x49 x6 x13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_123 x3 x2 x1 x50 x49 x6 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_122 :: Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_122 x50 x49 x51 x6 x13 x3 x2 x1 x52 x3250 x3500 = case x52 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 (Curry_AbstractHaskell.C_Apply x49 x50) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_122 x50 x49 x51 x6 x13 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_122 x50 x49 x51 x6 x13 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_122 x50 x49 x51 x6 x13 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_122 x50 x49 x51 x6 x13 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_125 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_125 x3 x2 x1 x32 x6 x13 x10 x3250 x3500 = case x10 of
     (Curry_AbstractHaskell.C_Var x33) -> d_OP__case_124 x32 x33 x6 x13 x3 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x13 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x33 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractHaskell.C_Symbol x32) x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractHaskell.C_Var x33) (Curry_AbstractHaskell.C_Symbol x32) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x34) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x35) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x36 x37) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x38 x39) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Let x40 x41) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x42) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x43 x44) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Case x45 x46) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x47 x48) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_125 x3 x2 x1 x32 x6 x13 x1002 x3250 x3500) (d_OP__case_125 x3 x2 x1 x32 x6 x13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_125 x3 x2 x1 x32 x6 x13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_125 x3 x2 x1 x32 x6 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_124 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_124 x32 x33 x6 x13 x3 x2 x1 x34 x3250 x3500 = case x34 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 (Curry_AbstractHaskell.C_Symbol x32) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_124 x32 x33 x6 x13 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_124 x32 x33 x6 x13 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_124 x32 x33 x6 x13 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_124 x32 x33 x6 x13 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_127 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_AbstractHaskell.C_Literal -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_127 x3 x2 x1 x15 x6 x13 x10 x3250 x3500 = case x10 of
     (Curry_AbstractHaskell.C_Var x16) -> d_OP__case_126 x15 x16 x6 x13 x3 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x13 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 x16 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isAtom (Curry_AbstractHaskell.C_Lit x15) x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_AbstractHaskell.C_Var x16) (Curry_AbstractHaskell.C_Lit x15) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractHaskell.C_Lit x17) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Symbol x18) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Apply x19 x20) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Lambda x21 x22) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Let x23 x24) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_DoExpr x25) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_ListComp x26 x27) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Case x28 x29) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.C_Typed x30 x31) -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_AbstractHaskell.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_127 x3 x2 x1 x15 x6 x13 x1002 x3250 x3500) (d_OP__case_127 x3 x2 x1 x15 x6 x13 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_127 x3 x2 x1 x15 x6 x13 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_127 x3 x2 x1 x15 x6 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_126 :: Curry_AbstractHaskell.C_Literal -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_126 x15 x16 x6 x13 x3 x2 x1 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 (Curry_AbstractHaskell.C_Lit x15) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_126 x15 x16 x6 x13 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_126 x15 x16 x6 x13 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_126 x15 x16 x6 x13 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_126 x15 x16 x6 x13 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_130 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_130 x10 x14 x13 x3 x2 x1 x6 x11 x15 x3250 x3500 = case x15 of
     Curry_Prelude.C_True -> d_OP__case_129 x14 x6 x10 x3 x2 x1 x11 (Curry_Prelude.d_OP_eq_eq x6 x14 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_130 x10 x14 x13 x3 x2 x1 x6 x11 x1002 x3250 x3500) (d_OP__case_130 x10 x14 x13 x3 x2 x1 x6 x11 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_130 x10 x14 x13 x3 x2 x1 x6 x11 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_130 x10 x14 x13 x3 x2 x1 x6 x11 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_129 :: Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_129 x14 x6 x10 x3 x2 x1 x11 x15 x3250 x3500 = case x15 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showBoxedExpr x1 x10 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x11 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_128 x6 x10 x3 x2 x1 x14 x11 (Curry_Prelude.d_OP_eq_eq x10 (Curry_AbstractHaskell.C_Var x6) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_129 x14 x6 x10 x3 x2 x1 x11 x1002 x3250 x3500) (d_OP__case_129 x14 x6 x10 x3 x2 x1 x11 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_129 x14 x6 x10 x3 x2 x1 x11 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_129 x14 x6 x10 x3 x2 x1 x11 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_128 :: Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Pattern -> C_Options -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_128 x6 x10 x3 x2 x1 x14 x11 x15 x3250 x3500 = case x15 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x11 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showExprOpt x1 (Curry_AbstractHaskell.C_Var x14) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_showLambda x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_128 x6 x10 x3 x2 x1 x14 x11 x1002 x3250 x3500) (d_OP__case_128 x6 x10 x3 x2 x1 x14 x11 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_128 x6 x10 x3 x2 x1 x14 x11 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_128 x6 x10 x3 x2 x1 x14 x11 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_140 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_140 x3 x1 x4 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> d_OP__case_139 x1 x3 x4 (Curry_Prelude.d_OP_eq_eq x3 (d_OP___hash_selR_at_Options_dot_currentModule x1 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_140 x3 x1 x4 x1002 x3250 x3500) (d_OP__case_140 x3 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_140 x3 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_140 x3 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_139 :: C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_139 x1 x3 x4 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> d_OP__case_138 x4 x3 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_139 x1 x3 x4 x1002 x3250 x3500) (d_OP__case_139 x1 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_139 x1 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_139 x1 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_138 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_138 x4 x3 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) x4 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_138 x4 x3 x1002 x3250 x3500) (d_OP__case_138 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_138 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_138 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_141 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_141 x7 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x7 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_showSymbol x1 x7 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_141 x7 x1 x1002 x3250 x3500) (d_OP__case_141 x7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_141 x7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_141 x7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_142 :: Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_142 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_C_apply (d_C_showIdentifier x3250 x3500) x5 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_142 x1002 x3250 x3500) (d_OP__case_142 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_142 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_142 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_143 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_LocalDecl -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_143 x6 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))) (d_C_showBlock (d_C_prefixMap (d_C_showLocalDecl x1) x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_143 x6 x1 x1002 x3250 x3500) (d_OP__case_143 x6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_143 x6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_143 x6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_146 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_AbstractHaskell.C_Expr Curry_AbstractHaskell.C_Expr) -> C_Options -> Curry_Prelude.OP_Tuple2 Curry_AbstractHaskell.C_Expr Curry_AbstractHaskell.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_146 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_145 x5 x4 x6 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 Curry_Prelude.OP_List x3250 x3500) (Curry_Prelude.d_OP_eq_eq x5 (Curry_AbstractHaskell.C_Symbol (Curry_Prelude.OP_Tuple2 (Curry_Names.d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_146 x4 x1 x1002 x3250 x3500) (d_OP__case_146 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_146 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_146 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_145 :: Curry_AbstractHaskell.C_Expr -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_AbstractHaskell.C_Expr Curry_AbstractHaskell.C_Expr) -> Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_145 x5 x4 x6 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (d_C_showExprOpt x1 x6 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_144 x4 x6 x5 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_145 x5 x4 x6 x1 x1002 x3250 x3500) (d_OP__case_145 x5 x4 x6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_145 x5 x4 x6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_145 x5 x4 x6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_144 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_AbstractHaskell.C_Expr Curry_AbstractHaskell.C_Expr) -> Curry_AbstractHaskell.C_Expr -> Curry_AbstractHaskell.C_Expr -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_144 x4 x6 x5 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (d_C_showBlock (d_C_combineMap (d_C_showCrhs x1) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x5 x6) x4) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_144 x4 x6 x5 x1 x1002 x3250 x3500) (d_OP__case_144 x4 x6 x5 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_144 x4 x6 x5 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_144 x4 x6 x5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_147 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_LocalDecl -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_147 x5 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))) (d_C_showBlock (d_C_prefixMap (d_C_showLocalDecl x1) x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_147 x5 x1 x1002 x3250 x3500) (d_OP__case_147 x5 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_147 x5 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_147 x5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_148 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_148 x3 x6 x5 x1 x2 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x2 x6 x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x5 x6 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_148 x3 x6 x5 x1 x2 x1002 x3250 x3500) (d_OP__case_148 x3 x6 x5 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_148 x3 x6 x5 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_148 x3 x6 x5 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_153 :: Curry_AbstractHaskell.C_Rules -> Curry_AbstractHaskell.C_TypeSig -> C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_153 x8 x7 x1 x3 x5 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x9 x10) -> d_OP__case_152 x10 x7 x1 x3 x5 x8 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_153 x8 x7 x1 x3 x5 x1002 x3250 x3500) (d_OP__case_153 x8 x7 x1 x3 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_153 x8 x7 x1 x3 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_153 x8 x7 x1 x3 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_152 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_TypeSig -> C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_AbstractHaskell.C_Rules -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_152 x10 x7 x1 x3 x5 x8 x3250 x3500 = case x8 of
     (Curry_AbstractHaskell.C_Rules x11) -> let
          x12 = Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x10 x3250 x3500
          x13 = d_OP__case_150 x10 x12 x3250 x3500
           in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_funcComment x3250 x3500) x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeSig x1 x10 x7 x3250 x3500) (d_OP__case_151 x10 x11 x1 x5 x13 x12 x3250 x3500) x3250 x3500) x3250 x3500)
     (Curry_AbstractHaskell.C_External x14) -> let
          x15 = d_OP__case_149 x10 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x10 x3250 x3500) x3250 x3500
           in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_funcComment x3250 x3500) x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeSig x1 x10 x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus x15 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))))))) x3250 x3500) x3250 x3500) x3250 x3500)
     (Curry_AbstractHaskell.Choice_C_Rules x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_152 x10 x7 x1 x3 x5 x1002 x3250 x3500) (d_OP__case_152 x10 x7 x1 x3 x5 x1003 x3250 x3500)
     (Curry_AbstractHaskell.Choices_C_Rules x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_152 x10 x7 x1 x3 x5 z x3250 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Rules x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_152 x10 x7 x1 x3 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Rules x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_149 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_149 x10 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x10 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> x10
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_149 x10 x1002 x3250 x3500) (d_OP__case_149 x10 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_149 x10 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_149 x10 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_150 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_150 x10 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x10 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> x10
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_150 x10 x1002 x3250 x3500) (d_OP__case_150 x10 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_150 x10 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_150 x10 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_151 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Rule -> C_Options -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_151 x10 x11 x1 x5 x13 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> d_OP_showFuncDeclOpt_dot_rulePrints_dot_160 x13 x10 x1 x11 x5 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus x10 (d_C_prefixInter (d_C_showRule x1) x11 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x10 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_151 x10 x11 x1 x5 x13 x1002 x3250 x3500) (d_OP__case_151 x10 x11 x1 x5 x13 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_151 x10 x11 x1 x5 x13 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_151 x10 x11 x1 x5 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_154 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_154 x3 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) x3)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x2 x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_154 x3 x2 x1002 x3250 x3500) (d_OP__case_154 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_154 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_154 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_155 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_155 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_155 x2 x1002 x3250 x3500) (d_OP__case_155 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_155 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_155 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_156 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_156 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_156 x2 x1002 x3250 x3500) (d_OP__case_156 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_156 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_156 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_159 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeExpr -> C_Options -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_159 x10 x1 x2 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> d_OP__case_158 x12 x11 x10 x9 x1 x2 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x11 (Curry_Names.d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_159 x10 x1 x2 x1002 x3250 x3500) (d_OP__case_159 x10 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_159 x10 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_159 x10 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_158 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeExpr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_Options -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_158 x12 x11 x10 x9 x1 x2 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_157 x10 x9 x1 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_158 x12 x11 x10 x9 x1 x2 x1002 x3250 x3500) (d_OP__case_158 x12 x11 x10 x9 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_158 x12 x11 x10 x9 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_158 x12 x11 x10 x9 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_157 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeExpr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_Options -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_157 x10 x9 x1 x2 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> d_C_maybeShowBrackets (Curry_Prelude.d_OP_ampersand_ampersand x2 (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x10 x3250 x3500) x3250 x3500) x3250 x3500) (d_C_showTypeCons x1 x9 x10 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_157 x10 x9 x1 x2 x1002 x3250 x3500) (d_OP__case_157 x10 x9 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_157 x10 x9 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_157 x10 x9 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_160 :: Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_160 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_C_showTypeVar (Curry_Prelude.d_C_apply (d_C_showIdentifier x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_160 x1002 x3250 x3500) (d_OP__case_160 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_160 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_160 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_161 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_Context -> C_Options -> Curry_AbstractHaskell.C_Context -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Context -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_161 x2 x1 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_plus_plus (d_C_showClass x1 x3 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) x3250 x3500
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map (d_C_showClass x1) x2 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_161 x2 x1 x3 x1002 x3250 x3500) (d_OP__case_161 x2 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_161 x2 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_161 x2 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_163 :: Curry_Prelude.Curry t0 => Curry_AbstractHaskell.C_Rule -> C_Options -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_163 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_162 x6 x4 x1 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_163 x4 x1 x1002 x3250 x3500) (d_OP__case_163 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_163 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_163 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_162 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractHaskell.C_Rule -> C_Options -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_162 x6 x4 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showRule x1 x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.d_OP_plus_plus (d_C_showRule x1 x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_162 x6 x4 x1 x1002 x3250 x3500) (d_OP__case_162 x6 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_162 x6 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_162 x6 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_165 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_ConsDecl -> C_Options -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_165 x10 x1 x9 x7 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_164 x10 x1 x9 x7 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_165 x10 x1 x9 x7 x1002 x3250 x3500) (d_OP__case_165 x10 x1 x9 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_165 x10 x1 x9 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_165 x10 x1 x9 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_164 :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_ConsDecl -> C_Options -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_164 x10 x1 x9 x7 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (d_C_showSymbol x1 x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_prefixMap (d_C_showTypeExpr x1 Curry_Prelude.C_False) (Curry_Prelude.d_C_map (acceptCs id Curry_AbstractHaskell.C_TVar) x9 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (d_C_showBlock (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_C_combineMap (d_C_showConsDecl x1) x10 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_164 x10 x1 x9 x7 x1002 x3250 x3500) (d_OP__case_164 x10 x1 x9 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_164 x10 x1 x9 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_164 x10 x1 x9 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_167 :: Curry_Prelude.C_Int -> Curry_AbstractHaskell.C_Fixity -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_167 x4 x3 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.d_OP_plus_plus (d_C_showFixity x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (d_OP__case_166 x6 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x6 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_167 x4 x3 x1002 x3250 x3500) (d_OP__case_167 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_167 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_167 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_166 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_166 x6 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_166 x6 x1002 x3250 x3500) (d_OP__case_166 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_166 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_166 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_169 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_169 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) x1 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_168 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_169 x1 x1002 x3250 x3500) (d_OP__case_169 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_169 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_169 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_168 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_168 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) x1 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_168 x1 x1002 x3250 x3500) (d_OP__case_168 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_168 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_168 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_171 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_171 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> d_OP__case_170 x9 (Curry_Prelude.d_C_apply (d_C_isInfixOpName x3250 x3500) x9 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_171 x1002 x3250 x3500) (d_OP__case_171 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_171 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_171 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_170 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_170 x9 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x9 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> x9
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_170 x9 x1002 x3250 x3500) (d_OP__case_170 x9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_170 x9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_170 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_172 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_172 x14 x3250 x3500 = case x14 of
     (Curry_Prelude.OP_Tuple2 x18 x19) -> x19
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_172 x1002 x3250 x3500) (d_OP__case_172 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_172 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_172 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_173 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_173 x8 x3250 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x12 x13) -> x13
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_173 x1002 x3250 x3500) (d_OP__case_173 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_173 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_173 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_174 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_174 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> x7
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_174 x1002 x3250 x3500) (d_OP__case_174 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_174 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_174 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_175 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_175 x6 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x6 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_175 x6 x1002 x3250 x3500) (d_OP__case_175 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_175 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_175 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
