{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Pretty (C_Doc, C_Tokens, d_C_empty, nd_C_empty, d_C_isEmpty, nd_C_isEmpty, d_C_text, nd_C_text, d_C_linesep, nd_C_linesep, d_C_line, nd_C_line, d_C_linebreak, nd_C_linebreak, d_C_softline, nd_C_softline, d_C_softbreak, nd_C_softbreak, d_C_group, nd_C_group, d_C_nest, nd_C_nest, d_C_hang, nd_C_hang, d_C_align, nd_C_align, d_C_combine, nd_C_combine, d_OP_lt_gt, nd_OP_lt_gt, d_OP_lt_plus_gt, nd_OP_lt_plus_gt, d_OP_lt_dollar_gt, nd_OP_lt_dollar_gt, d_OP_lt_slash_gt, nd_OP_lt_slash_gt, d_OP_lt_dollar_dollar_gt, nd_OP_lt_dollar_dollar_gt, d_OP_lt_slash_slash_gt, nd_OP_lt_slash_slash_gt, d_C_compose, nd_C_compose, d_C_hsep, nd_C_hsep, d_C_vsep, nd_C_vsep, d_C_fillSep, nd_C_fillSep, d_C_sep, nd_C_sep, d_C_hcat, nd_C_hcat, d_C_vcat, nd_C_vcat, d_C_fillCat, nd_C_fillCat, d_C_cat, nd_C_cat, d_C_punctuate, nd_C_punctuate, d_C_encloseSep, nd_C_encloseSep, d_C_hEncloseSep, nd_C_hEncloseSep, d_C_fillEncloseSep, nd_C_fillEncloseSep, d_C_list, nd_C_list, d_C_tupled, nd_C_tupled, d_C_semiBraces, nd_C_semiBraces, d_C_enclose, nd_C_enclose, d_C_squotes, nd_C_squotes, d_C_dquotes, nd_C_dquotes, d_C_bquotes, nd_C_bquotes, d_C_parens, nd_C_parens, d_C_angles, nd_C_angles, d_C_braces, nd_C_braces, d_C_brackets, nd_C_brackets, d_C_char, nd_C_char, d_C_string, nd_C_string, d_C_int, nd_C_int, d_C_float, nd_C_float, d_C_lparen, nd_C_lparen, d_C_rparen, nd_C_rparen, d_C_langle, nd_C_langle, d_C_rangle, nd_C_rangle, d_C_lbrace, nd_C_lbrace, d_C_rbrace, nd_C_rbrace, d_C_lbracket, nd_C_lbracket, d_C_rbracket, nd_C_rbracket, d_C_squote, nd_C_squote, d_C_dquote, nd_C_dquote, d_C_semi, nd_C_semi, d_C_colon, nd_C_colon, d_C_comma, nd_C_comma, d_C_space, nd_C_space, d_C_dot, nd_C_dot, d_C_backslash, nd_C_backslash, d_C_equals, nd_C_equals, d_C_pretty, nd_C_pretty) where

import Basics
import qualified Curry_Dequeue
import qualified Curry_Prelude
type C_Layout = Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_Horizontal = Curry_Prelude.C_Bool

type C_Remaining = Curry_Prelude.C_Int

type C_Width = Curry_Prelude.C_Int

type C_Position = Curry_Prelude.C_Int

type C_StartPosition = Curry_Prelude.C_Int

type C_EndPosition = Curry_Prelude.C_Int

type C_Out = Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_OutGroupPrefix = Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_Margins = Curry_Prelude.OP_List Curry_Prelude.C_Int

data C_Doc
     = C_Doc (C_Tokens -> Cover -> ConstStore -> C_Tokens)
     | HO_C_Doc (Func C_Tokens C_Tokens)
     | Choice_C_Doc Cover ID C_Doc C_Doc
     | Choices_C_Doc Cover ID ([C_Doc])
     | Fail_C_Doc Cover FailInfo
     | Guard_C_Doc Cover Constraints C_Doc

instance Show C_Doc where
  showsPrec d (Choice_C_Doc cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Doc cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Doc cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Doc cd info) = showChar '!'
  showsPrec _ (C_Doc x1) = (showString "(Doc") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (HO_C_Doc x1) = (showString "(Doc") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Doc where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Doc x1,r1) | (_,r0) <- readQualified "Pretty" "Doc" r, (x1,r1) <- readsPrec 11 r0]) s


instance NonDet C_Doc where
  choiceCons = Choice_C_Doc
  choicesCons = Choices_C_Doc
  failCons = Fail_C_Doc
  guardCons = Guard_C_Doc
  try (Choice_C_Doc cd i x y) = tryChoice cd i x y
  try (Choices_C_Doc cd i xs) = tryChoices cd i xs
  try (Fail_C_Doc cd info) = Fail cd info
  try (Guard_C_Doc cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Doc cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Doc cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Doc cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Doc cd i _) = error ("Pretty.Doc.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Doc cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Doc cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Doc where
  generate s c = Choices_C_Doc c (freeID [1] s) [(HO_C_Doc (generate (leftSupply s) c))]


instance NormalForm C_Doc where
  ($!!) cont (C_Doc x1) d cs = (((\y1 d cs -> cont (C_Doc y1) d cs) $!! x1) d) cs
  ($!!) cont (HO_C_Doc x1) d cs = (((\y1 d cs -> cont (HO_C_Doc y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Doc cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Doc cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Doc cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Doc cd info) _ _ = failCons cd info
  ($##) cont (C_Doc x1) d cs = (((\y1 d cs -> cont (C_Doc y1) d cs) $## x1) d) cs
  ($##) cont (HO_C_Doc x1) d cs = (((\y1 d cs -> cont (HO_C_Doc y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Doc cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Doc cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Doc cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Doc cd info) _ _ = failCons cd info
  searchNF search cont (C_Doc x1) = search (\y1 -> cont (C_Doc y1)) x1
  searchNF search cont (HO_C_Doc x1) = search (\y1 -> cont (HO_C_Doc y1)) x1
  searchNF _ _ x = error ("Pretty.Doc.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Doc where
  (=.=) (C_Doc x1) (C_Doc y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (HO_C_Doc x1) (HO_C_Doc y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Doc x1) (C_Doc y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (HO_C_Doc x1) (HO_C_Doc y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Doc x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (HO_C_Doc x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Doc cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Doc cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Doc cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Doc cd i _) = error ("Pretty.Doc.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Doc cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Doc cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Doc x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (HO_C_Doc x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Doc cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Doc cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Doc cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Doc cd i _) = error ("Pretty.Doc.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Doc cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Doc cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Doc where
  (=?=) (Choice_C_Doc cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Doc cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Doc cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Doc cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Doc cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Doc cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Doc cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Doc cd info) _ _ = failCons cd info
  (=?=) (C_Doc x1) (C_Doc y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (HO_C_Doc x1) (HO_C_Doc y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (<?=) (Choice_C_Doc cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Doc cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Doc cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Doc cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Doc cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Doc cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Doc cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Doc cd info) _ _ = failCons cd info
  (<?=) (C_Doc x1) (C_Doc y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (HO_C_Doc x1) (HO_C_Doc y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs


data C_Tokens
     = C_Text (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_Tokens
     | C_Line (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_Tokens
     | C_Open C_Tokens
     | C_Close C_Tokens
     | C_Empty
     | C_OpenNest (Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int) C_Tokens
     | HO_C_OpenNest (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Func Curry_Prelude.C_Int (Func Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Int)))) C_Tokens
     | C_CloseNest C_Tokens
     | Choice_C_Tokens Cover ID C_Tokens C_Tokens
     | Choices_C_Tokens Cover ID ([C_Tokens])
     | Fail_C_Tokens Cover FailInfo
     | Guard_C_Tokens Cover Constraints C_Tokens

instance Show C_Tokens where
  showsPrec d (Choice_C_Tokens cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Tokens cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Tokens cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Tokens cd info) = showChar '!'
  showsPrec _ (C_Text x1 x2) = (showString "(Text") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_Line x1 x2) = (showString "(Line") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_Open x1) = (showString "(Open") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Close x1) = (showString "(Close") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ C_Empty = showString "Empty"
  showsPrec _ (C_OpenNest x1 x2) = (showString "(OpenNest") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (HO_C_OpenNest x1 x2) = (showString "(OpenNest") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CloseNest x1) = (showString "(CloseNest") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Tokens where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Text x1 x2,r2) | (_,r0) <- readQualified "Pretty" "Text" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_Line x1 x2,r2) | (_,r0) <- readQualified "Pretty" "Line" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_Open x1,r1) | (_,r0) <- readQualified "Pretty" "Open" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Close x1,r1) | (_,r0) <- readQualified "Pretty" "Close" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen False (\r -> [ (C_Empty,r0) | (_,r0) <- readQualified "Pretty" "Empty" r]) s) ++ ((readParen (d > 10) (\r -> [ (C_OpenNest x1 x2,r2) | (_,r0) <- readQualified "Pretty" "OpenNest" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_CloseNest x1,r1) | (_,r0) <- readQualified "Pretty" "CloseNest" r, (x1,r1) <- readsPrec 11 r0]) s))))))


instance NonDet C_Tokens where
  choiceCons = Choice_C_Tokens
  choicesCons = Choices_C_Tokens
  failCons = Fail_C_Tokens
  guardCons = Guard_C_Tokens
  try (Choice_C_Tokens cd i x y) = tryChoice cd i x y
  try (Choices_C_Tokens cd i xs) = tryChoices cd i xs
  try (Fail_C_Tokens cd info) = Fail cd info
  try (Guard_C_Tokens cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Tokens cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Tokens cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Tokens cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Tokens cd i _) = error ("Pretty.Tokens.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Tokens cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Tokens cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Tokens where
  generate s c = Choices_C_Tokens c (freeID [2,2,1,1,0,2,1] s) [(C_Text (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_Line (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_Open (generate (leftSupply s) c)),(C_Close (generate (leftSupply s) c)),C_Empty,(HO_C_OpenNest (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CloseNest (generate (leftSupply s) c))]


instance NormalForm C_Tokens where
  ($!!) cont (C_Text x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Text y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Line x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Line y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Open x1) d cs = (((\y1 d cs -> cont (C_Open y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Close x1) d cs = (((\y1 d cs -> cont (C_Close y1) d cs) $!! x1) d) cs
  ($!!) cont C_Empty d cs = cont C_Empty d cs
  ($!!) cont (C_OpenNest x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_OpenNest y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_OpenNest x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (HO_C_OpenNest y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CloseNest x1) d cs = (((\y1 d cs -> cont (C_CloseNest y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Tokens cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Tokens cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Tokens cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Tokens cd info) _ _ = failCons cd info
  ($##) cont (C_Text x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Text y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Line x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Line y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Open x1) d cs = (((\y1 d cs -> cont (C_Open y1) d cs) $## x1) d) cs
  ($##) cont (C_Close x1) d cs = (((\y1 d cs -> cont (C_Close y1) d cs) $## x1) d) cs
  ($##) cont C_Empty d cs = cont C_Empty d cs
  ($##) cont (C_OpenNest x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_OpenNest y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_OpenNest x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (HO_C_OpenNest y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CloseNest x1) d cs = (((\y1 d cs -> cont (C_CloseNest y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Tokens cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Tokens cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Tokens cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Tokens cd info) _ _ = failCons cd info
  searchNF search cont (C_Text x1 x2) = search (\y1 -> search (\y2 -> cont (C_Text y1 y2)) x2) x1
  searchNF search cont (C_Line x1 x2) = search (\y1 -> search (\y2 -> cont (C_Line y1 y2)) x2) x1
  searchNF search cont (C_Open x1) = search (\y1 -> cont (C_Open y1)) x1
  searchNF search cont (C_Close x1) = search (\y1 -> cont (C_Close y1)) x1
  searchNF _ cont C_Empty = cont C_Empty
  searchNF search cont (C_OpenNest x1 x2) = search (\y1 -> search (\y2 -> cont (C_OpenNest y1 y2)) x2) x1
  searchNF search cont (HO_C_OpenNest x1 x2) = search (\y1 -> search (\y2 -> cont (HO_C_OpenNest y1 y2)) x2) x1
  searchNF search cont (C_CloseNest x1) = search (\y1 -> cont (C_CloseNest y1)) x1
  searchNF _ _ x = error ("Pretty.Tokens.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Tokens where
  (=.=) (C_Text x1 x2) (C_Text y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_Line x1 x2) (C_Line y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_Open x1) (C_Open y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Close x1) (C_Close y1) d cs = ((x1 =:= y1) d) cs
  (=.=) C_Empty C_Empty d cs = C_Success
  (=.=) (C_OpenNest x1 x2) (C_OpenNest y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (HO_C_OpenNest x1 x2) (HO_C_OpenNest y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CloseNest x1) (C_CloseNest y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Text x1 x2) (C_Text y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_Line x1 x2) (C_Line y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_Open x1) (C_Open y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Close x1) (C_Close y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) C_Empty C_Empty d cs = C_Success
  (=.<=) (C_OpenNest x1 x2) (C_OpenNest y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (HO_C_OpenNest x1 x2) (HO_C_OpenNest y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CloseNest x1) (C_CloseNest y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Text x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_Line x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_Open x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Close x3) = ((i :=: (ChooseN 3 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i C_Empty = ((i :=: (ChooseN 4 0)):(concat []))
  bind cd i (C_OpenNest x3 x4) = ((i :=: (ChooseN 5 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (HO_C_OpenNest x3 x4) = ((i :=: (ChooseN 5 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CloseNest x3) = ((i :=: (ChooseN 6 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Tokens cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Tokens cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Tokens cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Tokens cd i _) = error ("Pretty.Tokens.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Tokens cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Tokens cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Text x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_Line x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_Open x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Close x3) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i C_Empty = [(i :=: (ChooseN 4 0))]
  lazyBind cd i (C_OpenNest x3 x4) = [(i :=: (ChooseN 5 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (HO_C_OpenNest x3 x4) = [(i :=: (ChooseN 5 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CloseNest x3) = [(i :=: (ChooseN 6 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Tokens cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Tokens cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Tokens cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Tokens cd i _) = error ("Pretty.Tokens.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Tokens cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Tokens cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Tokens where
  (=?=) (Choice_C_Tokens cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Tokens cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Tokens cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Tokens cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Tokens cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Tokens cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Tokens cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Tokens cd info) _ _ = failCons cd info
  (=?=) (C_Text x1 x2) (C_Text y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_Line x1 x2) (C_Line y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_Open x1) (C_Open y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Close x1) (C_Close y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) C_Empty C_Empty d cs = Curry_Prelude.C_True
  (=?=) (C_OpenNest x1 x2) (C_OpenNest y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (HO_C_OpenNest x1 x2) (HO_C_OpenNest y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CloseNest x1) (C_CloseNest y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Tokens cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Tokens cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Tokens cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Tokens cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Tokens cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Tokens cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Tokens cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Tokens cd info) _ _ = failCons cd info
  (<?=) (C_Text x1 x2) (C_Text y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Text _ _) (C_Line _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _ _) (C_Open _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _ _) (C_Close _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _ _) C_Empty _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _ _) (C_OpenNest _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _ _) (HO_C_OpenNest _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _ _) (C_CloseNest _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Line x1 x2) (C_Line y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Line _ _) (C_Open _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Line _ _) (C_Close _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Line _ _) C_Empty _ _ = Curry_Prelude.C_True
  (<?=) (C_Line _ _) (C_OpenNest _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Line _ _) (HO_C_OpenNest _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Line _ _) (C_CloseNest _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Open x1) (C_Open y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Open _) (C_Close _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Open _) C_Empty _ _ = Curry_Prelude.C_True
  (<?=) (C_Open _) (C_OpenNest _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Open _) (HO_C_OpenNest _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Open _) (C_CloseNest _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Close x1) (C_Close y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Close _) C_Empty _ _ = Curry_Prelude.C_True
  (<?=) (C_Close _) (C_OpenNest _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Close _) (HO_C_OpenNest _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Close _) (C_CloseNest _) _ _ = Curry_Prelude.C_True
  (<?=) C_Empty C_Empty d cs = Curry_Prelude.C_True
  (<?=) C_Empty (C_OpenNest _ _) _ _ = Curry_Prelude.C_True
  (<?=) C_Empty (HO_C_OpenNest _ _) _ _ = Curry_Prelude.C_True
  (<?=) C_Empty (C_CloseNest _) _ _ = Curry_Prelude.C_True
  (<?=) (C_OpenNest x1 x2) (C_OpenNest y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_OpenNest _ _) (C_CloseNest _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_OpenNest x1 x2) (HO_C_OpenNest y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (HO_C_OpenNest _ _) (C_CloseNest _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CloseNest x1) (C_CloseNest y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_deDoc :: C_Doc -> Cover -> ConstStore -> C_Tokens -> Cover -> ConstStore -> C_Tokens
d_C_deDoc x1 x3250 x3500 = case x1 of
     (C_Doc x2) -> x2
     (Choice_C_Doc x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deDoc x1002 x3250 x3500) (d_C_deDoc x1003 x3250 x3500)
     (Choices_C_Doc x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deDoc z x3250 x3500) x1002
     (Guard_C_Doc x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deDoc x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Doc x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_deDoc :: C_Doc -> IDSupply -> Cover -> ConstStore -> Func C_Tokens C_Tokens
nd_C_deDoc x1 x3000 x3250 x3500 = case x1 of
     (HO_C_Doc x2) -> x2
     (Choice_C_Doc x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_deDoc x1002 x3000 x3250 x3500) (nd_C_deDoc x1003 x3000 x3250 x3500)
     (Choices_C_Doc x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_deDoc z x3000 x3250 x3500) x1002
     (Guard_C_Doc x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_deDoc x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Doc x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_empty :: Cover -> ConstStore -> C_Doc
d_C_empty x3250 x3500 = d_C_text Curry_Prelude.OP_List x3250 x3500

nd_C_empty :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_empty x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_text Curry_Prelude.OP_List x2000 x3250 x3500))

d_C_isEmpty :: C_Doc -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isEmpty x1 x3250 x3500 = case x1 of
     (C_Doc x2) -> Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_apply x2 C_Empty x3250 x3500) (C_Text Curry_Prelude.OP_List C_Empty) x3250 x3500
     (Choice_C_Doc x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isEmpty x1002 x3250 x3500) (d_C_isEmpty x1003 x3250 x3500)
     (Choices_C_Doc x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isEmpty z x3250 x3500) x1002
     (Guard_C_Doc x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isEmpty x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Doc x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_isEmpty :: C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_C_isEmpty x1 x3000 x3250 x3500 = case x1 of
     (HO_C_Doc x2) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.nd_C_apply x2 C_Empty x2000 x3250 x3500) (C_Text Curry_Prelude.OP_List C_Empty) x3250 x3500))
     (Choice_C_Doc x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_isEmpty x1002 x3000 x3250 x3500) (nd_C_isEmpty x1003 x3000 x3250 x3500)
     (Choices_C_Doc x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_isEmpty z x3000 x3250 x3500) x1002
     (Guard_C_Doc x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_isEmpty x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Doc x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_text :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_Doc
d_C_text x1 x3250 x3500 = C_Doc (acceptCs id (C_Text x1))

nd_C_text :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_text x1 x3000 x3250 x3500 = HO_C_Doc (wrapDX id (acceptCs id (C_Text x1)))

d_C_linesep :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_Doc
d_C_linesep x3250 x3500 = Curry_Prelude.d_OP_dot (acceptCs id C_Doc) (acceptCs (acceptCs id) C_Line) x3250 x3500

nd_C_linesep :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_Doc
nd_C_linesep x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id (acceptCs id HO_C_Doc)) (wrapDX (wrapDX id) (acceptCs (acceptCs id) C_Line)) x2000 x3250 x3500))

d_C_line :: Cover -> ConstStore -> C_Doc
d_C_line x3250 x3500 = Curry_Prelude.d_C_apply (d_C_linesep x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3250 x3500

nd_C_line :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_line x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_linesep x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x2001 x3250 x3500)))))

d_C_linebreak :: Cover -> ConstStore -> C_Doc
d_C_linebreak x3250 x3500 = Curry_Prelude.d_C_apply (d_C_linesep x3250 x3500) Curry_Prelude.OP_List x3250 x3500

nd_C_linebreak :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_linebreak x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_linesep x2000 x3250 x3500) Curry_Prelude.OP_List x2001 x3250 x3500)))))

d_C_softline :: Cover -> ConstStore -> C_Doc
d_C_softline x3250 x3500 = d_C_group (d_C_line x3250 x3500) x3250 x3500

nd_C_softline :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_softline x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_group (nd_C_line x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_softbreak :: Cover -> ConstStore -> C_Doc
d_C_softbreak x3250 x3500 = d_C_group (d_C_linebreak x3250 x3500) x3250 x3500

nd_C_softbreak :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_softbreak x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_group (nd_C_linebreak x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_group :: C_Doc -> Cover -> ConstStore -> C_Doc
d_C_group x1 x3250 x3500 = C_Doc (Curry_Prelude.d_OP_dot (acceptCs id C_Open) (Curry_Prelude.d_OP_dot (d_C_deDoc x1 x3250 x3500) (acceptCs id C_Close) x3250 x3500) x3250 x3500)

nd_C_group :: C_Doc -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_group x1 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (HO_C_Doc (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dot (wrapDX id (acceptCs id C_Open)) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (nd_C_deDoc x1 x2000 x3250 x3500) (wrapDX id (acceptCs id C_Close)) x2001 x3250 x3500)))) x2003 x3250 x3500))))))

d_C_nest :: Curry_Prelude.C_Int -> C_Doc -> Cover -> ConstStore -> C_Doc
d_C_nest x1 x2 x3250 x3500 = C_Doc (Curry_Prelude.d_OP_dot (acceptCs id (C_OpenNest (acceptCs (acceptCs id) (d_OP_nest_dot___hash_lambda1 x1)))) (Curry_Prelude.d_OP_dot (d_C_deDoc x2 x3250 x3500) (acceptCs id C_CloseNest) x3250 x3500) x3250 x3500)

nd_C_nest :: Curry_Prelude.C_Int -> C_Doc -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_nest x1 x2 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (HO_C_Doc (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dot (wrapDX id (acceptCs id (HO_C_OpenNest (wrapDX (wrapDX (wrapDX id)) (acceptCs (acceptCs id) (d_OP_nest_dot___hash_lambda1 x1)))))) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (nd_C_deDoc x2 x2000 x3250 x3500) (wrapDX id (acceptCs id C_CloseNest)) x2001 x3250 x3500)))) x2003 x3250 x3500))))))

d_OP_nest_dot___hash_lambda1 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_nest_dot___hash_lambda1 x1 x2 x3 x4 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_plus x5 x1 x3250 x3500) x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_nest_dot___hash_lambda1 x1 x1002 x3 x4 x3250 x3500) (d_OP_nest_dot___hash_lambda1 x1 x1003 x3 x4 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_nest_dot___hash_lambda1 x1 z x3 x4 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_nest_dot___hash_lambda1 x1 x1002 x3 x4 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_hang :: Curry_Prelude.C_Int -> C_Doc -> Cover -> ConstStore -> C_Doc
d_C_hang x1 x2 x3250 x3500 = C_Doc (Curry_Prelude.d_OP_dot (acceptCs id (C_OpenNest (acceptCs (acceptCs id) (d_OP_hang_dot___hash_lambda2 x1)))) (Curry_Prelude.d_OP_dot (d_C_deDoc x2 x3250 x3500) (acceptCs id C_CloseNest) x3250 x3500) x3250 x3500)

nd_C_hang :: Curry_Prelude.C_Int -> C_Doc -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_hang x1 x2 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (HO_C_Doc (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dot (wrapDX id (acceptCs id (HO_C_OpenNest (wrapDX (wrapDX (wrapDX id)) (acceptCs (acceptCs id) (d_OP_hang_dot___hash_lambda2 x1)))))) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (nd_C_deDoc x2 x2000 x3250 x3500) (wrapDX id (acceptCs id C_CloseNest)) x2001 x3250 x3500)))) x2003 x3250 x3500))))))

d_OP_hang_dot___hash_lambda2 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_hang_dot___hash_lambda2 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_minus x4 x3 x3250 x3500) x1 x3250 x3500) x2

d_C_align :: Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc
d_C_align x3250 x3500 = d_C_hang (Curry_Prelude.C_Int 0#)

nd_C_align :: IDSupply -> Cover -> ConstStore -> Func C_Doc C_Doc
nd_C_align x3000 x3250 x3500 = wrapNX id (nd_C_hang (Curry_Prelude.C_Int 0#))

d_C_combine :: C_Doc -> C_Doc -> C_Doc -> Cover -> ConstStore -> C_Doc
d_C_combine x1 x2 x3 x3250 x3500 = d_C_enclose x2 x3 x1 x3250 x3500

nd_C_combine :: C_Doc -> C_Doc -> C_Doc -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_combine x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_enclose x2 x3 x1 x2000 x3250 x3500))

d_OP_lt_gt :: C_Doc -> C_Doc -> Cover -> ConstStore -> C_Doc
d_OP_lt_gt x1 x2 x3250 x3500 = C_Doc (Curry_Prelude.d_OP_dot (d_C_deDoc x1 x3250 x3500) (d_C_deDoc x2 x3250 x3500) x3250 x3500)

nd_OP_lt_gt :: C_Doc -> C_Doc -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_OP_lt_gt x1 x2 x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (HO_C_Doc (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_dot (nd_C_deDoc x1 x2000 x3250 x3500) (nd_C_deDoc x2 x2001 x3250 x3500) x2002 x3250 x3500)))))))))

d_OP_lt_plus_gt :: Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc
d_OP_lt_plus_gt x3250 x3500 = acceptCs id (d_C_combine (d_C_space x3250 x3500))

nd_OP_lt_plus_gt :: IDSupply -> Cover -> ConstStore -> Func C_Doc (Func C_Doc C_Doc)
nd_OP_lt_plus_gt x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapDX (wrapNX id) (acceptCs id (nd_C_combine (nd_C_space x2000 x3250 x3500)))))

d_OP_lt_dollar_gt :: Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc
d_OP_lt_dollar_gt x3250 x3500 = acceptCs id (d_C_combine (d_C_line x3250 x3500))

nd_OP_lt_dollar_gt :: IDSupply -> Cover -> ConstStore -> Func C_Doc (Func C_Doc C_Doc)
nd_OP_lt_dollar_gt x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapDX (wrapNX id) (acceptCs id (nd_C_combine (nd_C_line x2000 x3250 x3500)))))

d_OP_lt_slash_gt :: Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc
d_OP_lt_slash_gt x3250 x3500 = acceptCs id (d_C_combine (d_C_softline x3250 x3500))

nd_OP_lt_slash_gt :: IDSupply -> Cover -> ConstStore -> Func C_Doc (Func C_Doc C_Doc)
nd_OP_lt_slash_gt x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapDX (wrapNX id) (acceptCs id (nd_C_combine (nd_C_softline x2000 x3250 x3500)))))

d_OP_lt_dollar_dollar_gt :: Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc
d_OP_lt_dollar_dollar_gt x3250 x3500 = acceptCs id (d_C_combine (d_C_linebreak x3250 x3500))

nd_OP_lt_dollar_dollar_gt :: IDSupply -> Cover -> ConstStore -> Func C_Doc (Func C_Doc C_Doc)
nd_OP_lt_dollar_dollar_gt x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapDX (wrapNX id) (acceptCs id (nd_C_combine (nd_C_linebreak x2000 x3250 x3500)))))

d_OP_lt_slash_slash_gt :: Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc
d_OP_lt_slash_slash_gt x3250 x3500 = acceptCs id (d_C_combine (d_C_softbreak x3250 x3500))

nd_OP_lt_slash_slash_gt :: IDSupply -> Cover -> ConstStore -> Func C_Doc (Func C_Doc C_Doc)
nd_OP_lt_slash_slash_gt x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapDX (wrapNX id) (acceptCs id (nd_C_combine (nd_C_softbreak x2000 x3250 x3500)))))

d_C_compose :: (C_Doc -> Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc) -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_compose x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_C_empty x3250 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.d_C_foldr1 x1 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_compose x1 x1002 x3250 x3500) (d_C_compose x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_compose x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_compose x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_compose :: Func C_Doc (Func C_Doc C_Doc) -> Curry_Prelude.OP_List C_Doc -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_compose x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_C_empty x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_foldr1 x1 x2 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_compose x1 x1002 x3000 x3250 x3500) (nd_C_compose x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_compose x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_compose x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_hsep :: Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_hsep x3250 x3500 = d_C_compose (d_OP_lt_plus_gt x3250 x3500)

nd_C_hsep :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List C_Doc) C_Doc
nd_C_hsep x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (nd_C_compose (nd_OP_lt_plus_gt x2000 x3250 x3500))))

d_C_vsep :: Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_vsep x3250 x3500 = d_C_compose (d_OP_lt_dollar_gt x3250 x3500)

nd_C_vsep :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List C_Doc) C_Doc
nd_C_vsep x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (nd_C_compose (nd_OP_lt_dollar_gt x2000 x3250 x3500))))

d_C_fillSep :: Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_fillSep x3250 x3500 = d_C_compose (d_OP_lt_slash_gt x3250 x3500)

nd_C_fillSep :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List C_Doc) C_Doc
nd_C_fillSep x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (nd_C_compose (nd_OP_lt_slash_gt x2000 x3250 x3500))))

d_C_sep :: Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_sep x3250 x3500 = Curry_Prelude.d_OP_dot d_C_group (d_C_vsep x3250 x3500) x3250 x3500

nd_C_sep :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List C_Doc) C_Doc
nd_C_sep x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id nd_C_group) (nd_C_vsep x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_hcat :: Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_hcat x3250 x3500 = d_C_compose (acceptCs id d_OP_lt_gt)

nd_C_hcat :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List C_Doc) C_Doc
nd_C_hcat x3000 x3250 x3500 = wrapNX id (nd_C_compose (wrapDX (wrapNX id) (acceptCs id nd_OP_lt_gt)))

d_C_vcat :: Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_vcat x3250 x3500 = d_C_compose (d_OP_lt_dollar_dollar_gt x3250 x3500)

nd_C_vcat :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List C_Doc) C_Doc
nd_C_vcat x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (nd_C_compose (nd_OP_lt_dollar_dollar_gt x2000 x3250 x3500))))

d_C_fillCat :: Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_fillCat x3250 x3500 = d_C_compose (d_OP_lt_slash_slash_gt x3250 x3500)

nd_C_fillCat :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List C_Doc) C_Doc
nd_C_fillCat x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (nd_C_compose (nd_OP_lt_slash_slash_gt x2000 x3250 x3500))))

d_C_cat :: Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_cat x3250 x3500 = Curry_Prelude.d_OP_dot d_C_group (d_C_vcat x3250 x3500) x3250 x3500

nd_C_cat :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List C_Doc) C_Doc
nd_C_cat x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id nd_C_group) (nd_C_vcat x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_punctuate :: C_Doc -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc
d_C_punctuate x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP_punctuate_dot_go_dot_76 x1 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_punctuate x1 x1002 x3250 x3500) (d_C_punctuate x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_punctuate x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_punctuate x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_punctuate :: C_Doc -> Curry_Prelude.OP_List C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc
nd_C_punctuate x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_punctuate_dot_go_dot_76 x1 x2 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_punctuate x1 x1002 x3000 x3250 x3500) (nd_C_punctuate x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_punctuate x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_punctuate x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_punctuate_dot_go_dot_76 :: C_Doc -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc
d_OP_punctuate_dot_go_dot_76 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_13 x1 x3 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_punctuate_dot_go_dot_76 x1 x1002 x3250 x3500) (d_OP_punctuate_dot_go_dot_76 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_punctuate_dot_go_dot_76 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_punctuate_dot_go_dot_76 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_punctuate_dot_go_dot_76 :: C_Doc -> Curry_Prelude.OP_List C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc
nd_OP_punctuate_dot_go_dot_76 x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_13 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_punctuate_dot_go_dot_76 x1 x1002 x3000 x3250 x3500) (nd_OP_punctuate_dot_go_dot_76 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_punctuate_dot_go_dot_76 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_punctuate_dot_go_dot_76 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_encloseSep :: C_Doc -> C_Doc -> C_Doc -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_encloseSep x1 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> d_OP_lt_gt x1 x2 x3250 x3500
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.d_C_apply (d_C_align x3250 x3500) (d_C_enclose x1 x2 (Curry_Prelude.d_C_apply (d_C_cat x3250 x3500) (Curry_Prelude.OP_Cons x5 (Curry_Prelude.d_C_map (d_OP_lt_gt x3) x6 x3250 x3500)) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_encloseSep x1 x2 x3 x1002 x3250 x3500) (d_C_encloseSep x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_encloseSep x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_encloseSep x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_encloseSep :: C_Doc -> C_Doc -> C_Doc -> Curry_Prelude.OP_List C_Doc -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_encloseSep x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_lt_gt x1 x2 x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2009 = x3000
           in (seq x2009 (let
               x2008 = leftSupply x2009
               x2010 = rightSupply x2009
                in (seq x2008 (seq x2010 (let
                    x2000 = leftSupply x2010
                    x2007 = rightSupply x2010
                     in (seq x2000 (seq x2007 (Curry_Prelude.nd_C_apply (nd_C_align x2000 x3250 x3500) (let
                         x2006 = leftSupply x2007
                         x2004 = rightSupply x2007
                          in (seq x2006 (seq x2004 (nd_C_enclose x1 x2 (let
                              x2003 = leftSupply x2004
                              x2005 = rightSupply x2004
                               in (seq x2003 (seq x2005 (let
                                   x2001 = leftSupply x2005
                                   x2002 = rightSupply x2005
                                    in (seq x2001 (seq x2002 (Curry_Prelude.nd_C_apply (nd_C_cat x2001 x3250 x3500) (Curry_Prelude.OP_Cons x5 (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_lt_gt x3)) x6 x2002 x3250 x3500)) x2003 x3250 x3500))))))) x2006 x3250 x3500)))) x2008 x3250 x3500))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_encloseSep x1 x2 x3 x1002 x3000 x3250 x3500) (nd_C_encloseSep x1 x2 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_encloseSep x1 x2 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_encloseSep x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_hEncloseSep :: C_Doc -> C_Doc -> C_Doc -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_hEncloseSep x1 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> d_OP_lt_gt x1 x2 x3250 x3500
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.d_C_apply (d_C_align x3250 x3500) (d_C_enclose x1 x2 (Curry_Prelude.d_C_apply (d_C_hcat x3250 x3500) (Curry_Prelude.OP_Cons x5 (Curry_Prelude.d_C_map (d_OP_lt_gt x3) x6 x3250 x3500)) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_hEncloseSep x1 x2 x3 x1002 x3250 x3500) (d_C_hEncloseSep x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_hEncloseSep x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_hEncloseSep x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_hEncloseSep :: C_Doc -> C_Doc -> C_Doc -> Curry_Prelude.OP_List C_Doc -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_hEncloseSep x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_lt_gt x1 x2 x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2009 = x3000
           in (seq x2009 (let
               x2008 = leftSupply x2009
               x2010 = rightSupply x2009
                in (seq x2008 (seq x2010 (let
                    x2000 = leftSupply x2010
                    x2007 = rightSupply x2010
                     in (seq x2000 (seq x2007 (Curry_Prelude.nd_C_apply (nd_C_align x2000 x3250 x3500) (let
                         x2006 = leftSupply x2007
                         x2004 = rightSupply x2007
                          in (seq x2006 (seq x2004 (nd_C_enclose x1 x2 (let
                              x2003 = leftSupply x2004
                              x2005 = rightSupply x2004
                               in (seq x2003 (seq x2005 (let
                                   x2001 = leftSupply x2005
                                   x2002 = rightSupply x2005
                                    in (seq x2001 (seq x2002 (Curry_Prelude.nd_C_apply (nd_C_hcat x2001 x3250 x3500) (Curry_Prelude.OP_Cons x5 (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_lt_gt x3)) x6 x2002 x3250 x3500)) x2003 x3250 x3500))))))) x2006 x3250 x3500)))) x2008 x3250 x3500))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_hEncloseSep x1 x2 x3 x1002 x3000 x3250 x3500) (nd_C_hEncloseSep x1 x2 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_hEncloseSep x1 x2 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_hEncloseSep x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_fillEncloseSep :: C_Doc -> C_Doc -> C_Doc -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_fillEncloseSep x1 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> d_OP_lt_gt x1 x2 x3250 x3500
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.d_C_apply (d_C_align x3250 x3500) (d_C_enclose x1 x2 (Curry_Prelude.d_C_apply (d_C_hcat x3250 x3500) (Curry_Prelude.OP_Cons x5 (d_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 (Curry_Prelude.d_C_map (d_OP_lt_gt x3) x6 x3250 x3500) x3250 x3500)) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fillEncloseSep x1 x2 x3 x1002 x3250 x3500) (d_C_fillEncloseSep x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fillEncloseSep x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fillEncloseSep x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_fillEncloseSep :: C_Doc -> C_Doc -> C_Doc -> Curry_Prelude.OP_List C_Doc -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_fillEncloseSep x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_lt_gt x1 x2 x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2011 = x3000
           in (seq x2011 (let
               x2010 = leftSupply x2011
               x2012 = rightSupply x2011
                in (seq x2010 (seq x2012 (let
                    x2000 = leftSupply x2012
                    x2009 = rightSupply x2012
                     in (seq x2000 (seq x2009 (Curry_Prelude.nd_C_apply (nd_C_align x2000 x3250 x3500) (let
                         x2008 = leftSupply x2009
                         x2006 = rightSupply x2009
                          in (seq x2008 (seq x2006 (nd_C_enclose x1 x2 (let
                              x2005 = leftSupply x2006
                              x2007 = rightSupply x2006
                               in (seq x2005 (seq x2007 (let
                                   x2001 = leftSupply x2007
                                   x2004 = rightSupply x2007
                                    in (seq x2001 (seq x2004 (Curry_Prelude.nd_C_apply (nd_C_hcat x2001 x3250 x3500) (Curry_Prelude.OP_Cons x5 (let
                                        x2003 = leftSupply x2004
                                        x2002 = rightSupply x2004
                                         in (seq x2003 (seq x2002 (nd_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_lt_gt x3)) x6 x2002 x3250 x3500) x2003 x3250 x3500))))) x2005 x3250 x3500))))))) x2008 x3250 x3500)))) x2010 x3250 x3500))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_fillEncloseSep x1 x2 x3 x1002 x3000 x3250 x3500) (nd_C_fillEncloseSep x1 x2 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_fillEncloseSep x1 x2 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_fillEncloseSep x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 :: Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc
d_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_12 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 x1002 x3250 x3500) (d_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 :: Curry_Prelude.OP_List C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc
nd_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 x1 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x2 x3 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 x1002 x3000 x3250 x3500) (nd_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_list :: Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_list x3250 x3500 = d_C_fillEncloseSep (d_C_lbracket x3250 x3500) (d_C_rbracket x3250 x3500) (d_C_comma x3250 x3500)

nd_C_list :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List C_Doc) C_Doc
nd_C_list x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2000 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2000 (seq x2004 (let
               x2001 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2001 (seq x2002 (wrapNX id (nd_C_fillEncloseSep (nd_C_lbracket x2000 x3250 x3500) (nd_C_rbracket x2001 x3250 x3500) (nd_C_comma x2002 x3250 x3500))))))))))

d_C_tupled :: Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_tupled x3250 x3500 = d_C_fillEncloseSep (d_C_lparen x3250 x3500) (d_C_rparen x3250 x3500) (d_C_comma x3250 x3500)

nd_C_tupled :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List C_Doc) C_Doc
nd_C_tupled x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2000 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2000 (seq x2004 (let
               x2001 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2001 (seq x2002 (wrapNX id (nd_C_fillEncloseSep (nd_C_lparen x2000 x3250 x3500) (nd_C_rparen x2001 x3250 x3500) (nd_C_comma x2002 x3250 x3500))))))))))

d_C_semiBraces :: Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> C_Doc
d_C_semiBraces x3250 x3500 = d_C_fillEncloseSep (d_C_lbrace x3250 x3500) (d_C_rbrace x3250 x3500) (d_C_semi x3250 x3500)

nd_C_semiBraces :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List C_Doc) C_Doc
nd_C_semiBraces x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2000 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2000 (seq x2004 (let
               x2001 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2001 (seq x2002 (wrapNX id (nd_C_fillEncloseSep (nd_C_lbrace x2000 x3250 x3500) (nd_C_rbrace x2001 x3250 x3500) (nd_C_semi x2002 x3250 x3500))))))))))

d_C_enclose :: C_Doc -> C_Doc -> C_Doc -> Cover -> ConstStore -> C_Doc
d_C_enclose x1 x2 x3 x3250 x3500 = d_OP_lt_gt (d_OP_lt_gt x1 x3 x3250 x3500) x2 x3250 x3500

nd_C_enclose :: C_Doc -> C_Doc -> C_Doc -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_enclose x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_lt_gt (nd_OP_lt_gt x1 x3 x2000 x3250 x3500) x2 x2001 x3250 x3500)))))

d_C_squotes :: Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc
d_C_squotes x3250 x3500 = d_C_enclose (d_C_squote x3250 x3500) (d_C_squote x3250 x3500)

nd_C_squotes :: IDSupply -> Cover -> ConstStore -> Func C_Doc C_Doc
nd_C_squotes x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (wrapNX id (nd_C_enclose (nd_C_squote x2000 x3250 x3500) (nd_C_squote x2001 x3250 x3500)))))))

d_C_dquotes :: Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc
d_C_dquotes x3250 x3500 = d_C_enclose (d_C_dquote x3250 x3500) (d_C_dquote x3250 x3500)

nd_C_dquotes :: IDSupply -> Cover -> ConstStore -> Func C_Doc C_Doc
nd_C_dquotes x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (wrapNX id (nd_C_enclose (nd_C_dquote x2000 x3250 x3500) (nd_C_dquote x2001 x3250 x3500)))))))

d_C_bquotes :: Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc
d_C_bquotes x3250 x3500 = d_C_enclose (d_C_bquote x3250 x3500) (d_C_bquote x3250 x3500)

nd_C_bquotes :: IDSupply -> Cover -> ConstStore -> Func C_Doc C_Doc
nd_C_bquotes x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (wrapNX id (nd_C_enclose (nd_C_bquote x2000 x3250 x3500) (nd_C_bquote x2001 x3250 x3500)))))))

d_C_parens :: Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc
d_C_parens x3250 x3500 = d_C_enclose (d_C_lparen x3250 x3500) (d_C_rparen x3250 x3500)

nd_C_parens :: IDSupply -> Cover -> ConstStore -> Func C_Doc C_Doc
nd_C_parens x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (wrapNX id (nd_C_enclose (nd_C_lparen x2000 x3250 x3500) (nd_C_rparen x2001 x3250 x3500)))))))

d_C_angles :: Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc
d_C_angles x3250 x3500 = d_C_enclose (d_C_langle x3250 x3500) (d_C_rangle x3250 x3500)

nd_C_angles :: IDSupply -> Cover -> ConstStore -> Func C_Doc C_Doc
nd_C_angles x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (wrapNX id (nd_C_enclose (nd_C_langle x2000 x3250 x3500) (nd_C_rangle x2001 x3250 x3500)))))))

d_C_braces :: Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc
d_C_braces x3250 x3500 = d_C_enclose (d_C_lbrace x3250 x3500) (d_C_rbrace x3250 x3500)

nd_C_braces :: IDSupply -> Cover -> ConstStore -> Func C_Doc C_Doc
nd_C_braces x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (wrapNX id (nd_C_enclose (nd_C_lbrace x2000 x3250 x3500) (nd_C_rbrace x2001 x3250 x3500)))))))

d_C_brackets :: Cover -> ConstStore -> C_Doc -> Cover -> ConstStore -> C_Doc
d_C_brackets x3250 x3500 = d_C_enclose (d_C_lbracket x3250 x3500) (d_C_rbracket x3250 x3500)

nd_C_brackets :: IDSupply -> Cover -> ConstStore -> Func C_Doc C_Doc
nd_C_brackets x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (wrapNX id (nd_C_enclose (nd_C_lbracket x2000 x3250 x3500) (nd_C_rbracket x2001 x3250 x3500)))))))

d_C_char :: Curry_Prelude.C_Char -> Cover -> ConstStore -> C_Doc
d_C_char x1 x3250 x3500 = d_C_text (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) x3250 x3500

nd_C_char :: Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_char x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_text (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) x2000 x3250 x3500))

d_C_string :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_Doc
d_C_string x3250 x3500 = Curry_Prelude.d_OP_dot (d_C_hcat x3250 x3500) (Curry_Prelude.d_C_map d_OP_string_dot___hash_lambda3) x3250 x3500

nd_C_string :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_Doc
nd_C_string x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (nd_C_hcat x2000 x3250 x3500) (wrapNX id (Curry_Prelude.nd_C_map (wrapNX id nd_OP_string_dot___hash_lambda3))) x2001 x3250 x3500)))))

d_OP_string_dot___hash_lambda3 :: Curry_Prelude.C_Char -> Cover -> ConstStore -> C_Doc
d_OP_string_dot___hash_lambda3 x1 x3250 x3500 = d_OP__case_11 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\r'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500

nd_OP_string_dot___hash_lambda3 :: Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_OP_string_dot___hash_lambda3 x1 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (nd_OP__case_11 x1 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_elem x1 x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\r'#) Curry_Prelude.OP_List)) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

d_C_int :: Curry_Prelude.C_Int -> Cover -> ConstStore -> C_Doc
d_C_int x1 x3250 x3500 = d_C_text (Curry_Prelude.d_C_show x1 x3250 x3500) x3250 x3500

nd_C_int :: Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_int x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_text (Curry_Prelude.d_C_show x1 x3250 x3500) x2000 x3250 x3500))

d_C_float :: Curry_Prelude.C_Float -> Cover -> ConstStore -> C_Doc
d_C_float x1 x3250 x3500 = d_C_text (Curry_Prelude.d_C_show x1 x3250 x3500) x3250 x3500

nd_C_float :: Curry_Prelude.C_Float -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_float x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_text (Curry_Prelude.d_C_show x1 x3250 x3500) x2000 x3250 x3500))

d_C_lparen :: Cover -> ConstStore -> C_Doc
d_C_lparen x3250 x3500 = d_C_char (Curry_Prelude.C_Char '('#) x3250 x3500

nd_C_lparen :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_lparen x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char '('#) x2000 x3250 x3500))

d_C_rparen :: Cover -> ConstStore -> C_Doc
d_C_rparen x3250 x3500 = d_C_char (Curry_Prelude.C_Char ')'#) x3250 x3500

nd_C_rparen :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_rparen x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char ')'#) x2000 x3250 x3500))

d_C_langle :: Cover -> ConstStore -> C_Doc
d_C_langle x3250 x3500 = d_C_char (Curry_Prelude.C_Char '<'#) x3250 x3500

nd_C_langle :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_langle x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char '<'#) x2000 x3250 x3500))

d_C_rangle :: Cover -> ConstStore -> C_Doc
d_C_rangle x3250 x3500 = d_C_char (Curry_Prelude.C_Char '>'#) x3250 x3500

nd_C_rangle :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_rangle x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char '>'#) x2000 x3250 x3500))

d_C_lbrace :: Cover -> ConstStore -> C_Doc
d_C_lbrace x3250 x3500 = d_C_char (Curry_Prelude.C_Char '{'#) x3250 x3500

nd_C_lbrace :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_lbrace x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char '{'#) x2000 x3250 x3500))

d_C_rbrace :: Cover -> ConstStore -> C_Doc
d_C_rbrace x3250 x3500 = d_C_char (Curry_Prelude.C_Char '}'#) x3250 x3500

nd_C_rbrace :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_rbrace x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char '}'#) x2000 x3250 x3500))

d_C_lbracket :: Cover -> ConstStore -> C_Doc
d_C_lbracket x3250 x3500 = d_C_char (Curry_Prelude.C_Char '['#) x3250 x3500

nd_C_lbracket :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_lbracket x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char '['#) x2000 x3250 x3500))

d_C_rbracket :: Cover -> ConstStore -> C_Doc
d_C_rbracket x3250 x3500 = d_C_char (Curry_Prelude.C_Char ']'#) x3250 x3500

nd_C_rbracket :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_rbracket x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char ']'#) x2000 x3250 x3500))

d_C_squote :: Cover -> ConstStore -> C_Doc
d_C_squote x3250 x3500 = d_C_char (Curry_Prelude.C_Char '\''#) x3250 x3500

nd_C_squote :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_squote x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char '\''#) x2000 x3250 x3500))

d_C_dquote :: Cover -> ConstStore -> C_Doc
d_C_dquote x3250 x3500 = d_C_char (Curry_Prelude.C_Char '"'#) x3250 x3500

nd_C_dquote :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_dquote x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char '"'#) x2000 x3250 x3500))

d_C_bquote :: Cover -> ConstStore -> C_Doc
d_C_bquote x3250 x3500 = d_C_char (Curry_Prelude.C_Char '`'#) x3250 x3500

nd_C_bquote :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_bquote x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char '`'#) x2000 x3250 x3500))

d_C_semi :: Cover -> ConstStore -> C_Doc
d_C_semi x3250 x3500 = d_C_char (Curry_Prelude.C_Char ';'#) x3250 x3500

nd_C_semi :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_semi x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char ';'#) x2000 x3250 x3500))

d_C_colon :: Cover -> ConstStore -> C_Doc
d_C_colon x3250 x3500 = d_C_char (Curry_Prelude.C_Char ':'#) x3250 x3500

nd_C_colon :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_colon x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char ':'#) x2000 x3250 x3500))

d_C_comma :: Cover -> ConstStore -> C_Doc
d_C_comma x3250 x3500 = d_C_char (Curry_Prelude.C_Char ','#) x3250 x3500

nd_C_comma :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_comma x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char ','#) x2000 x3250 x3500))

d_C_space :: Cover -> ConstStore -> C_Doc
d_C_space x3250 x3500 = d_C_char (Curry_Prelude.C_Char ' '#) x3250 x3500

nd_C_space :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_space x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char ' '#) x2000 x3250 x3500))

d_C_dot :: Cover -> ConstStore -> C_Doc
d_C_dot x3250 x3500 = d_C_char (Curry_Prelude.C_Char '.'#) x3250 x3500

nd_C_dot :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_dot x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char '.'#) x2000 x3250 x3500))

d_C_backslash :: Cover -> ConstStore -> C_Doc
d_C_backslash x3250 x3500 = d_C_char (Curry_Prelude.C_Char '\\'#) x3250 x3500

nd_C_backslash :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_backslash x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char '\\'#) x2000 x3250 x3500))

d_C_equals :: Cover -> ConstStore -> C_Doc
d_C_equals x3250 x3500 = d_C_char (Curry_Prelude.C_Char '='#) x3250 x3500

nd_C_equals :: IDSupply -> Cover -> ConstStore -> C_Doc
nd_C_equals x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_char (Curry_Prelude.C_Char '='#) x2000 x3250 x3500))

d_C_normalise :: Cover -> ConstStore -> C_Tokens -> Cover -> ConstStore -> C_Tokens
d_C_normalise x3250 x3500 = d_OP_normalise_dot_go_dot_174 Curry_Prelude.d_C_id

nd_C_normalise :: IDSupply -> Cover -> ConstStore -> Func C_Tokens C_Tokens
nd_C_normalise x3000 x3250 x3500 = wrapNX id (nd_OP_normalise_dot_go_dot_174 (wrapDX id Curry_Prelude.d_C_id))

d_OP_normalise_dot_open_dot_174 :: C_Tokens -> Cover -> ConstStore -> C_Tokens
d_OP_normalise_dot_open_dot_174 x1 x3250 x3500 = case x1 of
     (C_Close x2) -> x2
     (C_Text x3 x4) -> C_Open x1
     (C_Line x5 x6) -> C_Open x1
     (C_Open x7) -> C_Open x1
     C_Empty -> C_Open x1
     (C_OpenNest x8 x9) -> C_Open x1
     (C_CloseNest x10) -> C_Open x1
     (Choice_C_Tokens x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_normalise_dot_open_dot_174 x1002 x3250 x3500) (d_OP_normalise_dot_open_dot_174 x1003 x3250 x3500)
     (Choices_C_Tokens x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_normalise_dot_open_dot_174 z x3250 x3500) x1002
     (Guard_C_Tokens x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_normalise_dot_open_dot_174 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tokens x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_normalise_dot_open_dot_174 :: C_Tokens -> IDSupply -> Cover -> ConstStore -> C_Tokens
nd_OP_normalise_dot_open_dot_174 x1 x3000 x3250 x3500 = case x1 of
     (C_Close x2) -> x2
     (C_Text x3 x4) -> C_Open x1
     (C_Line x5 x6) -> C_Open x1
     (C_Open x7) -> C_Open x1
     C_Empty -> C_Open x1
     (HO_C_OpenNest x8 x9) -> C_Open x1
     (C_CloseNest x10) -> C_Open x1
     (Choice_C_Tokens x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_normalise_dot_open_dot_174 x1002 x3000 x3250 x3500) (nd_OP_normalise_dot_open_dot_174 x1003 x3000 x3250 x3500)
     (Choices_C_Tokens x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_normalise_dot_open_dot_174 z x3000 x3250 x3500) x1002
     (Guard_C_Tokens x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_normalise_dot_open_dot_174 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tokens x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_normalise_dot_go_dot_174 :: (C_Tokens -> Cover -> ConstStore -> C_Tokens) -> C_Tokens -> Cover -> ConstStore -> C_Tokens
d_OP_normalise_dot_go_dot_174 x1 x2 x3250 x3500 = case x2 of
     C_Empty -> Curry_Prelude.d_C_apply x1 C_Empty x3250 x3500
     (C_Open x3) -> d_OP_normalise_dot_go_dot_174 (Curry_Prelude.d_OP_dot x1 d_OP_normalise_dot_open_dot_174 x3250 x3500) x3 x3250 x3500
     (C_Close x4) -> d_OP_normalise_dot_go_dot_174 (Curry_Prelude.d_OP_dot x1 (acceptCs id C_Close) x3250 x3500) x4 x3250 x3500
     (C_Line x5 x6) -> Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot x1 (Curry_Prelude.d_OP_dot (acceptCs id (C_Line x5)) (d_OP_normalise_dot_go_dot_174 Curry_Prelude.d_C_id) x3250 x3500) x3250 x3500) x6 x3250 x3500
     (C_Text x7 x8) -> C_Text x7 (d_OP_normalise_dot_go_dot_174 x1 x8 x3250 x3500)
     (C_OpenNest x9 x10) -> C_OpenNest x9 (d_OP_normalise_dot_go_dot_174 x1 x10 x3250 x3500)
     (C_CloseNest x11) -> C_CloseNest (d_OP_normalise_dot_go_dot_174 x1 x11 x3250 x3500)
     (Choice_C_Tokens x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_normalise_dot_go_dot_174 x1 x1002 x3250 x3500) (d_OP_normalise_dot_go_dot_174 x1 x1003 x3250 x3500)
     (Choices_C_Tokens x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_normalise_dot_go_dot_174 x1 z x3250 x3500) x1002
     (Guard_C_Tokens x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_normalise_dot_go_dot_174 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tokens x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_normalise_dot_go_dot_174 :: Func C_Tokens C_Tokens -> C_Tokens -> IDSupply -> Cover -> ConstStore -> C_Tokens
nd_OP_normalise_dot_go_dot_174 x1 x2 x3000 x3250 x3500 = case x2 of
     C_Empty -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_apply x1 C_Empty x2000 x3250 x3500))
     (C_Open x3) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP_normalise_dot_go_dot_174 (Curry_Prelude.nd_OP_dot x1 (wrapNX id nd_OP_normalise_dot_open_dot_174) x2000 x3250 x3500) x3 x2001 x3250 x3500)))))
     (C_Close x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP_normalise_dot_go_dot_174 (Curry_Prelude.nd_OP_dot x1 (wrapDX id (acceptCs id C_Close)) x2000 x3250 x3500) x4 x2001 x3250 x3500)))))
     (C_Line x5 x6) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot x1 (Curry_Prelude.nd_OP_dot (wrapDX id (acceptCs id (C_Line x5))) (wrapNX id (nd_OP_normalise_dot_go_dot_174 (wrapDX id Curry_Prelude.d_C_id))) x2000 x3250 x3500) x2001 x3250 x3500)))) x6 x2003 x3250 x3500)))))
     (C_Text x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (C_Text x7 (nd_OP_normalise_dot_go_dot_174 x1 x8 x2000 x3250 x3500)))
     (HO_C_OpenNest x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (HO_C_OpenNest x9 (nd_OP_normalise_dot_go_dot_174 x1 x10 x2000 x3250 x3500)))
     (C_CloseNest x11) -> let
          x2000 = x3000
           in (seq x2000 (C_CloseNest (nd_OP_normalise_dot_go_dot_174 x1 x11 x2000 x3250 x3500)))
     (Choice_C_Tokens x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_normalise_dot_go_dot_174 x1 x1002 x3000 x3250 x3500) (nd_OP_normalise_dot_go_dot_174 x1 x1003 x3000 x3250 x3500)
     (Choices_C_Tokens x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_normalise_dot_go_dot_174 x1 z x3000 x3250 x3500) x1002
     (Guard_C_Tokens x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_normalise_dot_go_dot_174 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tokens x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_doc2Tokens :: C_Doc -> Cover -> ConstStore -> C_Tokens
d_C_doc2Tokens x1 x3250 x3500 = case x1 of
     (C_Doc x2) -> Curry_Prelude.d_C_apply (d_C_normalise x3250 x3500) (Curry_Prelude.d_C_apply x2 C_Empty x3250 x3500) x3250 x3500
     (Choice_C_Doc x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_doc2Tokens x1002 x3250 x3500) (d_C_doc2Tokens x1003 x3250 x3500)
     (Choices_C_Doc x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_doc2Tokens z x3250 x3500) x1002
     (Guard_C_Doc x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_doc2Tokens x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Doc x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_doc2Tokens :: C_Doc -> IDSupply -> Cover -> ConstStore -> C_Tokens
nd_C_doc2Tokens x1 x3000 x3250 x3500 = case x1 of
     (HO_C_Doc x2) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_normalise x2000 x3250 x3500) (Curry_Prelude.nd_C_apply x2 C_Empty x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Choice_C_Doc x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_doc2Tokens x1002 x3000 x3250 x3500) (nd_C_doc2Tokens x1003 x3000 x3250 x3500)
     (Choices_C_Doc x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_doc2Tokens z x3000 x3250 x3500) x1002
     (Guard_C_Doc x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_doc2Tokens x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Doc x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_pretty :: Curry_Prelude.C_Int -> C_Doc -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_pretty x1 x2 x3250 x3500 = d_C_noGroup (d_C_doc2Tokens x2 x3250 x3500) x1 (Curry_Prelude.C_Int 1#) x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 0#) Curry_Prelude.OP_List) x3250 x3500

nd_C_pretty :: Curry_Prelude.C_Int -> C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_pretty x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_noGroup (nd_C_doc2Tokens x2 x2000 x3250 x3500) x1 (Curry_Prelude.C_Int 1#) x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 0#) Curry_Prelude.OP_List) x2001 x3250 x3500)))))

d_C_length :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_length x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_length (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_not (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 5#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 6#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 7#) Curry_Prelude.OP_List))) (Curry_Prelude.d_C_enumFromTo (Curry_Prelude.C_Int 16#) (Curry_Prelude.C_Int 31#) x3250 x3500) x3250 x3500)) Curry_Prelude.d_C_ord x3250 x3500) x3250 x3500)) x3250 x3500

nd_C_length :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Int
nd_C_length x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_length) (wrapNX id (Curry_Prelude.nd_C_filter (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_not) (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_flip (wrapNX id Curry_Prelude.nd_C_elem) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 5#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 6#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 7#) Curry_Prelude.OP_List))) (Curry_Prelude.d_C_enumFromTo (Curry_Prelude.C_Int 16#) (Curry_Prelude.C_Int 31#) x3250 x3500) x3250 x3500))) (wrapDX id Curry_Prelude.d_C_ord) x2000 x3250 x3500) x2001 x3250 x3500)))))) x2003 x3250 x3500)))))

d_C_noGroup :: C_Tokens -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_noGroup x1 x2 x3 x4 x5 x3250 x3500 = case x1 of
     C_Empty -> Curry_Prelude.OP_List
     (C_Text x6 x7) -> let
          x8 = Curry_Prelude.d_C_apply (d_C_length x3250 x3500) x6 x3250 x3500
           in (Curry_Prelude.d_OP_plus_plus x6 (d_C_noGroup x7 x2 (Curry_Prelude.d_OP_plus x3 x8 x3250 x3500) (Curry_Prelude.d_OP_minus x4 x8 x3250 x3500) x5 x3250 x3500) x3250 x3500)
     (C_Line x9 x10) -> d_OP__case_10 x2 x3 x10 x5 x3250 x3500
     (C_Open x13) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_oneGroup x13 x2 x3 (Curry_Prelude.d_OP_plus x3 x4 x3250 x3500) (acceptCs id d_OP_noGroup_dot___hash_lambda5) x3250 x3500) x4 x3250 x3500) x5 x3250 x3500
     (C_Close x14) -> d_C_noGroup x14 x2 x3 x4 x5 x3250 x3500
     (C_OpenNest x15 x16) -> d_C_noGroup x16 x2 x3 x4 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x15 x5 x3250 x3500) x4 x3250 x3500) x2 x3250 x3500) x3250 x3500
     (C_CloseNest x17) -> d_C_noGroup x17 x2 x3 x4 (Curry_Prelude.d_C_tail x5 x3250 x3500) x3250 x3500
     (Choice_C_Tokens x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_noGroup x1002 x2 x3 x4 x5 x3250 x3500) (d_C_noGroup x1003 x2 x3 x4 x5 x3250 x3500)
     (Choices_C_Tokens x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_noGroup z x2 x3 x4 x5 x3250 x3500) x1002
     (Guard_C_Tokens x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_noGroup x1002 x2 x3 x4 x5 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tokens x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_noGroup :: C_Tokens -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_noGroup x1 x2 x3 x4 x5 x3000 x3250 x3500 = case x1 of
     C_Empty -> Curry_Prelude.OP_List
     (C_Text x6 x7) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x8 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_length x2000 x3250 x3500) x6 x2001 x3250 x3500)))
                     in (Curry_Prelude.d_OP_plus_plus x6 (nd_C_noGroup x7 x2 (Curry_Prelude.d_OP_plus x3 x8 x3250 x3500) (Curry_Prelude.d_OP_minus x4 x8 x3250 x3500) x5 x2003 x3250 x3500) x3250 x3500))))))
     (C_Line x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x2 x3 x10 x5 x2000 x3250 x3500))
     (C_Open x13) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_oneGroup x13 x2 x3 (Curry_Prelude.d_OP_plus x3 x4 x3250 x3500) (wrapDX (wrapNX id) (acceptCs id nd_OP_noGroup_dot___hash_lambda5)) x2000 x3250 x3500) x4 x2001 x3250 x3500)))) x5 x2003 x3250 x3500)))))
     (C_Close x14) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_noGroup x14 x2 x3 x4 x5 x2000 x3250 x3500))
     (HO_C_OpenNest x15 x16) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (nd_C_noGroup x16 x2 x3 x4 (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x15 x5 x2000 x3250 x3500) x4 x2001 x3250 x3500)))) x2 x2003 x3250 x3500)))) x2005 x3250 x3500)))))
     (C_CloseNest x17) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_noGroup x17 x2 x3 x4 (Curry_Prelude.d_C_tail x5 x3250 x3500) x2000 x3250 x3500))
     (Choice_C_Tokens x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_noGroup x1002 x2 x3 x4 x5 x3000 x3250 x3500) (nd_C_noGroup x1003 x2 x3 x4 x5 x3000 x3250 x3500)
     (Choices_C_Tokens x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_noGroup z x2 x3 x4 x5 x3000 x3250 x3500) x1002
     (Guard_C_Tokens x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_noGroup x1002 x2 x3 x4 x5 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tokens x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_noGroup_dot___hash_lambda5 :: Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_noGroup_dot___hash_lambda5 x1 x2 x3250 x3500 = x2

nd_OP_noGroup_dot___hash_lambda5 :: Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_noGroup_dot___hash_lambda5 x1 x2 x3000 x3250 x3500 = x2

d_C_oneGroup :: C_Tokens -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_oneGroup x1 x2 x3 x4 x5 x3250 x3500 = case x1 of
     (C_Text x6 x7) -> let
          x8 = Curry_Prelude.d_C_apply (d_C_length x3250 x3500) x6 x3250 x3500
           in (d_C_pruneOne x7 x2 (Curry_Prelude.d_OP_plus x3 x8 x3250 x3500) x4 (acceptCs id (d_OP_oneGroup_dot___hash_lambda6 x8 x5 x6)) x3250 x3500)
     (C_Line x9 x10) -> let
          x11 = Curry_Prelude.d_C_apply (d_C_length x3250 x3500) x9 x3250 x3500
           in (d_C_pruneOne x10 x2 (Curry_Prelude.d_OP_plus x3 x11 x3250 x3500) x4 (acceptCs id (d_OP_oneGroup_dot___hash_lambda7 x11 x5 x9 x2)) x3250 x3500)
     (C_Open x12) -> d_C_multiGroup x12 x2 x3 x4 x5 (Curry_Dequeue.d_C_empty x3250 x3500) x3 (acceptCs id d_OP_oneGroup_dot___hash_lambda8) x3250 x3500
     (C_Close x13) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x5 (Curry_Prelude.d_OP_lt_eq x3 x4 x3250 x3500) x3250 x3500) (acceptCs id (d_C_noGroup x13 x2 x3)) x3250 x3500
     (C_OpenNest x14 x15) -> d_C_oneGroup x15 x2 x3 x4 (acceptCs id (d_OP_oneGroup_dot___hash_lambda9 x14 x5 x2)) x3250 x3500
     (C_CloseNest x16) -> d_C_oneGroup x16 x2 x3 x4 (acceptCs id (d_OP_oneGroup_dot___hash_lambda11 x5)) x3250 x3500
     (Choice_C_Tokens x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_oneGroup x1002 x2 x3 x4 x5 x3250 x3500) (d_C_oneGroup x1003 x2 x3 x4 x5 x3250 x3500)
     (Choices_C_Tokens x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_oneGroup z x2 x3 x4 x5 x3250 x3500) x1002
     (Guard_C_Tokens x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_oneGroup x1002 x2 x3 x4 x5 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tokens x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_oneGroup :: C_Tokens -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_oneGroup x1 x2 x3 x4 x5 x3000 x3250 x3500 = case x1 of
     (C_Text x6 x7) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x8 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_length x2000 x3250 x3500) x6 x2001 x3250 x3500)))
                     in (nd_C_pruneOne x7 x2 (Curry_Prelude.d_OP_plus x3 x8 x3250 x3500) x4 (wrapDX (wrapNX id) (acceptCs id (nd_OP_oneGroup_dot___hash_lambda6 x8 x5 x6))) x2003 x3250 x3500))))))
     (C_Line x9 x10) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x11 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_length x2000 x3250 x3500) x9 x2001 x3250 x3500)))
                     in (nd_C_pruneOne x10 x2 (Curry_Prelude.d_OP_plus x3 x11 x3250 x3500) x4 (wrapDX (wrapNX id) (acceptCs id (nd_OP_oneGroup_dot___hash_lambda7 x11 x5 x9 x2))) x2003 x3250 x3500))))))
     (C_Open x12) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_multiGroup x12 x2 x3 x4 x5 (Curry_Dequeue.d_C_empty x3250 x3500) x3 (wrapDX (wrapNX id) (acceptCs id nd_OP_oneGroup_dot___hash_lambda8)) x2000 x3250 x3500))
     (C_Close x13) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x5 (Curry_Prelude.d_OP_lt_eq x3 x4 x3250 x3500) x2000 x3250 x3500) (wrapDX (wrapNX id) (acceptCs id (nd_C_noGroup x13 x2 x3))) x2001 x3250 x3500)))))
     (HO_C_OpenNest x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_oneGroup x15 x2 x3 x4 (wrapDX (wrapNX id) (acceptCs id (nd_OP_oneGroup_dot___hash_lambda9 x14 x5 x2))) x2000 x3250 x3500))
     (C_CloseNest x16) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_oneGroup x16 x2 x3 x4 (wrapDX (wrapNX id) (acceptCs id (nd_OP_oneGroup_dot___hash_lambda11 x5))) x2000 x3250 x3500))
     (Choice_C_Tokens x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_oneGroup x1002 x2 x3 x4 x5 x3000 x3250 x3500) (nd_C_oneGroup x1003 x2 x3 x4 x5 x3000 x3250 x3500)
     (Choices_C_Tokens x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_oneGroup z x2 x3 x4 x5 x3000 x3250 x3500) x1002
     (Guard_C_Tokens x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_oneGroup x1002 x2 x3 x4 x5 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tokens x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_oneGroup_dot_outText_dot_235 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_oneGroup_dot_outText_dot_235 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x3 (Curry_Prelude.d_OP_minus x4 x1 x3250 x3500) x3250 x3500) x5 x3250 x3500) x3250 x3500

nd_OP_oneGroup_dot_outText_dot_235 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func Curry_Prelude.C_Int (Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_oneGroup_dot_outText_dot_235 x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_OP_plus_plus x2 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x3 (Curry_Prelude.d_OP_minus x4 x1 x3250 x3500) x2000 x3250 x3500) x5 x2001 x3250 x3500)))) x3250 x3500))

d_OP_oneGroup_dot___hash_lambda6 :: Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_oneGroup_dot___hash_lambda6 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x4 x3250 x3500) (acceptCs id (d_OP_oneGroup_dot_outText_dot_235 x1 x3 x5)) x3250 x3500

nd_OP_oneGroup_dot___hash_lambda6 :: Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_oneGroup_dot___hash_lambda6 x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x4 x2000 x3250 x3500) (wrapDX (wrapNX id) (acceptCs id (nd_OP_oneGroup_dot_outText_dot_235 x1 x3 x5))) x2001 x3250 x3500)))))

d_OP_oneGroup_dot_outLine_dot_241 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_oneGroup_dot_outLine_dot_241 x1 x2 x3 x4 x5 x6 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_9 x7 x8 x3 x5 x1 x6 x2 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_oneGroup_dot_outLine_dot_241 x1 x2 x3 x4 x5 x6 x1002 x3250 x3500) (d_OP_oneGroup_dot_outLine_dot_241 x1 x2 x3 x4 x5 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_oneGroup_dot_outLine_dot_241 x1 x2 x3 x4 x5 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_oneGroup_dot_outLine_dot_241 x1 x2 x3 x4 x5 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_oneGroup_dot_outLine_dot_241 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_oneGroup_dot_outLine_dot_241 x1 x2 x3 x4 x5 x6 x7 x3000 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x7 x8 x3 x5 x1 x6 x2 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_oneGroup_dot_outLine_dot_241 x1 x2 x3 x4 x5 x6 x1002 x3000 x3250 x3500) (nd_OP_oneGroup_dot_outLine_dot_241 x1 x2 x3 x4 x5 x6 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_oneGroup_dot_outLine_dot_241 x1 x2 x3 x4 x5 x6 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_oneGroup_dot_outLine_dot_241 x1 x2 x3 x4 x5 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_oneGroup_dot___hash_lambda7 :: Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_oneGroup_dot___hash_lambda7 x1 x2 x3 x4 x5 x6 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x5 x3250 x3500) (acceptCs id (d_OP_oneGroup_dot_outLine_dot_241 x1 x3 x4 x5 x6)) x3250 x3500

nd_OP_oneGroup_dot___hash_lambda7 :: Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_oneGroup_dot___hash_lambda7 x1 x2 x3 x4 x5 x6 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x5 x2000 x3250 x3500) (wrapDX (wrapNX id) (acceptCs id (nd_OP_oneGroup_dot_outLine_dot_241 x1 x3 x4 x5 x6))) x2001 x3250 x3500)))))

d_OP_oneGroup_dot___hash_lambda8 :: Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_oneGroup_dot___hash_lambda8 x1 x2 x3250 x3500 = x2

nd_OP_oneGroup_dot___hash_lambda8 :: Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_oneGroup_dot___hash_lambda8 x1 x2 x3000 x3250 x3500 = x2

d_OP_oneGroup_dot___hash_lambda9 :: (Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int) -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_oneGroup_dot___hash_lambda9 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x4 x3250 x3500) (acceptCs id (d_OP_oneGroup_dot___hash_lambda9_dot___hash_lambda10 x5 x1 x3)) x3250 x3500

nd_OP_oneGroup_dot___hash_lambda9 :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Func Curry_Prelude.C_Int (Func Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Int))) -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_oneGroup_dot___hash_lambda9 x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x4 x2000 x3250 x3500) (wrapDX (wrapNX id) (acceptCs id (nd_OP_oneGroup_dot___hash_lambda9_dot___hash_lambda10 x5 x1 x3))) x2001 x3250 x3500)))))

d_OP_oneGroup_dot___hash_lambda9_dot___hash_lambda10 :: (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_oneGroup_dot___hash_lambda9_dot___hash_lambda10 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x4 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x5 x3250 x3500) x4 x3250 x3500) x3 x3250 x3500) x3250 x3500

nd_OP_oneGroup_dot___hash_lambda9_dot___hash_lambda10 :: Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Func Curry_Prelude.C_Int (Func Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Int))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_oneGroup_dot___hash_lambda9_dot___hash_lambda10 x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2007 = x3000
      in (seq x2007 (let
          x2006 = leftSupply x2007
          x2008 = rightSupply x2007
           in (seq x2006 (seq x2008 (let
               x2000 = leftSupply x2008
               x2005 = rightSupply x2008
                in (seq x2000 (seq x2005 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x4 x2000 x3250 x3500) (let
                    x2004 = leftSupply x2005
                    x2003 = rightSupply x2005
                     in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x5 x2001 x3250 x3500) x4 x2002 x3250 x3500)))) x3 x2004 x3250 x3500)))) x2006 x3250 x3500))))))))

d_OP_oneGroup_dot___hash_lambda11 :: (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_oneGroup_dot___hash_lambda11 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3250 x3500) (acceptCs id (d_OP_oneGroup_dot___hash_lambda11_dot___hash_lambda12 x3)) x3250 x3500

nd_OP_oneGroup_dot___hash_lambda11 :: Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_oneGroup_dot___hash_lambda11 x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3250 x3500) (wrapDX (wrapNX id) (acceptCs id (nd_OP_oneGroup_dot___hash_lambda11_dot___hash_lambda12 x3))) x2001 x3250 x3500)))))

d_OP_oneGroup_dot___hash_lambda11_dot___hash_lambda12 :: (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_oneGroup_dot___hash_lambda11_dot___hash_lambda12 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3250 x3500) (Curry_Prelude.d_C_tail x3 x3250 x3500) x3250 x3500

nd_OP_oneGroup_dot___hash_lambda11_dot___hash_lambda12 :: Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_oneGroup_dot___hash_lambda11_dot___hash_lambda12 x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3250 x3500) (Curry_Prelude.d_C_tail x3 x3250 x3500) x2001 x3250 x3500)))))

d_C_multiGroup :: C_Tokens -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_multiGroup x1 x2 x3 x4 x5 x6 x7 x8 x3250 x3500 = case x1 of
     (C_Text x9 x10) -> let
          x11 = Curry_Prelude.d_C_apply (d_C_length x3250 x3500) x9 x3250 x3500
           in (d_C_pruneMulti x10 x2 (Curry_Prelude.d_OP_plus x3 x11 x3250 x3500) x4 x5 x6 x7 (acceptCs id (d_OP_multiGroup_dot___hash_lambda13 x11 x8 x9)) x3250 x3500)
     (C_Line x12 x13) -> let
          x14 = Curry_Prelude.d_C_apply (d_C_length x3250 x3500) x12 x3250 x3500
           in (d_C_pruneMulti x13 x2 (Curry_Prelude.d_OP_plus x3 x14 x3250 x3500) x4 x5 x6 x7 (acceptCs id (d_OP_multiGroup_dot___hash_lambda14 x14 x8 x12 x2)) x3250 x3500)
     (C_Open x15) -> d_C_multiGroup x15 x2 x3 x4 x5 (Curry_Dequeue.d_C_cons (Curry_Prelude.OP_Tuple2 x7 x8) x6 x3250 x3500) x3 (acceptCs id d_OP_multiGroup_dot___hash_lambda15) x3250 x3500
     (C_Close x16) -> d_OP__case_8 x6 x7 x3 x8 x5 x4 x2 x16 (Curry_Dequeue.d_C_matchHead x6 x3250 x3500) x3250 x3500
     (C_OpenNest x22 x23) -> d_C_multiGroup x23 x2 x3 x4 x5 x6 x7 (acceptCs id (d_OP_multiGroup_dot___hash_lambda21 x22 x8 x2)) x3250 x3500
     (C_CloseNest x24) -> d_C_multiGroup x24 x2 x3 x4 x5 x6 x7 (acceptCs id (d_OP_multiGroup_dot___hash_lambda23 x8)) x3250 x3500
     (Choice_C_Tokens x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_multiGroup x1002 x2 x3 x4 x5 x6 x7 x8 x3250 x3500) (d_C_multiGroup x1003 x2 x3 x4 x5 x6 x7 x8 x3250 x3500)
     (Choices_C_Tokens x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_multiGroup z x2 x3 x4 x5 x6 x7 x8 x3250 x3500) x1002
     (Guard_C_Tokens x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_multiGroup x1002 x2 x3 x4 x5 x6 x7 x8 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tokens x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_multiGroup :: C_Tokens -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_multiGroup x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3250 x3500 = case x1 of
     (C_Text x9 x10) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x11 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_length x2000 x3250 x3500) x9 x2001 x3250 x3500)))
                     in (nd_C_pruneMulti x10 x2 (Curry_Prelude.d_OP_plus x3 x11 x3250 x3500) x4 x5 x6 x7 (wrapDX (wrapNX id) (acceptCs id (nd_OP_multiGroup_dot___hash_lambda13 x11 x8 x9))) x2003 x3250 x3500))))))
     (C_Line x12 x13) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x14 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_length x2000 x3250 x3500) x12 x2001 x3250 x3500)))
                     in (nd_C_pruneMulti x13 x2 (Curry_Prelude.d_OP_plus x3 x14 x3250 x3500) x4 x5 x6 x7 (wrapDX (wrapNX id) (acceptCs id (nd_OP_multiGroup_dot___hash_lambda14 x14 x8 x12 x2))) x2003 x3250 x3500))))))
     (C_Open x15) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_multiGroup x15 x2 x3 x4 x5 (Curry_Dequeue.d_C_cons (Curry_Prelude.OP_Tuple2 x7 x8) x6 x3250 x3500) x3 (wrapDX (wrapNX id) (acceptCs id nd_OP_multiGroup_dot___hash_lambda15)) x2000 x3250 x3500))
     (C_Close x16) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x6 x7 x3 x8 x5 x4 x2 x16 (Curry_Dequeue.d_C_matchHead x6 x3250 x3500) x2000 x3250 x3500))
     (HO_C_OpenNest x22 x23) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_multiGroup x23 x2 x3 x4 x5 x6 x7 (wrapDX (wrapNX id) (acceptCs id (nd_OP_multiGroup_dot___hash_lambda21 x22 x8 x2))) x2000 x3250 x3500))
     (C_CloseNest x24) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_multiGroup x24 x2 x3 x4 x5 x6 x7 (wrapDX (wrapNX id) (acceptCs id (nd_OP_multiGroup_dot___hash_lambda23 x8))) x2000 x3250 x3500))
     (Choice_C_Tokens x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_multiGroup x1002 x2 x3 x4 x5 x6 x7 x8 x3000 x3250 x3500) (nd_C_multiGroup x1003 x2 x3 x4 x5 x6 x7 x8 x3000 x3250 x3500)
     (Choices_C_Tokens x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_multiGroup z x2 x3 x4 x5 x6 x7 x8 x3000 x3250 x3500) x1002
     (Guard_C_Tokens x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_multiGroup x1002 x2 x3 x4 x5 x6 x7 x8 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tokens x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_multiGroup_dot_outText_dot_262 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_multiGroup_dot_outText_dot_262 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x3 (Curry_Prelude.d_OP_minus x4 x1 x3250 x3500) x3250 x3500) x5 x3250 x3500) x3250 x3500

nd_OP_multiGroup_dot_outText_dot_262 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func Curry_Prelude.C_Int (Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_multiGroup_dot_outText_dot_262 x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_OP_plus_plus x2 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x3 (Curry_Prelude.d_OP_minus x4 x1 x3250 x3500) x2000 x3250 x3500) x5 x2001 x3250 x3500)))) x3250 x3500))

d_OP_multiGroup_dot___hash_lambda13 :: Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_multiGroup_dot___hash_lambda13 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x4 x3250 x3500) (acceptCs id (d_OP_multiGroup_dot_outText_dot_262 x1 x3 x5)) x3250 x3500

nd_OP_multiGroup_dot___hash_lambda13 :: Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_multiGroup_dot___hash_lambda13 x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x4 x2000 x3250 x3500) (wrapDX (wrapNX id) (acceptCs id (nd_OP_multiGroup_dot_outText_dot_262 x1 x3 x5))) x2001 x3250 x3500)))))

d_OP_multiGroup_dot_outLine_dot_268 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_multiGroup_dot_outLine_dot_268 x1 x2 x3 x4 x5 x6 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_5 x7 x8 x3 x5 x1 x6 x2 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_multiGroup_dot_outLine_dot_268 x1 x2 x3 x4 x5 x6 x1002 x3250 x3500) (d_OP_multiGroup_dot_outLine_dot_268 x1 x2 x3 x4 x5 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_multiGroup_dot_outLine_dot_268 x1 x2 x3 x4 x5 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_multiGroup_dot_outLine_dot_268 x1 x2 x3 x4 x5 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_multiGroup_dot_outLine_dot_268 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_multiGroup_dot_outLine_dot_268 x1 x2 x3 x4 x5 x6 x7 x3000 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x7 x8 x3 x5 x1 x6 x2 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_multiGroup_dot_outLine_dot_268 x1 x2 x3 x4 x5 x6 x1002 x3000 x3250 x3500) (nd_OP_multiGroup_dot_outLine_dot_268 x1 x2 x3 x4 x5 x6 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_multiGroup_dot_outLine_dot_268 x1 x2 x3 x4 x5 x6 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_multiGroup_dot_outLine_dot_268 x1 x2 x3 x4 x5 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_multiGroup_dot___hash_lambda14 :: Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_multiGroup_dot___hash_lambda14 x1 x2 x3 x4 x5 x6 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x5 x3250 x3500) (acceptCs id (d_OP_multiGroup_dot_outLine_dot_268 x1 x3 x4 x5 x6)) x3250 x3500

nd_OP_multiGroup_dot___hash_lambda14 :: Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_multiGroup_dot___hash_lambda14 x1 x2 x3 x4 x5 x6 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x5 x2000 x3250 x3500) (wrapDX (wrapNX id) (acceptCs id (nd_OP_multiGroup_dot_outLine_dot_268 x1 x3 x4 x5 x6))) x2001 x3250 x3500)))))

d_OP_multiGroup_dot___hash_lambda15 :: Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_multiGroup_dot___hash_lambda15 x1 x2 x3250 x3500 = x2

nd_OP_multiGroup_dot___hash_lambda15 :: Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_multiGroup_dot___hash_lambda15 x1 x2 x3000 x3250 x3500 = x2

d_OP_multiGroup_dot___hash_lambda17 :: (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_multiGroup_dot___hash_lambda17 x1 x2 x3 x4 x5 x6 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x5 x3250 x3500) (d_OP_multiGroup_dot___hash_lambda17_dot___hash_lambda18 x6 x1 x3 x4) x3250 x3500

nd_OP_multiGroup_dot___hash_lambda17 :: Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_multiGroup_dot___hash_lambda17 x1 x2 x3 x4 x5 x6 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x5 x2000 x3250 x3500) (wrapNX id (nd_OP_multiGroup_dot___hash_lambda17_dot___hash_lambda18 x6 x1 x3 x4)) x2001 x3250 x3500)))))

d_OP_multiGroup_dot___hash_lambda17_dot___hash_lambda18 :: (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_multiGroup_dot___hash_lambda17_dot___hash_lambda18 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 (Curry_Prelude.d_OP_lt_eq x3 (Curry_Prelude.d_OP_plus x4 x5 x3250 x3500) x3250 x3500) x3250 x3500) x1 x3250 x3500) x5 x3250 x3500

nd_OP_multiGroup_dot___hash_lambda17_dot___hash_lambda18 :: Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_multiGroup_dot___hash_lambda17_dot___hash_lambda18 x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 (Curry_Prelude.d_OP_lt_eq x3 (Curry_Prelude.d_OP_plus x4 x5 x3250 x3500) x3250 x3500) x2000 x3250 x3500) x1 x2001 x3250 x3500)))) x5 x2003 x3250 x3500)))))

d_OP_multiGroup_dot___hash_lambda19 :: (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_multiGroup_dot___hash_lambda19 x1 x2 x3 x4 x5 x6 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x5 x3250 x3500) (d_OP_multiGroup_dot___hash_lambda19_dot___hash_lambda20 x6 x2 x3 x4) x3250 x3500

nd_OP_multiGroup_dot___hash_lambda19 :: Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_multiGroup_dot___hash_lambda19 x1 x2 x3 x4 x5 x6 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x5 x2000 x3250 x3500) (wrapNX id (nd_OP_multiGroup_dot___hash_lambda19_dot___hash_lambda20 x6 x2 x3 x4)) x2001 x3250 x3500)))))

d_OP_multiGroup_dot___hash_lambda19_dot___hash_lambda20 :: (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_multiGroup_dot___hash_lambda19_dot___hash_lambda20 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 (Curry_Prelude.d_OP_lt_eq x3 (Curry_Prelude.d_OP_plus x4 x5 x3250 x3500) x3250 x3500) x3250 x3500) x1 x3250 x3500) x5 x3250 x3500

nd_OP_multiGroup_dot___hash_lambda19_dot___hash_lambda20 :: Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_multiGroup_dot___hash_lambda19_dot___hash_lambda20 x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 (Curry_Prelude.d_OP_lt_eq x3 (Curry_Prelude.d_OP_plus x4 x5 x3250 x3500) x3250 x3500) x2000 x3250 x3500) x1 x2001 x3250 x3500)))) x5 x2003 x3250 x3500)))))

d_OP_multiGroup_dot___hash_lambda21 :: (Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int) -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_multiGroup_dot___hash_lambda21 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x4 x3250 x3500) (acceptCs id (d_OP_multiGroup_dot___hash_lambda21_dot___hash_lambda22 x5 x1 x3)) x3250 x3500

nd_OP_multiGroup_dot___hash_lambda21 :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Func Curry_Prelude.C_Int (Func Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Int))) -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_multiGroup_dot___hash_lambda21 x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x4 x2000 x3250 x3500) (wrapDX (wrapNX id) (acceptCs id (nd_OP_multiGroup_dot___hash_lambda21_dot___hash_lambda22 x5 x1 x3))) x2001 x3250 x3500)))))

d_OP_multiGroup_dot___hash_lambda21_dot___hash_lambda22 :: (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_multiGroup_dot___hash_lambda21_dot___hash_lambda22 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x4 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x5 x3250 x3500) x4 x3250 x3500) x3 x3250 x3500) x3250 x3500

nd_OP_multiGroup_dot___hash_lambda21_dot___hash_lambda22 :: Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Func Curry_Prelude.C_Int (Func Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Int))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_multiGroup_dot___hash_lambda21_dot___hash_lambda22 x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2007 = x3000
      in (seq x2007 (let
          x2006 = leftSupply x2007
          x2008 = rightSupply x2007
           in (seq x2006 (seq x2008 (let
               x2000 = leftSupply x2008
               x2005 = rightSupply x2008
                in (seq x2000 (seq x2005 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x4 x2000 x3250 x3500) (let
                    x2004 = leftSupply x2005
                    x2003 = rightSupply x2005
                     in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x5 x2001 x3250 x3500) x4 x2002 x3250 x3500)))) x3 x2004 x3250 x3500)))) x2006 x3250 x3500))))))))

d_OP_multiGroup_dot___hash_lambda23 :: (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_multiGroup_dot___hash_lambda23 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3250 x3500) (acceptCs id (d_OP_multiGroup_dot___hash_lambda23_dot___hash_lambda24 x3)) x3250 x3500

nd_OP_multiGroup_dot___hash_lambda23 :: Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_multiGroup_dot___hash_lambda23 x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3250 x3500) (wrapDX (wrapNX id) (acceptCs id (nd_OP_multiGroup_dot___hash_lambda23_dot___hash_lambda24 x3))) x2001 x3250 x3500)))))

d_OP_multiGroup_dot___hash_lambda23_dot___hash_lambda24 :: (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_multiGroup_dot___hash_lambda23_dot___hash_lambda24 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3250 x3500) (Curry_Prelude.d_C_tail x3 x3250 x3500) x3250 x3500

nd_OP_multiGroup_dot___hash_lambda23_dot___hash_lambda24 :: Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_multiGroup_dot___hash_lambda23_dot___hash_lambda24 x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3250 x3500) (Curry_Prelude.d_C_tail x3 x3250 x3500) x2001 x3250 x3500)))))

d_C_pruneOne :: C_Tokens -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_pruneOne x1 x2 x3 x4 x5 x3250 x3500 = d_OP__case_4 x4 x3 x2 x1 x5 (Curry_Prelude.d_OP_lt_eq x3 x4 x3250 x3500) x3250 x3500

nd_C_pruneOne :: C_Tokens -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_pruneOne x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_4 x4 x3 x2 x1 x5 (Curry_Prelude.d_OP_lt_eq x3 x4 x3250 x3500) x2000 x3250 x3500))

d_C_pruneMulti :: C_Tokens -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_pruneMulti x1 x2 x3 x4 x5 x6 x7 x8 x3250 x3500 = d_OP__case_3 x4 x3 x2 x1 x7 x6 x8 x5 (Curry_Prelude.d_OP_lt_eq x3 x4 x3250 x3500) x3250 x3500

nd_C_pruneMulti :: C_Tokens -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_pruneMulti x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_3 x4 x3 x2 x1 x7 x6 x8 x5 (Curry_Prelude.d_OP_lt_eq x3 x4 x3250 x3500) x2000 x3250 x3500))

d_OP_pruneMulti_dot___hash_lambda25 :: (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_pruneMulti_dot___hash_lambda25 x1 x2 x3 x4 x5 x6 x7 x3250 x3500 = d_OP__case_2 x3 x7 x1 x4 x2 x6 x5 (Curry_Dequeue.d_C_matchLast x3 x3250 x3500) x3250 x3500

nd_OP_pruneMulti_dot___hash_lambda25 :: Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Int -> Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_pruneMulti_dot___hash_lambda25 x1 x2 x3 x4 x5 x6 x7 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_2 x3 x7 x1 x4 x2 x6 x5 (Curry_Dequeue.d_C_matchLast x3 x3250 x3500) x2000 x3250 x3500))

d_OP__case_2 :: Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_2 x3 x7 x1 x4 x2 x6 x5 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.d_C_apply (d_C_pruneOne x5 x6 x2 (Curry_Prelude.d_OP_plus x4 x7 x3250 x3500) x1 x3250 x3500) x7 x3250 x3500
     (Curry_Prelude.C_Just x8) -> d_OP__case_1 x7 x1 x4 x2 x6 x5 x8 x3250 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x3 x7 x1 x4 x2 x6 x5 x1002 x3250 x3500) (d_OP__case_2 x3 x7 x1 x4 x2 x6 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x3 x7 x1 x4 x2 x6 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x3 x7 x1 x4 x2 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_2 :: Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) (Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))))) -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP__case_2 x3 x7 x1 x4 x2 x6 x5 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_Nothing -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_pruneOne x5 x6 x2 (Curry_Prelude.d_OP_plus x4 x7 x3250 x3500) x1 x2000 x3250 x3500) x7 x2001 x3250 x3500)))))
     (Curry_Prelude.C_Just x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x7 x1 x4 x2 x6 x5 x8 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x3 x7 x1 x4 x2 x6 x5 x1002 x3000 x3250 x3500) (nd_OP__case_2 x3 x7 x1 x4 x2 x6 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x3 x7 x1 x4 x2 x6 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x3 x7 x1 x4 x2 x6 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_1 x7 x1 x4 x2 x6 x5 x8 x3250 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x9 x10) -> d_OP__case_0 x7 x1 x4 x10 x2 x6 x5 x9 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x7 x1 x4 x2 x6 x5 x1002 x3250 x3500) (d_OP__case_1 x7 x1 x4 x2 x6 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x7 x1 x4 x2 x6 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x7 x1 x4 x2 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_1 :: Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) (Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP__case_1 x7 x1 x4 x2 x6 x5 x8 x3000 x3250 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x7 x1 x4 x10 x2 x6 x5 x9 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x7 x1 x4 x2 x6 x5 x1002 x3000 x3250 x3500) (nd_OP__case_1 x7 x1 x4 x2 x6 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x7 x1 x4 x2 x6 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x7 x1 x4 x2 x6 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x7 x1 x4 x10 x2 x6 x5 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> Curry_Prelude.d_C_apply (d_C_pruneMulti x5 x6 x2 (Curry_Prelude.d_OP_plus x11 x7 x3250 x3500) x12 x10 x4 x1 x3250 x3500) x7 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x7 x1 x4 x10 x2 x6 x5 x1002 x3250 x3500) (d_OP__case_0 x7 x1 x4 x10 x2 x6 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x7 x1 x4 x10 x2 x6 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x7 x1 x4 x10 x2 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_0 :: Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Int -> Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))) -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP__case_0 x7 x1 x4 x10 x2 x6 x5 x9 x3000 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_pruneMulti x5 x6 x2 (Curry_Prelude.d_OP_plus x11 x7 x3250 x3500) x12 x10 x4 x1 x2000 x3250 x3500) x7 x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x7 x1 x4 x10 x2 x6 x5 x1002 x3000 x3250 x3500) (nd_OP__case_0 x7 x1 x4 x10 x2 x6 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x7 x1 x4 x10 x2 x6 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x7 x1 x4 x10 x2 x6 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.C_Int -> Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_3 x4 x3 x2 x1 x7 x6 x8 x5 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_multiGroup x1 x2 x3 x4 x5 x6 x7 x8 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x5 Curry_Prelude.C_False x3250 x3500) (d_OP_pruneMulti_dot___hash_lambda25 x8 x3 x6 x7 x1 x2) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x4 x3 x2 x1 x7 x6 x8 x5 x1002 x3250 x3500) (d_OP__case_3 x4 x3 x2 x1 x7 x6 x8 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x4 x3 x2 x1 x7 x6 x8 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x4 x3 x2 x1 x7 x6 x8 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_3 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.C_Int -> Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_3 x4 x3 x2 x1 x7 x6 x8 x5 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_multiGroup x1 x2 x3 x4 x5 x6 x7 x8 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x5 Curry_Prelude.C_False x2000 x3250 x3500) (wrapNX id (nd_OP_pruneMulti_dot___hash_lambda25 x8 x3 x6 x7 x1 x2)) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x4 x3 x2 x1 x7 x6 x8 x5 x1002 x3000 x3250 x3500) (nd_OP__case_3 x4 x3 x2 x1 x7 x6 x8 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x4 x3 x2 x1 x7 x6 x8 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x4 x3 x2 x1 x7 x6 x8 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_4 x4 x3 x2 x1 x5 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_oneGroup x1 x2 x3 x4 x5 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x5 Curry_Prelude.C_False x3250 x3500) (acceptCs id (d_C_noGroup x1 x2 x3)) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x4 x3 x2 x1 x5 x1002 x3250 x3500) (d_OP__case_4 x4 x3 x2 x1 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x4 x3 x2 x1 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x4 x3 x2 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_4 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_4 x4 x3 x2 x1 x5 x6 x3000 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_oneGroup x1 x2 x3 x4 x5 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x5 Curry_Prelude.C_False x2000 x3250 x3500) (wrapDX (wrapNX id) (acceptCs id (nd_C_noGroup x1 x2 x3))) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x4 x3 x2 x1 x5 x1002 x3000 x3250 x3500) (nd_OP__case_4 x4 x3 x2 x1 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x4 x3 x2 x1 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x4 x3 x2 x1 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_5 x7 x8 x3 x5 x1 x6 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x5 (Curry_Prelude.d_OP_minus x6 x1 x3250 x3500) x3250 x3500) x7 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_replicate x8 (Curry_Prelude.C_Char ' '#) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x5 (Curry_Prelude.d_OP_minus x3 x8 x3250 x3500) x3250 x3500) x7 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x7 x8 x3 x5 x1 x6 x2 x1002 x3250 x3500) (d_OP__case_5 x7 x8 x3 x5 x1 x6 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x7 x8 x3 x5 x1 x6 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x7 x8 x3 x5 x1 x6 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_5 x7 x8 x3 x5 x1 x6 x2 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus x2 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x5 (Curry_Prelude.d_OP_minus x6 x1 x3250 x3500) x2000 x3250 x3500) x7 x2001 x3250 x3500)))) x3250 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_replicate x8 (Curry_Prelude.C_Char ' '#) x3250 x3500) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x5 (Curry_Prelude.d_OP_minus x3 x8 x3250 x3500) x2000 x3250 x3500) x7 x2001 x3250 x3500)))) x3250 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x7 x8 x3 x5 x1 x6 x2 x1002 x3000 x3250 x3500) (nd_OP__case_5 x7 x8 x3 x5 x1 x6 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x7 x8 x3 x5 x1 x6 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x7 x8 x3 x5 x1 x6 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_8 x6 x7 x3 x8 x5 x4 x2 x16 x18 x3250 x3500 = case x18 of
     Curry_Prelude.C_Nothing -> d_C_oneGroup x16 x2 x3 x4 (acceptCs id (d_OP_multiGroup_dot___hash_lambda17 x8 x5 x3 x7)) x3250 x3500
     (Curry_Prelude.C_Just x17) -> d_OP__case_7 x7 x3 x8 x5 x4 x2 x16 x17 x3250 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x6 x7 x3 x8 x5 x4 x2 x16 x1002 x3250 x3500) (d_OP__case_8 x6 x7 x3 x8 x5 x4 x2 x16 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x6 x7 x3 x8 x5 x4 x2 x16 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x6 x7 x3 x8 x5 x4 x2 x16 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_8 :: Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) (Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))))) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_8 x6 x7 x3 x8 x5 x4 x2 x16 x18 x3000 x3250 x3500 = case x18 of
     Curry_Prelude.C_Nothing -> let
          x2000 = x3000
           in (seq x2000 (nd_C_oneGroup x16 x2 x3 x4 (wrapDX (wrapNX id) (acceptCs id (nd_OP_multiGroup_dot___hash_lambda17 x8 x5 x3 x7))) x2000 x3250 x3500))
     (Curry_Prelude.C_Just x17) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x7 x3 x8 x5 x4 x2 x16 x17 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x6 x7 x3 x8 x5 x4 x2 x16 x1002 x3000 x3250 x3500) (nd_OP__case_8 x6 x7 x3 x8 x5 x4 x2 x16 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x6 x7 x3 x8 x5 x4 x2 x16 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x6 x7 x3 x8 x5 x4 x2 x16 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_7 x7 x3 x8 x5 x4 x2 x16 x17 x3250 x3500 = case x17 of
     (Curry_Prelude.OP_Tuple2 x18 x19) -> d_OP__case_6 x7 x3 x8 x19 x5 x4 x2 x16 x18 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x7 x3 x8 x5 x4 x2 x16 x1002 x3250 x3500) (d_OP__case_7 x7 x3 x8 x5 x4 x2 x16 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x7 x3 x8 x5 x4 x2 x16 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x7 x3 x8 x5 x4 x2 x16 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_7 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) (Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_7 x7 x3 x8 x5 x4 x2 x16 x17 x3000 x3250 x3500 = case x17 of
     (Curry_Prelude.OP_Tuple2 x18 x19) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x7 x3 x8 x19 x5 x4 x2 x16 x18 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x7 x3 x8 x5 x4 x2 x16 x1002 x3000 x3250 x3500) (nd_OP__case_7 x7 x3 x8 x5 x4 x2 x16 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x7 x3 x8 x5 x4 x2 x16 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x7 x3 x8 x5 x4 x2 x16 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Bool -> Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_6 x7 x3 x8 x19 x5 x4 x2 x16 x18 x3250 x3500 = case x18 of
     (Curry_Prelude.OP_Tuple2 x20 x21) -> d_C_multiGroup x16 x2 x3 x4 x5 x19 x20 (acceptCs id (d_OP_multiGroup_dot___hash_lambda19 x21 x8 x3 x7)) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x7 x3 x8 x19 x5 x4 x2 x16 x1002 x3250 x3500) (d_OP__case_6 x7 x3 x8 x19 x5 x4 x2 x16 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x7 x3 x8 x19 x5 x4 x2 x16 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x7 x3 x8 x19 x5 x4 x2 x16 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_6 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Dequeue.C_Queue (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func Curry_Prelude.C_Bool (Func (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))) -> IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_6 x7 x3 x8 x19 x5 x4 x2 x16 x18 x3000 x3250 x3500 = case x18 of
     (Curry_Prelude.OP_Tuple2 x20 x21) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_multiGroup x16 x2 x3 x4 x5 x19 x20 (wrapDX (wrapNX id) (acceptCs id (nd_OP_multiGroup_dot___hash_lambda19 x21 x8 x3 x7))) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x7 x3 x8 x19 x5 x4 x2 x16 x1002 x3000 x3250 x3500) (nd_OP__case_6 x7 x3 x8 x19 x5 x4 x2 x16 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x7 x3 x8 x19 x5 x4 x2 x16 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x7 x3 x8 x19 x5 x4 x2 x16 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_9 x7 x8 x3 x5 x1 x6 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x5 (Curry_Prelude.d_OP_minus x6 x1 x3250 x3500) x3250 x3500) x7 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_replicate x8 (Curry_Prelude.C_Char ' '#) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x5 (Curry_Prelude.d_OP_minus x3 x8 x3250 x3500) x3250 x3500) x7 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x7 x8 x3 x5 x1 x6 x2 x1002 x3250 x3500) (d_OP__case_9 x7 x8 x3 x5 x1 x6 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x7 x8 x3 x5 x1 x6 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x7 x8 x3 x5 x1 x6 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_9 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Int (Func (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_9 x7 x8 x3 x5 x1 x6 x2 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus x2 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x5 (Curry_Prelude.d_OP_minus x6 x1 x3250 x3500) x2000 x3250 x3500) x7 x2001 x3250 x3500)))) x3250 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_replicate x8 (Curry_Prelude.C_Char ' '#) x3250 x3500) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x5 (Curry_Prelude.d_OP_minus x3 x8 x3250 x3500) x2000 x3250 x3500) x7 x2001 x3250 x3500)))) x3250 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x7 x8 x3 x5 x1 x6 x2 x1002 x3000 x3250 x3500) (nd_OP__case_9 x7 x8 x3 x5 x1 x6 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x7 x8 x3 x5 x1 x6 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x7 x8 x3 x5 x1 x6 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_10 x2 x3 x10 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x11 x12) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_replicate x11 (Curry_Prelude.C_Char ' '#) x3250 x3500) (d_C_noGroup x10 x2 (Curry_Prelude.d_OP_plus x3 (Curry_Prelude.C_Int 1#) x3250 x3500) (Curry_Prelude.d_OP_minus x2 x11 x3250 x3500) x5 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x2 x3 x10 x1002 x3250 x3500) (d_OP__case_10 x2 x3 x10 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x2 x3 x10 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x2 x3 x10 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_10 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Tokens -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_10 x2 x3 x10 x5 x3000 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x11 x12) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_replicate x11 (Curry_Prelude.C_Char ' '#) x3250 x3500) (nd_C_noGroup x10 x2 (Curry_Prelude.d_OP_plus x3 (Curry_Prelude.C_Int 1#) x3250 x3500) (Curry_Prelude.d_OP_minus x2 x11 x3250 x3500) x5 x2000 x3250 x3500) x3250 x3500)))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x2 x3 x10 x1002 x3000 x3250 x3500) (nd_OP__case_10 x2 x3 x10 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x2 x3 x10 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x2 x3 x10 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Doc
d_OP__case_11 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> d_C_line x3250 x3500
     Curry_Prelude.C_False -> d_C_char x1 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1 x1002 x3250 x3500) (d_OP__case_11 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_11 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_Doc
nd_OP__case_11 x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_line x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_char x1 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1 x1002 x3000 x3250 x3500) (nd_OP__case_11 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: C_Doc -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc
d_OP__case_12 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_group (d_OP_lt_gt (d_C_linebreak x3250 x3500) x2 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.OP_Cons (d_C_group (d_OP_lt_gt (d_C_linebreak x3250 x3500) (d_C_group (d_OP_lt_gt x2 (d_C_linebreak x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (d_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 x3 x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x2 x1002 x3250 x3500) (d_OP__case_12 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_12 :: C_Doc -> Curry_Prelude.OP_List C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc
nd_OP__case_12 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2004 = x3000
           in (seq x2004 (Curry_Prelude.OP_Cons (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_C_group (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_OP_lt_gt (nd_C_linebreak x2000 x3250 x3500) x2 x2001 x3250 x3500)))) x2003 x3250 x3500)))) Curry_Prelude.OP_List))
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2012 = x3000
           in (seq x2012 (let
               x2010 = leftSupply x2012
               x2011 = rightSupply x2012
                in (seq x2010 (seq x2011 (Curry_Prelude.OP_Cons (let
                    x2009 = leftSupply x2010
                    x2007 = rightSupply x2010
                     in (seq x2009 (seq x2007 (nd_C_group (let
                         x2006 = leftSupply x2007
                         x2008 = rightSupply x2007
                          in (seq x2006 (seq x2008 (let
                              x2000 = leftSupply x2008
                              x2005 = rightSupply x2008
                               in (seq x2000 (seq x2005 (nd_OP_lt_gt (nd_C_linebreak x2000 x3250 x3500) (let
                                   x2004 = leftSupply x2005
                                   x2003 = rightSupply x2005
                                    in (seq x2004 (seq x2003 (nd_C_group (let
                                        x2002 = leftSupply x2003
                                        x2001 = rightSupply x2003
                                         in (seq x2002 (seq x2001 (nd_OP_lt_gt x2 (nd_C_linebreak x2001 x3250 x3500) x2002 x3250 x3500)))) x2004 x3250 x3500)))) x2006 x3250 x3500))))))) x2009 x3250 x3500)))) (nd_OP_fillEncloseSep_dot_withSoftBreaks_dot_97 x3 x2011 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x2 x1002 x3000 x3250 x3500) (nd_OP__case_12 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: C_Doc -> C_Doc -> Curry_Prelude.OP_List C_Doc -> Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc
d_OP__case_13 x1 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.OP_Cons (d_OP_lt_gt x3 x1 x3250 x3500) (d_OP_punctuate_dot_go_dot_76 x1 x4 x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1 x3 x1002 x3250 x3500) (d_OP__case_13 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_13 :: C_Doc -> C_Doc -> Curry_Prelude.OP_List C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_Doc
nd_OP__case_13 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_OP_lt_gt x3 x1 x2000 x3250 x3500) (nd_OP_punctuate_dot_go_dot_76 x1 x4 x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_13 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo