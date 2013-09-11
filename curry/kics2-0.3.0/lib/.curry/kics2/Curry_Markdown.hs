{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Markdown (C_MarkdownElem (..), C_MarkdownDoc, C_SourceMDElem, d_C_fromMarkdownText, nd_C_fromMarkdownText, d_C_removeEscapes, d_C_markdownText2HTML, nd_C_markdownText2HTML, d_C_markdownText2CompleteHTML, d_C_markdownText2LaTeX, nd_C_markdownText2LaTeX, d_C_markdownText2LaTeXWithFormat, nd_C_markdownText2LaTeXWithFormat, d_C_markdownText2CompleteLaTeX, d_C_formatMarkdownInputAsPDF, d_C_formatMarkdownFileAsPDF) where

import Basics
import qualified Curry_Char
import qualified Curry_HTML
import qualified Curry_HtmlParser
import qualified Curry_IO
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_System
type C_MarkdownDoc = Curry_Prelude.OP_List C_MarkdownElem

data C_MarkdownElem
     = C_Text (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_Emph (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_Strong (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_Code (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_HRef (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_Par (Curry_Prelude.OP_List C_MarkdownElem)
     | C_CodeBlock (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_UList (Curry_Prelude.OP_List (Curry_Prelude.OP_List C_MarkdownElem))
     | C_OList (Curry_Prelude.OP_List (Curry_Prelude.OP_List C_MarkdownElem))
     | C_Quote (Curry_Prelude.OP_List C_MarkdownElem)
     | C_HRule
     | C_Header Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_MarkdownElem Cover ID C_MarkdownElem C_MarkdownElem
     | Choices_C_MarkdownElem Cover ID ([C_MarkdownElem])
     | Fail_C_MarkdownElem Cover FailInfo
     | Guard_C_MarkdownElem Cover Constraints C_MarkdownElem

instance Show C_MarkdownElem where
  showsPrec d (Choice_C_MarkdownElem cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_MarkdownElem cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_MarkdownElem cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_MarkdownElem cd info) = showChar '!'
  showsPrec _ (C_Text x1) = (showString "(Text") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Emph x1) = (showString "(Emph") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Strong x1) = (showString "(Strong") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Code x1) = (showString "(Code") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_HRef x1 x2) = (showString "(HRef") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_Par x1) = (showString "(Par") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CodeBlock x1) = (showString "(CodeBlock") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_UList x1) = (showString "(UList") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_OList x1) = (showString "(OList") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Quote x1) = (showString "(Quote") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ C_HRule = showString "HRule"
  showsPrec _ (C_Header x1 x2) = (showString "(Header") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_MarkdownElem where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Text x1,r1) | (_,r0) <- readQualified "Markdown" "Text" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Emph x1,r1) | (_,r0) <- readQualified "Markdown" "Emph" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Strong x1,r1) | (_,r0) <- readQualified "Markdown" "Strong" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Code x1,r1) | (_,r0) <- readQualified "Markdown" "Code" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_HRef x1 x2,r2) | (_,r0) <- readQualified "Markdown" "HRef" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_Par x1,r1) | (_,r0) <- readQualified "Markdown" "Par" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_CodeBlock x1,r1) | (_,r0) <- readQualified "Markdown" "CodeBlock" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_UList x1,r1) | (_,r0) <- readQualified "Markdown" "UList" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_OList x1,r1) | (_,r0) <- readQualified "Markdown" "OList" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Quote x1,r1) | (_,r0) <- readQualified "Markdown" "Quote" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen False (\r -> [ (C_HRule,r0) | (_,r0) <- readQualified "Markdown" "HRule" r]) s) ++ (readParen (d > 10) (\r -> [ (C_Header x1 x2,r2) | (_,r0) <- readQualified "Markdown" "Header" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)))))))))))


instance NonDet C_MarkdownElem where
  choiceCons = Choice_C_MarkdownElem
  choicesCons = Choices_C_MarkdownElem
  failCons = Fail_C_MarkdownElem
  guardCons = Guard_C_MarkdownElem
  try (Choice_C_MarkdownElem cd i x y) = tryChoice cd i x y
  try (Choices_C_MarkdownElem cd i xs) = tryChoices cd i xs
  try (Fail_C_MarkdownElem cd info) = Fail cd info
  try (Guard_C_MarkdownElem cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_MarkdownElem cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_MarkdownElem cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_MarkdownElem cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_MarkdownElem cd i _) = error ("Markdown.MarkdownElem.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_MarkdownElem cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_MarkdownElem cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_MarkdownElem where
  generate s c = Choices_C_MarkdownElem c (freeID [1,1,1,1,2,1,1,1,1,1,0,2] s) [(C_Text (generate (leftSupply s) c)),(C_Emph (generate (leftSupply s) c)),(C_Strong (generate (leftSupply s) c)),(C_Code (generate (leftSupply s) c)),(C_HRef (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_Par (generate (leftSupply s) c)),(C_CodeBlock (generate (leftSupply s) c)),(C_UList (generate (leftSupply s) c)),(C_OList (generate (leftSupply s) c)),(C_Quote (generate (leftSupply s) c)),C_HRule,(C_Header (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_MarkdownElem where
  ($!!) cont (C_Text x1) d cs = (((\y1 d cs -> cont (C_Text y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Emph x1) d cs = (((\y1 d cs -> cont (C_Emph y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Strong x1) d cs = (((\y1 d cs -> cont (C_Strong y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Code x1) d cs = (((\y1 d cs -> cont (C_Code y1) d cs) $!! x1) d) cs
  ($!!) cont (C_HRef x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_HRef y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Par x1) d cs = (((\y1 d cs -> cont (C_Par y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CodeBlock x1) d cs = (((\y1 d cs -> cont (C_CodeBlock y1) d cs) $!! x1) d) cs
  ($!!) cont (C_UList x1) d cs = (((\y1 d cs -> cont (C_UList y1) d cs) $!! x1) d) cs
  ($!!) cont (C_OList x1) d cs = (((\y1 d cs -> cont (C_OList y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Quote x1) d cs = (((\y1 d cs -> cont (C_Quote y1) d cs) $!! x1) d) cs
  ($!!) cont C_HRule d cs = cont C_HRule d cs
  ($!!) cont (C_Header x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Header y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_MarkdownElem cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_MarkdownElem cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_MarkdownElem cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_MarkdownElem cd info) _ _ = failCons cd info
  ($##) cont (C_Text x1) d cs = (((\y1 d cs -> cont (C_Text y1) d cs) $## x1) d) cs
  ($##) cont (C_Emph x1) d cs = (((\y1 d cs -> cont (C_Emph y1) d cs) $## x1) d) cs
  ($##) cont (C_Strong x1) d cs = (((\y1 d cs -> cont (C_Strong y1) d cs) $## x1) d) cs
  ($##) cont (C_Code x1) d cs = (((\y1 d cs -> cont (C_Code y1) d cs) $## x1) d) cs
  ($##) cont (C_HRef x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_HRef y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Par x1) d cs = (((\y1 d cs -> cont (C_Par y1) d cs) $## x1) d) cs
  ($##) cont (C_CodeBlock x1) d cs = (((\y1 d cs -> cont (C_CodeBlock y1) d cs) $## x1) d) cs
  ($##) cont (C_UList x1) d cs = (((\y1 d cs -> cont (C_UList y1) d cs) $## x1) d) cs
  ($##) cont (C_OList x1) d cs = (((\y1 d cs -> cont (C_OList y1) d cs) $## x1) d) cs
  ($##) cont (C_Quote x1) d cs = (((\y1 d cs -> cont (C_Quote y1) d cs) $## x1) d) cs
  ($##) cont C_HRule d cs = cont C_HRule d cs
  ($##) cont (C_Header x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Header y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_MarkdownElem cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_MarkdownElem cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_MarkdownElem cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_MarkdownElem cd info) _ _ = failCons cd info
  searchNF search cont (C_Text x1) = search (\y1 -> cont (C_Text y1)) x1
  searchNF search cont (C_Emph x1) = search (\y1 -> cont (C_Emph y1)) x1
  searchNF search cont (C_Strong x1) = search (\y1 -> cont (C_Strong y1)) x1
  searchNF search cont (C_Code x1) = search (\y1 -> cont (C_Code y1)) x1
  searchNF search cont (C_HRef x1 x2) = search (\y1 -> search (\y2 -> cont (C_HRef y1 y2)) x2) x1
  searchNF search cont (C_Par x1) = search (\y1 -> cont (C_Par y1)) x1
  searchNF search cont (C_CodeBlock x1) = search (\y1 -> cont (C_CodeBlock y1)) x1
  searchNF search cont (C_UList x1) = search (\y1 -> cont (C_UList y1)) x1
  searchNF search cont (C_OList x1) = search (\y1 -> cont (C_OList y1)) x1
  searchNF search cont (C_Quote x1) = search (\y1 -> cont (C_Quote y1)) x1
  searchNF _ cont C_HRule = cont C_HRule
  searchNF search cont (C_Header x1 x2) = search (\y1 -> search (\y2 -> cont (C_Header y1 y2)) x2) x1
  searchNF _ _ x = error ("Markdown.MarkdownElem.searchNF: no constructor: " ++ (show x))


instance Unifiable C_MarkdownElem where
  (=.=) (C_Text x1) (C_Text y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Emph x1) (C_Emph y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Strong x1) (C_Strong y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Code x1) (C_Code y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_HRef x1 x2) (C_HRef y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_Par x1) (C_Par y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CodeBlock x1) (C_CodeBlock y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_UList x1) (C_UList y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_OList x1) (C_OList y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Quote x1) (C_Quote y1) d cs = ((x1 =:= y1) d) cs
  (=.=) C_HRule C_HRule d cs = C_Success
  (=.=) (C_Header x1 x2) (C_Header y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Text x1) (C_Text y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Emph x1) (C_Emph y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Strong x1) (C_Strong y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Code x1) (C_Code y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_HRef x1 x2) (C_HRef y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_Par x1) (C_Par y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CodeBlock x1) (C_CodeBlock y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_UList x1) (C_UList y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_OList x1) (C_OList y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Quote x1) (C_Quote y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) C_HRule C_HRule d cs = C_Success
  (=.<=) (C_Header x1 x2) (C_Header y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Text x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Emph x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Strong x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Code x3) = ((i :=: (ChooseN 3 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_HRef x3 x4) = ((i :=: (ChooseN 4 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_Par x3) = ((i :=: (ChooseN 5 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CodeBlock x3) = ((i :=: (ChooseN 6 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_UList x3) = ((i :=: (ChooseN 7 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_OList x3) = ((i :=: (ChooseN 8 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Quote x3) = ((i :=: (ChooseN 9 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i C_HRule = ((i :=: (ChooseN 10 0)):(concat []))
  bind cd i (C_Header x3 x4) = ((i :=: (ChooseN 11 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_MarkdownElem cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_MarkdownElem cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_MarkdownElem cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_MarkdownElem cd i _) = error ("Markdown.MarkdownElem.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_MarkdownElem cd info) = [(Unsolvable info)]
  bind d i (Guard_C_MarkdownElem cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Text x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Emph x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Strong x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Code x3) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_HRef x3 x4) = [(i :=: (ChooseN 4 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_Par x3) = [(i :=: (ChooseN 5 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CodeBlock x3) = [(i :=: (ChooseN 6 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_UList x3) = [(i :=: (ChooseN 7 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_OList x3) = [(i :=: (ChooseN 8 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Quote x3) = [(i :=: (ChooseN 9 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i C_HRule = [(i :=: (ChooseN 10 0))]
  lazyBind cd i (C_Header x3 x4) = [(i :=: (ChooseN 11 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_MarkdownElem cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_MarkdownElem cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_MarkdownElem cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_MarkdownElem cd i _) = error ("Markdown.MarkdownElem.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_MarkdownElem cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_MarkdownElem cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_MarkdownElem where
  (=?=) (Choice_C_MarkdownElem cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_MarkdownElem cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_MarkdownElem cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_MarkdownElem cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_MarkdownElem cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_MarkdownElem cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_MarkdownElem cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_MarkdownElem cd info) _ _ = failCons cd info
  (=?=) (C_Text x1) (C_Text y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Emph x1) (C_Emph y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Strong x1) (C_Strong y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Code x1) (C_Code y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_HRef x1 x2) (C_HRef y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_Par x1) (C_Par y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CodeBlock x1) (C_CodeBlock y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_UList x1) (C_UList y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_OList x1) (C_OList y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Quote x1) (C_Quote y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) C_HRule C_HRule d cs = Curry_Prelude.C_True
  (=?=) (C_Header x1 x2) (C_Header y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_MarkdownElem cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_MarkdownElem cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_MarkdownElem cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_MarkdownElem cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_MarkdownElem cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_MarkdownElem cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_MarkdownElem cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_MarkdownElem cd info) _ _ = failCons cd info
  (<?=) (C_Text x1) (C_Text y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Text _) (C_Emph _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _) (C_Strong _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _) (C_Code _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _) (C_HRef _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _) (C_Par _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _) (C_CodeBlock _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _) (C_UList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _) (C_OList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _) (C_Quote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _) C_HRule _ _ = Curry_Prelude.C_True
  (<?=) (C_Text _) (C_Header _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Emph x1) (C_Emph y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Emph _) (C_Strong _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Emph _) (C_Code _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Emph _) (C_HRef _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Emph _) (C_Par _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Emph _) (C_CodeBlock _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Emph _) (C_UList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Emph _) (C_OList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Emph _) (C_Quote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Emph _) C_HRule _ _ = Curry_Prelude.C_True
  (<?=) (C_Emph _) (C_Header _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Strong x1) (C_Strong y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Strong _) (C_Code _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Strong _) (C_HRef _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Strong _) (C_Par _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Strong _) (C_CodeBlock _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Strong _) (C_UList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Strong _) (C_OList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Strong _) (C_Quote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Strong _) C_HRule _ _ = Curry_Prelude.C_True
  (<?=) (C_Strong _) (C_Header _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Code x1) (C_Code y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Code _) (C_HRef _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Code _) (C_Par _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Code _) (C_CodeBlock _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Code _) (C_UList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Code _) (C_OList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Code _) (C_Quote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Code _) C_HRule _ _ = Curry_Prelude.C_True
  (<?=) (C_Code _) (C_Header _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_HRef x1 x2) (C_HRef y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_HRef _ _) (C_Par _) _ _ = Curry_Prelude.C_True
  (<?=) (C_HRef _ _) (C_CodeBlock _) _ _ = Curry_Prelude.C_True
  (<?=) (C_HRef _ _) (C_UList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_HRef _ _) (C_OList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_HRef _ _) (C_Quote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_HRef _ _) C_HRule _ _ = Curry_Prelude.C_True
  (<?=) (C_HRef _ _) (C_Header _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Par x1) (C_Par y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Par _) (C_CodeBlock _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Par _) (C_UList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Par _) (C_OList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Par _) (C_Quote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Par _) C_HRule _ _ = Curry_Prelude.C_True
  (<?=) (C_Par _) (C_Header _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CodeBlock x1) (C_CodeBlock y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CodeBlock _) (C_UList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CodeBlock _) (C_OList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CodeBlock _) (C_Quote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CodeBlock _) C_HRule _ _ = Curry_Prelude.C_True
  (<?=) (C_CodeBlock _) (C_Header _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_UList x1) (C_UList y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_UList _) (C_OList _) _ _ = Curry_Prelude.C_True
  (<?=) (C_UList _) (C_Quote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_UList _) C_HRule _ _ = Curry_Prelude.C_True
  (<?=) (C_UList _) (C_Header _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_OList x1) (C_OList y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_OList _) (C_Quote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_OList _) C_HRule _ _ = Curry_Prelude.C_True
  (<?=) (C_OList _) (C_Header _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Quote x1) (C_Quote y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Quote _) C_HRule _ _ = Curry_Prelude.C_True
  (<?=) (C_Quote _) (C_Header _ _) _ _ = Curry_Prelude.C_True
  (<?=) C_HRule C_HRule d cs = Curry_Prelude.C_True
  (<?=) C_HRule (C_Header _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Header x1 x2) (C_Header y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_SourceMDElem
     = C_SMDText (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_SMDEmph (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_SMDStrong (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_SMDCode (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_SMDHRef (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_SMDPar (Curry_Prelude.OP_List C_MarkdownElem)
     | C_SMDCodeBlock (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_SMDUItem (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_SMDOItem (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_SMDQuote (Curry_Prelude.OP_List C_MarkdownElem)
     | C_SMDHRule
     | C_SMDHeader Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_SourceMDElem Cover ID C_SourceMDElem C_SourceMDElem
     | Choices_C_SourceMDElem Cover ID ([C_SourceMDElem])
     | Fail_C_SourceMDElem Cover FailInfo
     | Guard_C_SourceMDElem Cover Constraints C_SourceMDElem

instance Show C_SourceMDElem where
  showsPrec d (Choice_C_SourceMDElem cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_SourceMDElem cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_SourceMDElem cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_SourceMDElem cd info) = showChar '!'
  showsPrec _ (C_SMDText x1) = (showString "(SMDText") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_SMDEmph x1) = (showString "(SMDEmph") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_SMDStrong x1) = (showString "(SMDStrong") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_SMDCode x1) = (showString "(SMDCode") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_SMDHRef x1 x2) = (showString "(SMDHRef") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_SMDPar x1) = (showString "(SMDPar") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_SMDCodeBlock x1) = (showString "(SMDCodeBlock") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_SMDUItem x1) = (showString "(SMDUItem") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_SMDOItem x1) = (showString "(SMDOItem") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_SMDQuote x1) = (showString "(SMDQuote") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ C_SMDHRule = showString "SMDHRule"
  showsPrec _ (C_SMDHeader x1 x2) = (showString "(SMDHeader") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_SourceMDElem where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_SMDText x1,r1) | (_,r0) <- readQualified "Markdown" "SMDText" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_SMDEmph x1,r1) | (_,r0) <- readQualified "Markdown" "SMDEmph" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_SMDStrong x1,r1) | (_,r0) <- readQualified "Markdown" "SMDStrong" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_SMDCode x1,r1) | (_,r0) <- readQualified "Markdown" "SMDCode" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_SMDHRef x1 x2,r2) | (_,r0) <- readQualified "Markdown" "SMDHRef" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_SMDPar x1,r1) | (_,r0) <- readQualified "Markdown" "SMDPar" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_SMDCodeBlock x1,r1) | (_,r0) <- readQualified "Markdown" "SMDCodeBlock" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_SMDUItem x1,r1) | (_,r0) <- readQualified "Markdown" "SMDUItem" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_SMDOItem x1,r1) | (_,r0) <- readQualified "Markdown" "SMDOItem" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_SMDQuote x1,r1) | (_,r0) <- readQualified "Markdown" "SMDQuote" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen False (\r -> [ (C_SMDHRule,r0) | (_,r0) <- readQualified "Markdown" "SMDHRule" r]) s) ++ (readParen (d > 10) (\r -> [ (C_SMDHeader x1 x2,r2) | (_,r0) <- readQualified "Markdown" "SMDHeader" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)))))))))))


instance NonDet C_SourceMDElem where
  choiceCons = Choice_C_SourceMDElem
  choicesCons = Choices_C_SourceMDElem
  failCons = Fail_C_SourceMDElem
  guardCons = Guard_C_SourceMDElem
  try (Choice_C_SourceMDElem cd i x y) = tryChoice cd i x y
  try (Choices_C_SourceMDElem cd i xs) = tryChoices cd i xs
  try (Fail_C_SourceMDElem cd info) = Fail cd info
  try (Guard_C_SourceMDElem cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_SourceMDElem cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_SourceMDElem cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_SourceMDElem cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_SourceMDElem cd i _) = error ("Markdown.SourceMDElem.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_SourceMDElem cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_SourceMDElem cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_SourceMDElem where
  generate s c = Choices_C_SourceMDElem c (freeID [1,1,1,1,2,1,1,1,1,1,0,2] s) [(C_SMDText (generate (leftSupply s) c)),(C_SMDEmph (generate (leftSupply s) c)),(C_SMDStrong (generate (leftSupply s) c)),(C_SMDCode (generate (leftSupply s) c)),(C_SMDHRef (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_SMDPar (generate (leftSupply s) c)),(C_SMDCodeBlock (generate (leftSupply s) c)),(C_SMDUItem (generate (leftSupply s) c)),(C_SMDOItem (generate (leftSupply s) c)),(C_SMDQuote (generate (leftSupply s) c)),C_SMDHRule,(C_SMDHeader (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_SourceMDElem where
  ($!!) cont (C_SMDText x1) d cs = (((\y1 d cs -> cont (C_SMDText y1) d cs) $!! x1) d) cs
  ($!!) cont (C_SMDEmph x1) d cs = (((\y1 d cs -> cont (C_SMDEmph y1) d cs) $!! x1) d) cs
  ($!!) cont (C_SMDStrong x1) d cs = (((\y1 d cs -> cont (C_SMDStrong y1) d cs) $!! x1) d) cs
  ($!!) cont (C_SMDCode x1) d cs = (((\y1 d cs -> cont (C_SMDCode y1) d cs) $!! x1) d) cs
  ($!!) cont (C_SMDHRef x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_SMDHRef y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_SMDPar x1) d cs = (((\y1 d cs -> cont (C_SMDPar y1) d cs) $!! x1) d) cs
  ($!!) cont (C_SMDCodeBlock x1) d cs = (((\y1 d cs -> cont (C_SMDCodeBlock y1) d cs) $!! x1) d) cs
  ($!!) cont (C_SMDUItem x1) d cs = (((\y1 d cs -> cont (C_SMDUItem y1) d cs) $!! x1) d) cs
  ($!!) cont (C_SMDOItem x1) d cs = (((\y1 d cs -> cont (C_SMDOItem y1) d cs) $!! x1) d) cs
  ($!!) cont (C_SMDQuote x1) d cs = (((\y1 d cs -> cont (C_SMDQuote y1) d cs) $!! x1) d) cs
  ($!!) cont C_SMDHRule d cs = cont C_SMDHRule d cs
  ($!!) cont (C_SMDHeader x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_SMDHeader y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_SourceMDElem cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_SourceMDElem cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_SourceMDElem cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_SourceMDElem cd info) _ _ = failCons cd info
  ($##) cont (C_SMDText x1) d cs = (((\y1 d cs -> cont (C_SMDText y1) d cs) $## x1) d) cs
  ($##) cont (C_SMDEmph x1) d cs = (((\y1 d cs -> cont (C_SMDEmph y1) d cs) $## x1) d) cs
  ($##) cont (C_SMDStrong x1) d cs = (((\y1 d cs -> cont (C_SMDStrong y1) d cs) $## x1) d) cs
  ($##) cont (C_SMDCode x1) d cs = (((\y1 d cs -> cont (C_SMDCode y1) d cs) $## x1) d) cs
  ($##) cont (C_SMDHRef x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_SMDHRef y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_SMDPar x1) d cs = (((\y1 d cs -> cont (C_SMDPar y1) d cs) $## x1) d) cs
  ($##) cont (C_SMDCodeBlock x1) d cs = (((\y1 d cs -> cont (C_SMDCodeBlock y1) d cs) $## x1) d) cs
  ($##) cont (C_SMDUItem x1) d cs = (((\y1 d cs -> cont (C_SMDUItem y1) d cs) $## x1) d) cs
  ($##) cont (C_SMDOItem x1) d cs = (((\y1 d cs -> cont (C_SMDOItem y1) d cs) $## x1) d) cs
  ($##) cont (C_SMDQuote x1) d cs = (((\y1 d cs -> cont (C_SMDQuote y1) d cs) $## x1) d) cs
  ($##) cont C_SMDHRule d cs = cont C_SMDHRule d cs
  ($##) cont (C_SMDHeader x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_SMDHeader y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_SourceMDElem cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_SourceMDElem cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_SourceMDElem cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_SourceMDElem cd info) _ _ = failCons cd info
  searchNF search cont (C_SMDText x1) = search (\y1 -> cont (C_SMDText y1)) x1
  searchNF search cont (C_SMDEmph x1) = search (\y1 -> cont (C_SMDEmph y1)) x1
  searchNF search cont (C_SMDStrong x1) = search (\y1 -> cont (C_SMDStrong y1)) x1
  searchNF search cont (C_SMDCode x1) = search (\y1 -> cont (C_SMDCode y1)) x1
  searchNF search cont (C_SMDHRef x1 x2) = search (\y1 -> search (\y2 -> cont (C_SMDHRef y1 y2)) x2) x1
  searchNF search cont (C_SMDPar x1) = search (\y1 -> cont (C_SMDPar y1)) x1
  searchNF search cont (C_SMDCodeBlock x1) = search (\y1 -> cont (C_SMDCodeBlock y1)) x1
  searchNF search cont (C_SMDUItem x1) = search (\y1 -> cont (C_SMDUItem y1)) x1
  searchNF search cont (C_SMDOItem x1) = search (\y1 -> cont (C_SMDOItem y1)) x1
  searchNF search cont (C_SMDQuote x1) = search (\y1 -> cont (C_SMDQuote y1)) x1
  searchNF _ cont C_SMDHRule = cont C_SMDHRule
  searchNF search cont (C_SMDHeader x1 x2) = search (\y1 -> search (\y2 -> cont (C_SMDHeader y1 y2)) x2) x1
  searchNF _ _ x = error ("Markdown.SourceMDElem.searchNF: no constructor: " ++ (show x))


instance Unifiable C_SourceMDElem where
  (=.=) (C_SMDText x1) (C_SMDText y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_SMDEmph x1) (C_SMDEmph y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_SMDStrong x1) (C_SMDStrong y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_SMDCode x1) (C_SMDCode y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_SMDHRef x1 x2) (C_SMDHRef y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_SMDPar x1) (C_SMDPar y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_SMDCodeBlock x1) (C_SMDCodeBlock y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_SMDUItem x1) (C_SMDUItem y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_SMDOItem x1) (C_SMDOItem y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_SMDQuote x1) (C_SMDQuote y1) d cs = ((x1 =:= y1) d) cs
  (=.=) C_SMDHRule C_SMDHRule d cs = C_Success
  (=.=) (C_SMDHeader x1 x2) (C_SMDHeader y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_SMDText x1) (C_SMDText y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_SMDEmph x1) (C_SMDEmph y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_SMDStrong x1) (C_SMDStrong y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_SMDCode x1) (C_SMDCode y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_SMDHRef x1 x2) (C_SMDHRef y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_SMDPar x1) (C_SMDPar y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_SMDCodeBlock x1) (C_SMDCodeBlock y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_SMDUItem x1) (C_SMDUItem y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_SMDOItem x1) (C_SMDOItem y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_SMDQuote x1) (C_SMDQuote y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) C_SMDHRule C_SMDHRule d cs = C_Success
  (=.<=) (C_SMDHeader x1 x2) (C_SMDHeader y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_SMDText x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_SMDEmph x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_SMDStrong x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_SMDCode x3) = ((i :=: (ChooseN 3 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_SMDHRef x3 x4) = ((i :=: (ChooseN 4 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_SMDPar x3) = ((i :=: (ChooseN 5 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_SMDCodeBlock x3) = ((i :=: (ChooseN 6 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_SMDUItem x3) = ((i :=: (ChooseN 7 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_SMDOItem x3) = ((i :=: (ChooseN 8 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_SMDQuote x3) = ((i :=: (ChooseN 9 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i C_SMDHRule = ((i :=: (ChooseN 10 0)):(concat []))
  bind cd i (C_SMDHeader x3 x4) = ((i :=: (ChooseN 11 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_SourceMDElem cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_SourceMDElem cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_SourceMDElem cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_SourceMDElem cd i _) = error ("Markdown.SourceMDElem.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_SourceMDElem cd info) = [(Unsolvable info)]
  bind d i (Guard_C_SourceMDElem cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_SMDText x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_SMDEmph x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_SMDStrong x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_SMDCode x3) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_SMDHRef x3 x4) = [(i :=: (ChooseN 4 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_SMDPar x3) = [(i :=: (ChooseN 5 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_SMDCodeBlock x3) = [(i :=: (ChooseN 6 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_SMDUItem x3) = [(i :=: (ChooseN 7 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_SMDOItem x3) = [(i :=: (ChooseN 8 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_SMDQuote x3) = [(i :=: (ChooseN 9 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i C_SMDHRule = [(i :=: (ChooseN 10 0))]
  lazyBind cd i (C_SMDHeader x3 x4) = [(i :=: (ChooseN 11 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_SourceMDElem cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_SourceMDElem cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_SourceMDElem cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_SourceMDElem cd i _) = error ("Markdown.SourceMDElem.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_SourceMDElem cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_SourceMDElem cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_SourceMDElem where
  (=?=) (Choice_C_SourceMDElem cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_SourceMDElem cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_SourceMDElem cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_SourceMDElem cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_SourceMDElem cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_SourceMDElem cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_SourceMDElem cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_SourceMDElem cd info) _ _ = failCons cd info
  (=?=) (C_SMDText x1) (C_SMDText y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_SMDEmph x1) (C_SMDEmph y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_SMDStrong x1) (C_SMDStrong y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_SMDCode x1) (C_SMDCode y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_SMDHRef x1 x2) (C_SMDHRef y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_SMDPar x1) (C_SMDPar y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_SMDCodeBlock x1) (C_SMDCodeBlock y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_SMDUItem x1) (C_SMDUItem y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_SMDOItem x1) (C_SMDOItem y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_SMDQuote x1) (C_SMDQuote y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) C_SMDHRule C_SMDHRule d cs = Curry_Prelude.C_True
  (=?=) (C_SMDHeader x1 x2) (C_SMDHeader y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_SourceMDElem cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_SourceMDElem cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_SourceMDElem cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_SourceMDElem cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_SourceMDElem cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_SourceMDElem cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_SourceMDElem cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_SourceMDElem cd info) _ _ = failCons cd info
  (<?=) (C_SMDText x1) (C_SMDText y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_SMDText _) (C_SMDEmph _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDText _) (C_SMDStrong _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDText _) (C_SMDCode _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDText _) (C_SMDHRef _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDText _) (C_SMDPar _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDText _) (C_SMDCodeBlock _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDText _) (C_SMDUItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDText _) (C_SMDOItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDText _) (C_SMDQuote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDText _) C_SMDHRule _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDText _) (C_SMDHeader _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDEmph x1) (C_SMDEmph y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_SMDEmph _) (C_SMDStrong _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDEmph _) (C_SMDCode _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDEmph _) (C_SMDHRef _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDEmph _) (C_SMDPar _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDEmph _) (C_SMDCodeBlock _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDEmph _) (C_SMDUItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDEmph _) (C_SMDOItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDEmph _) (C_SMDQuote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDEmph _) C_SMDHRule _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDEmph _) (C_SMDHeader _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDStrong x1) (C_SMDStrong y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_SMDStrong _) (C_SMDCode _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDStrong _) (C_SMDHRef _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDStrong _) (C_SMDPar _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDStrong _) (C_SMDCodeBlock _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDStrong _) (C_SMDUItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDStrong _) (C_SMDOItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDStrong _) (C_SMDQuote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDStrong _) C_SMDHRule _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDStrong _) (C_SMDHeader _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDCode x1) (C_SMDCode y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_SMDCode _) (C_SMDHRef _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDCode _) (C_SMDPar _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDCode _) (C_SMDCodeBlock _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDCode _) (C_SMDUItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDCode _) (C_SMDOItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDCode _) (C_SMDQuote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDCode _) C_SMDHRule _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDCode _) (C_SMDHeader _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDHRef x1 x2) (C_SMDHRef y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_SMDHRef _ _) (C_SMDPar _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDHRef _ _) (C_SMDCodeBlock _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDHRef _ _) (C_SMDUItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDHRef _ _) (C_SMDOItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDHRef _ _) (C_SMDQuote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDHRef _ _) C_SMDHRule _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDHRef _ _) (C_SMDHeader _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDPar x1) (C_SMDPar y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_SMDPar _) (C_SMDCodeBlock _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDPar _) (C_SMDUItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDPar _) (C_SMDOItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDPar _) (C_SMDQuote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDPar _) C_SMDHRule _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDPar _) (C_SMDHeader _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDCodeBlock x1) (C_SMDCodeBlock y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_SMDCodeBlock _) (C_SMDUItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDCodeBlock _) (C_SMDOItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDCodeBlock _) (C_SMDQuote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDCodeBlock _) C_SMDHRule _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDCodeBlock _) (C_SMDHeader _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDUItem x1) (C_SMDUItem y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_SMDUItem _) (C_SMDOItem _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDUItem _) (C_SMDQuote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDUItem _) C_SMDHRule _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDUItem _) (C_SMDHeader _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDOItem x1) (C_SMDOItem y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_SMDOItem _) (C_SMDQuote _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDOItem _) C_SMDHRule _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDOItem _) (C_SMDHeader _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDQuote x1) (C_SMDQuote y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_SMDQuote _) C_SMDHRule _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDQuote _) (C_SMDHeader _ _) _ _ = Curry_Prelude.C_True
  (<?=) C_SMDHRule C_SMDHRule d cs = Curry_Prelude.C_True
  (<?=) C_SMDHRule (C_SMDHeader _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SMDHeader x1 x2) (C_SMDHeader y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_isSMDUItem :: C_SourceMDElem -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isSMDUItem x1 x3250 x3500 = case x1 of
     (C_SMDUItem x2) -> Curry_Prelude.C_True
     (C_SMDText x3) -> Curry_Prelude.C_False
     (C_SMDEmph x4) -> Curry_Prelude.C_False
     (C_SMDStrong x5) -> Curry_Prelude.C_False
     (C_SMDCode x6) -> Curry_Prelude.C_False
     (C_SMDHRef x7 x8) -> Curry_Prelude.C_False
     (C_SMDPar x9) -> Curry_Prelude.C_False
     (C_SMDCodeBlock x10) -> Curry_Prelude.C_False
     (C_SMDOItem x11) -> Curry_Prelude.C_False
     (C_SMDQuote x12) -> Curry_Prelude.C_False
     C_SMDHRule -> Curry_Prelude.C_False
     (C_SMDHeader x13 x14) -> Curry_Prelude.C_False
     (Choice_C_SourceMDElem x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isSMDUItem x1002 x3250 x3500) (d_C_isSMDUItem x1003 x3250 x3500)
     (Choices_C_SourceMDElem x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isSMDUItem z x3250 x3500) x1002
     (Guard_C_SourceMDElem x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isSMDUItem x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SourceMDElem x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isSMDOItem :: C_SourceMDElem -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isSMDOItem x1 x3250 x3500 = case x1 of
     (C_SMDOItem x2) -> Curry_Prelude.C_True
     (C_SMDText x3) -> Curry_Prelude.C_False
     (C_SMDEmph x4) -> Curry_Prelude.C_False
     (C_SMDStrong x5) -> Curry_Prelude.C_False
     (C_SMDCode x6) -> Curry_Prelude.C_False
     (C_SMDHRef x7 x8) -> Curry_Prelude.C_False
     (C_SMDPar x9) -> Curry_Prelude.C_False
     (C_SMDCodeBlock x10) -> Curry_Prelude.C_False
     (C_SMDUItem x11) -> Curry_Prelude.C_False
     (C_SMDQuote x12) -> Curry_Prelude.C_False
     C_SMDHRule -> Curry_Prelude.C_False
     (C_SMDHeader x13 x14) -> Curry_Prelude.C_False
     (Choice_C_SourceMDElem x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isSMDOItem x1002 x3250 x3500) (d_C_isSMDOItem x1003 x3250 x3500)
     (Choices_C_SourceMDElem x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isSMDOItem z x3250 x3500) x1002
     (Guard_C_SourceMDElem x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isSMDOItem x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SourceMDElem x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_textOfItem :: C_SourceMDElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_textOfItem x1 x3250 x3500 = case x1 of
     (C_SMDUItem x2) -> x2
     (C_SMDOItem x3) -> x3
     (Choice_C_SourceMDElem x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_textOfItem x1002 x3250 x3500) (d_C_textOfItem x1003 x3250 x3500)
     (Choices_C_SourceMDElem x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_textOfItem z x3250 x3500) x1002
     (Guard_C_SourceMDElem x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_textOfItem x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SourceMDElem x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_fromMarkdownText :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_MarkdownElem
d_C_fromMarkdownText x3250 x3500 = Curry_Prelude.d_OP_dot d_C_groupMarkDownElems d_C_markdownText x3250 x3500

nd_C_fromMarkdownText :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List C_MarkdownElem)
nd_C_fromMarkdownText x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id d_C_groupMarkDownElems) (wrapDX id d_C_markdownText) x2000 x3250 x3500))

d_C_groupMarkDownElems :: Curry_Prelude.OP_List C_SourceMDElem -> Cover -> ConstStore -> Curry_Prelude.OP_List C_MarkdownElem
d_C_groupMarkDownElems x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_83 x3 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_groupMarkDownElems x1002 x3250 x3500) (d_C_groupMarkDownElems x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_groupMarkDownElems z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_groupMarkDownElems x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_joinItems :: (Curry_Prelude.OP_List (Curry_Prelude.OP_List C_MarkdownElem) -> Cover -> ConstStore -> C_MarkdownElem) -> (C_SourceMDElem -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List C_SourceMDElem -> Cover -> ConstStore -> Curry_Prelude.OP_List C_MarkdownElem
d_C_joinItems x1 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) (Curry_Prelude.d_C_map (d_C_fromMarkdownText x3250 x3500) x3 x3250 x3500) x3250 x3500) x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP__case_82 x5 x2 x6 x3 x1 (Curry_Prelude.d_C_apply x2 x5 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_joinItems x1 x2 x3 x1002 x3250 x3500) (d_C_joinItems x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_joinItems x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_joinItems x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_joinItems :: Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List C_MarkdownElem)) C_MarkdownElem -> Func C_SourceMDElem Curry_Prelude.C_Bool -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List C_SourceMDElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_MarkdownElem
nd_C_joinItems x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2008 = x3000
           in (seq x2008 (Curry_Prelude.OP_Cons (let
               x2007 = leftSupply x2008
               x2005 = rightSupply x2008
                in (seq x2007 (seq x2005 (Curry_Prelude.nd_C_apply x1 (let
                    x2004 = leftSupply x2005
                    x2006 = rightSupply x2005
                     in (seq x2004 (seq x2006 (let
                         x2000 = leftSupply x2006
                         x2003 = rightSupply x2006
                          in (seq x2000 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2000 x3250 x3500) (let
                              x2002 = leftSupply x2003
                              x2001 = rightSupply x2003
                               in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_map (nd_C_fromMarkdownText x2001 x3250 x3500) x3 x2002 x3250 x3500)))) x2004 x3250 x3500))))))) x2007 x3250 x3500)))) Curry_Prelude.OP_List))
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_82 x5 x2 x6 x3 x1 (Curry_Prelude.nd_C_apply x2 x5 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_joinItems x1 x2 x3 x1002 x3000 x3250 x3500) (nd_C_joinItems x1 x2 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_joinItems x1 x2 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_joinItems x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_markdownText :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_C_markdownText x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x4 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '\n'#)) x3250 x3500) x1 x3250 x3500
          x5 = d_OP_markdownText_dot___hash_selFP2_hash_fstline x4 x3250 x3500
          x6 = d_OP_markdownText_dot___hash_selFP3_hash_remtxt x4 x3250 x3500
           in (d_C_markdownLine x5 (d_C_dropFirst x6 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_markdownText x1002 x3250 x3500) (d_C_markdownText x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_markdownText z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_markdownText x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_markdownText_dot___hash_selFP2_hash_fstline :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownText_dot___hash_selFP2_hash_fstline x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownText_dot___hash_selFP2_hash_fstline x1002 x3250 x3500) (d_OP_markdownText_dot___hash_selFP2_hash_fstline x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownText_dot___hash_selFP2_hash_fstline z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownText_dot___hash_selFP2_hash_fstline x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_markdownText_dot___hash_selFP3_hash_remtxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownText_dot___hash_selFP3_hash_remtxt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownText_dot___hash_selFP3_hash_remtxt x1002 x3250 x3500) (d_OP_markdownText_dot___hash_selFP3_hash_remtxt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownText_dot___hash_selFP3_hash_remtxt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownText_dot___hash_selFP3_hash_remtxt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_markdownLine :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_C_markdownLine x1 x2 x3250 x3500 = let
     x3 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '\n'#)) x3250 x3500) x2 x3250 x3500
     x4 = d_OP_markdownLine_dot___hash_selFP5_hash_sndline x3 x3250 x3500
     x5 = d_OP_markdownLine_dot___hash_selFP6_hash_furtherlines x3 x3250 x3500
     x6 = Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x4 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '='#)) x3250 x3500) x4 x3250 x3500) x3250 x3500
     x7 = Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x4 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '-'#)) x3250 x3500) x4 x3250 x3500) x3250 x3500
     x8 = d_C_isNumberedItemLine x1 x3250 x3500
     x9 = d_C_isUnorderedItemLine x1 x3250 x3500
     x10 = d_C_isCodeLine x1 x3250 x3500
      in (d_OP__case_81 x1 x6 x7 x10 x9 x8 x2 x5 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Char.d_C_isSpace x3250 x3500) x1 x3250 x3500) x3250 x3500)

d_OP_markdownLine_dot___hash_selFP5_hash_sndline :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownLine_dot___hash_selFP5_hash_sndline x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownLine_dot___hash_selFP5_hash_sndline x1002 x3250 x3500) (d_OP_markdownLine_dot___hash_selFP5_hash_sndline x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownLine_dot___hash_selFP5_hash_sndline z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownLine_dot___hash_selFP5_hash_sndline x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_markdownLine_dot___hash_selFP6_hash_furtherlines :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownLine_dot___hash_selFP6_hash_furtherlines x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownLine_dot___hash_selFP6_hash_furtherlines x1002 x3250 x3500) (d_OP_markdownLine_dot___hash_selFP6_hash_furtherlines x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownLine_dot___hash_selFP6_hash_furtherlines z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownLine_dot___hash_selFP6_hash_furtherlines x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_dropFirst :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_dropFirst x1 x3250 x3500 = d_OP__case_71 x1 (Curry_Prelude.d_C_null x1 x3250 x3500) x3250 x3500

d_C_tryMDHeader :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_C_tryMDHeader x1 x2 x3250 x3500 = let
     x3 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char ' '#)) x3250 x3500) x1 x3250 x3500
     x4 = d_OP_tryMDHeader_dot___hash_selFP8_hash_sharps x3 x3250 x3500
     x5 = d_OP_tryMDHeader_dot___hash_selFP9_hash_htxt x3 x3250 x3500
     x6 = Curry_Prelude.d_C_length x4 x3250 x3500
      in (d_OP__case_70 x6 x5 x2 x1 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_null x5 x3250 x3500) (Curry_Prelude.d_OP_gt x6 (Curry_Prelude.C_Int 6#) x3250 x3500) x3250 x3500) x3250 x3500)

d_OP_tryMDHeader_dot___hash_selFP8_hash_sharps :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_tryMDHeader_dot___hash_selFP8_hash_sharps x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tryMDHeader_dot___hash_selFP8_hash_sharps x1002 x3250 x3500) (d_OP_tryMDHeader_dot___hash_selFP8_hash_sharps x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tryMDHeader_dot___hash_selFP8_hash_sharps z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tryMDHeader_dot___hash_selFP8_hash_sharps x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_tryMDHeader_dot___hash_selFP9_hash_htxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_tryMDHeader_dot___hash_selFP9_hash_htxt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tryMDHeader_dot___hash_selFP9_hash_htxt x1002 x3250 x3500) (d_OP_tryMDHeader_dot___hash_selFP9_hash_htxt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tryMDHeader_dot___hash_selFP9_hash_htxt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tryMDHeader_dot___hash_selFP9_hash_htxt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isHRule :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isHRule x1 x3250 x3500 = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all d_OP_isHRule_dot___hash_lambda3 x3250 x3500) x1 x3250 x3500) (Curry_Prelude.d_OP_gt (Curry_Prelude.d_C_length (Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '-'#)) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.C_Int 3#) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all d_OP_isHRule_dot___hash_lambda4 x3250 x3500) x1 x3250 x3500) (Curry_Prelude.d_OP_gt (Curry_Prelude.d_C_length (Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '*'#)) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.C_Int 3#) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_isHRule_dot___hash_lambda3 :: Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isHRule_dot___hash_lambda3 x1 x3250 x3500 = Curry_Prelude.d_OP_bar_bar (Curry_Char.d_C_isSpace x1 x3250 x3500) (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '-'#) x3250 x3500) x3250 x3500

d_OP_isHRule_dot___hash_lambda4 :: Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isHRule_dot___hash_lambda4 x1 x3250 x3500 = Curry_Prelude.d_OP_bar_bar (Curry_Char.d_C_isSpace x1 x3250 x3500) (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '*'#) x3250 x3500) x3250 x3500

d_C_isUnorderedItemLine :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_isUnorderedItemLine x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_span (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char ' '#)) x1 x3250 x3500
     x3 = d_OP_isUnorderedItemLine_dot___hash_selFP11_hash_blanks x2 x3250 x3500
     x4 = d_OP_isUnorderedItemLine_dot___hash_selFP12_hash_nonblanks x2 x3250 x3500
      in (d_OP__case_69 x4 x3 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 2#) x4 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500)

d_OP_isUnorderedItemLine_dot___hash_selFP11_hash_blanks :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_isUnorderedItemLine_dot___hash_selFP11_hash_blanks x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isUnorderedItemLine_dot___hash_selFP11_hash_blanks x1002 x3250 x3500) (d_OP_isUnorderedItemLine_dot___hash_selFP11_hash_blanks x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isUnorderedItemLine_dot___hash_selFP11_hash_blanks z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isUnorderedItemLine_dot___hash_selFP11_hash_blanks x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_isUnorderedItemLine_dot___hash_selFP12_hash_nonblanks :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_isUnorderedItemLine_dot___hash_selFP12_hash_nonblanks x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isUnorderedItemLine_dot___hash_selFP12_hash_nonblanks x1002 x3250 x3500) (d_OP_isUnorderedItemLine_dot___hash_selFP12_hash_nonblanks x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isUnorderedItemLine_dot___hash_selFP12_hash_nonblanks z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isUnorderedItemLine_dot___hash_selFP12_hash_nonblanks x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isNumberedItemLine :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_isNumberedItemLine x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_span (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char ' '#)) x1 x3250 x3500
     x3 = d_OP_isNumberedItemLine_dot___hash_selFP20_hash_blanks x2 x3250 x3500
     x4 = d_OP_isNumberedItemLine_dot___hash_selFP21_hash_nonblanks x2 x3250 x3500
     x5 = Curry_Prelude.d_C_length x3 x3250 x3500
      in (d_OP_isNumberedItemLine_dot_checkNumber_dot_99 x5 x4 x3250 x3500)

d_OP_isNumberedItemLine_dot_checkNumber_dot_99 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_isNumberedItemLine_dot_checkNumber_dot_99 x1 x2 x3250 x3500 = let
     x3 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char ' '#)) x3250 x3500) x2 x3250 x3500
     x4 = d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP17_hash_ns x3 x3250 x3500
     x5 = d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP18_hash_brt x3 x3250 x3500
     x6 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Prelude.C_Char ' '#)) x3250 x3500) x5 x3250 x3500
     x7 = d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP15_hash_blanks x6 x3250 x3500
     x8 = d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP16_hash_rtxt x6 x3250 x3500
     x9 = Curry_Prelude.d_C_length x4 x3250 x3500
      in (d_OP__case_68 x8 x7 x9 x4 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_gt x9 (Curry_Prelude.C_Int 0#) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Char.d_C_isDigit x3250 x3500) (Curry_Prelude.d_C_take (Curry_Prelude.d_OP_minus x9 (Curry_Prelude.C_Int 1#) x3250 x3500) x4 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_OP_bang_bang x4 (Curry_Prelude.d_OP_minus x9 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) (Curry_Prelude.C_Char '.'#) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x7 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x8 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)

d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP17_hash_ns :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP17_hash_ns x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP17_hash_ns x1002 x3250 x3500) (d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP17_hash_ns x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP17_hash_ns z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP17_hash_ns x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP18_hash_brt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP18_hash_brt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP18_hash_brt x1002 x3250 x3500) (d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP18_hash_brt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP18_hash_brt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP18_hash_brt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP15_hash_blanks :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP15_hash_blanks x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP15_hash_blanks x1002 x3250 x3500) (d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP15_hash_blanks x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP15_hash_blanks z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP15_hash_blanks x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP16_hash_rtxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP16_hash_rtxt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP16_hash_rtxt x1002 x3250 x3500) (d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP16_hash_rtxt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP16_hash_rtxt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isNumberedItemLine_dot_checkNumber_dot_99_dot___hash_selFP16_hash_rtxt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_isNumberedItemLine_dot___hash_selFP20_hash_blanks :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_isNumberedItemLine_dot___hash_selFP20_hash_blanks x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isNumberedItemLine_dot___hash_selFP20_hash_blanks x1002 x3250 x3500) (d_OP_isNumberedItemLine_dot___hash_selFP20_hash_blanks x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isNumberedItemLine_dot___hash_selFP20_hash_blanks z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isNumberedItemLine_dot___hash_selFP20_hash_blanks x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_isNumberedItemLine_dot___hash_selFP21_hash_nonblanks :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_isNumberedItemLine_dot___hash_selFP21_hash_nonblanks x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isNumberedItemLine_dot___hash_selFP21_hash_nonblanks x1002 x3250 x3500) (d_OP_isNumberedItemLine_dot___hash_selFP21_hash_nonblanks x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isNumberedItemLine_dot___hash_selFP21_hash_nonblanks z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isNumberedItemLine_dot___hash_selFP21_hash_nonblanks x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isCodeLine :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_isCodeLine x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_span (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char ' '#)) x1 x3250 x3500
     x3 = d_OP_isCodeLine_dot___hash_selFP23_hash_blanks x2 x3250 x3500
     x4 = d_OP_isCodeLine_dot___hash_selFP24_hash_nonblanks x2 x3250 x3500
     x5 = Curry_Prelude.d_C_length x3 x3250 x3500
      in (d_OP__case_67 x5 x4 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x4 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_eq x5 (Curry_Prelude.C_Int 4#) x3250 x3500) x3250 x3500) x3250 x3500)

d_OP_isCodeLine_dot___hash_selFP23_hash_blanks :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_isCodeLine_dot___hash_selFP23_hash_blanks x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isCodeLine_dot___hash_selFP23_hash_blanks x1002 x3250 x3500) (d_OP_isCodeLine_dot___hash_selFP23_hash_blanks x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isCodeLine_dot___hash_selFP23_hash_blanks z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isCodeLine_dot___hash_selFP23_hash_blanks x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_isCodeLine_dot___hash_selFP24_hash_nonblanks :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_isCodeLine_dot___hash_selFP24_hash_nonblanks x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isCodeLine_dot___hash_selFP24_hash_nonblanks x1002 x3250 x3500) (d_OP_isCodeLine_dot___hash_selFP24_hash_nonblanks x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isCodeLine_dot___hash_selFP24_hash_nonblanks z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isCodeLine_dot___hash_selFP24_hash_nonblanks x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_markdownPar :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_C_markdownPar x1 x2 x3250 x3500 = let
     x3 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '\n'#)) x3250 x3500) x2 x3250 x3500
     x4 = d_OP_markdownPar_dot___hash_selFP26_hash_fstline x3 x3250 x3500
     x5 = d_OP_markdownPar_dot___hash_selFP27_hash_remtxt x3 x3250 x3500
     x6 = d_C_isNumberedItemLine x4 x3250 x3500
     x7 = d_C_isUnorderedItemLine x4 x3250 x3500
      in (d_OP__case_66 x6 x7 x2 x5 x4 x1 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_null x2 x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem (Curry_Prelude.d_C_head x2 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_gt x7 (Curry_Prelude.C_Int 0#) x3250 x3500) (Curry_Prelude.d_OP_gt x6 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)

d_OP_markdownPar_dot___hash_selFP26_hash_fstline :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownPar_dot___hash_selFP26_hash_fstline x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownPar_dot___hash_selFP26_hash_fstline x1002 x3250 x3500) (d_OP_markdownPar_dot___hash_selFP26_hash_fstline x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownPar_dot___hash_selFP26_hash_fstline z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownPar_dot___hash_selFP26_hash_fstline x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_markdownPar_dot___hash_selFP27_hash_remtxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownPar_dot___hash_selFP27_hash_remtxt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownPar_dot___hash_selFP27_hash_remtxt x1002 x3250 x3500) (d_OP_markdownPar_dot___hash_selFP27_hash_remtxt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownPar_dot___hash_selFP27_hash_remtxt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownPar_dot___hash_selFP27_hash_remtxt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_markdownQuote :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_C_markdownQuote x1 x2 x3250 x3500 = let
     x3 = d_OP__case_61 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 2#) x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500
      in (d_OP__case_63 x3 x1 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 2#) x3 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500)

d_OP_markdownQuote_dot___hash_selFP29_hash_fstline :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownQuote_dot___hash_selFP29_hash_fstline x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownQuote_dot___hash_selFP29_hash_fstline x1002 x3250 x3500) (d_OP_markdownQuote_dot___hash_selFP29_hash_fstline x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownQuote_dot___hash_selFP29_hash_fstline z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownQuote_dot___hash_selFP29_hash_fstline x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_markdownQuote_dot___hash_selFP30_hash_remtxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownQuote_dot___hash_selFP30_hash_remtxt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownQuote_dot___hash_selFP30_hash_remtxt x1002 x3250 x3500) (d_OP_markdownQuote_dot___hash_selFP30_hash_remtxt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownQuote_dot___hash_selFP30_hash_remtxt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownQuote_dot___hash_selFP30_hash_remtxt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_markdownCodeBlock :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_C_markdownCodeBlock x1 x2 x3 x3250 x3500 = d_OP__case_60 x3 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take x1 x3 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500

d_OP_markdownCodeBlock_dot___hash_selFP32_hash_fstline :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownCodeBlock_dot___hash_selFP32_hash_fstline x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownCodeBlock_dot___hash_selFP32_hash_fstline x1002 x3250 x3500) (d_OP_markdownCodeBlock_dot___hash_selFP32_hash_fstline x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownCodeBlock_dot___hash_selFP32_hash_fstline z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownCodeBlock_dot___hash_selFP32_hash_fstline x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_markdownCodeBlock_dot___hash_selFP33_hash_remtxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownCodeBlock_dot___hash_selFP33_hash_remtxt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownCodeBlock_dot___hash_selFP33_hash_remtxt x1002 x3250 x3500) (d_OP_markdownCodeBlock_dot___hash_selFP33_hash_remtxt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownCodeBlock_dot___hash_selFP33_hash_remtxt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownCodeBlock_dot___hash_selFP33_hash_remtxt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_markdownItem :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_SourceMDElem) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_C_markdownItem x1 x2 x3 x4 x3250 x3500 = d_OP__case_58 x2 x4 x3 x1 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take x2 x4 x3250 x3500) (Curry_Prelude.d_C_take x2 (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ' '#) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

nd_C_markdownItem :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_SourceMDElem -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
nd_C_markdownItem x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_58 x2 x4 x3 x1 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take x2 x4 x3250 x3500) (Curry_Prelude.d_C_take x2 (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ' '#) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))

d_OP_markdownItem_dot___hash_selFP35_hash_fstline :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownItem_dot___hash_selFP35_hash_fstline x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownItem_dot___hash_selFP35_hash_fstline x1002 x3250 x3500) (d_OP_markdownItem_dot___hash_selFP35_hash_fstline x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownItem_dot___hash_selFP35_hash_fstline z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownItem_dot___hash_selFP35_hash_fstline x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_markdownItem_dot___hash_selFP36_hash_remtxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownItem_dot___hash_selFP36_hash_remtxt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownItem_dot___hash_selFP36_hash_remtxt x1002 x3250 x3500) (d_OP_markdownItem_dot___hash_selFP36_hash_remtxt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownItem_dot___hash_selFP36_hash_remtxt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownItem_dot___hash_selFP36_hash_remtxt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_markdownItem_dot___hash_selFP38_hash_fstline :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownItem_dot___hash_selFP38_hash_fstline x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownItem_dot___hash_selFP38_hash_fstline x1002 x3250 x3500) (d_OP_markdownItem_dot___hash_selFP38_hash_fstline x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownItem_dot___hash_selFP38_hash_fstline z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownItem_dot___hash_selFP38_hash_fstline x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_markdownItem_dot___hash_selFP39_hash_remtxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownItem_dot___hash_selFP39_hash_remtxt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownItem_dot___hash_selFP39_hash_remtxt x1002 x3250 x3500) (d_OP_markdownItem_dot___hash_selFP39_hash_remtxt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownItem_dot___hash_selFP39_hash_remtxt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownItem_dot___hash_selFP39_hash_remtxt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_removeEscapes :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_removeEscapes x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x4 = x2
           in (d_OP__case_54 x4 x3 x2 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Char '\\'#) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_removeEscapes x1002 x3250 x3500) (d_C_removeEscapes x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_removeEscapes z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_removeEscapes x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_markdownEscapeChars :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_markdownEscapeChars x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))))))

d_C_outsideMarkdownElem :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_C_outsideMarkdownElem x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_C_addPrevious x1 Curry_Prelude.OP_List x3250 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = x3
           in (d_OP__case_51 x5 x4 x1 x3 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '\\'#) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_outsideMarkdownElem x1 x1002 x3250 x3500) (d_C_outsideMarkdownElem x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_outsideMarkdownElem x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_outsideMarkdownElem x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_addPrevious :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_SourceMDElem -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_C_addPrevious x1 x2 x3250 x3500 = d_OP__case_38 x1 x2 (Curry_Prelude.d_C_null x1 x3250 x3500) x3250 x3500

d_C_tryParseLink :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_C_tryParseLink x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char ']'#)) x3250 x3500) x1 x3250 x3500
     x3 = d_OP_tryParseLink_dot___hash_selFP44_hash_linktxt x2 x3250 x3500
     x4 = d_OP_tryParseLink_dot___hash_selFP45_hash_rtxt x2 x3250 x3500
      in (d_OP__case_37 x4 x3 x1 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_null x4 x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_null (Curry_Prelude.d_C_tail x4 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_OP_bang_bang x4 (Curry_Prelude.C_Int 1#) x3250 x3500) (Curry_Prelude.C_Char '('#) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)

d_OP_tryParseLink_dot___hash_selFP44_hash_linktxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_tryParseLink_dot___hash_selFP44_hash_linktxt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tryParseLink_dot___hash_selFP44_hash_linktxt x1002 x3250 x3500) (d_OP_tryParseLink_dot___hash_selFP44_hash_linktxt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tryParseLink_dot___hash_selFP44_hash_linktxt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tryParseLink_dot___hash_selFP44_hash_linktxt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_tryParseLink_dot___hash_selFP45_hash_rtxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_tryParseLink_dot___hash_selFP45_hash_rtxt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tryParseLink_dot___hash_selFP45_hash_rtxt x1002 x3250 x3500) (d_OP_tryParseLink_dot___hash_selFP45_hash_rtxt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tryParseLink_dot___hash_selFP45_hash_rtxt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tryParseLink_dot___hash_selFP45_hash_rtxt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_tryParseLink_dot___hash_selFP42_hash_url :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_tryParseLink_dot___hash_selFP42_hash_url x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tryParseLink_dot___hash_selFP42_hash_url x1002 x3250 x3500) (d_OP_tryParseLink_dot___hash_selFP42_hash_url x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tryParseLink_dot___hash_selFP42_hash_url z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tryParseLink_dot___hash_selFP42_hash_url x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_tryParseLink_dot___hash_selFP43_hash_mtxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_tryParseLink_dot___hash_selFP43_hash_mtxt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tryParseLink_dot___hash_selFP43_hash_mtxt x1002 x3250 x3500) (d_OP_tryParseLink_dot___hash_selFP43_hash_mtxt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tryParseLink_dot___hash_selFP43_hash_mtxt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tryParseLink_dot___hash_selFP43_hash_mtxt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_markdownHRef :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_C_markdownHRef x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '>'#)) x3250 x3500) x1 x3250 x3500
     x3 = d_OP_markdownHRef_dot___hash_selFP47_hash_url x2 x3250 x3500
     x4 = d_OP_markdownHRef_dot___hash_selFP48_hash_rtxt x2 x3250 x3500
      in (d_OP__case_35 x4 x3 x1 (Curry_Prelude.d_C_null x4 x3250 x3500) x3250 x3500)

d_OP_markdownHRef_dot___hash_selFP47_hash_url :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownHRef_dot___hash_selFP47_hash_url x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownHRef_dot___hash_selFP47_hash_url x1002 x3250 x3500) (d_OP_markdownHRef_dot___hash_selFP47_hash_url x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownHRef_dot___hash_selFP47_hash_url z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownHRef_dot___hash_selFP47_hash_url x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_markdownHRef_dot___hash_selFP48_hash_rtxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_markdownHRef_dot___hash_selFP48_hash_rtxt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_markdownHRef_dot___hash_selFP48_hash_rtxt x1002 x3250 x3500) (d_OP_markdownHRef_dot___hash_selFP48_hash_rtxt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_markdownHRef_dot___hash_selFP48_hash_rtxt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_markdownHRef_dot___hash_selFP48_hash_rtxt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_insideMarkdownElem :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_C_insideMarkdownElem x1 x2 x3 x3250 x3500 = d_OP__case_34 x3 x1 x2 (Curry_List.d_C_isPrefixOf x1 x3 x3250 x3500) x3250 x3500

d_C_text2MDElem :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_SourceMDElem
d_C_text2MDElem x1 x2 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = x3
           in (d_OP__case_29 x5 x1 x4 x2 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '*'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_text2MDElem x1002 x2 x3250 x3500) (d_C_text2MDElem x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_text2MDElem z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_text2MDElem x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_mdDoc2html :: Cover -> ConstStore -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_C_mdDoc2html x3250 x3500 = Curry_Prelude.d_C_map d_C_mdElem2html

nd_C_mdDoc2html :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List C_MarkdownElem) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
nd_C_mdDoc2html x3000 x3250 x3500 = wrapNX id (Curry_Prelude.nd_C_map (wrapNX id nd_C_mdElem2html))

d_C_mdtxt2html :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_HTML.C_HtmlExp
d_C_mdtxt2html x1 x3250 x3500 = Curry_HTML.C_HtmlText (d_C_removeEscapes x1 x3250 x3500)

nd_C_mdtxt2html :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_HTML.C_HtmlExp
nd_C_mdtxt2html x1 x3000 x3250 x3500 = Curry_HTML.C_HtmlText (d_C_removeEscapes x1 x3250 x3500)

d_C_mdElem2html :: C_MarkdownElem -> Cover -> ConstStore -> Curry_HTML.C_HtmlExp
d_C_mdElem2html x1 x3250 x3500 = case x1 of
     (C_Text x2) -> d_C_mdtxt2html x2 x3250 x3500
     (C_Emph x3) -> Curry_HTML.d_C_emphasize (Curry_Prelude.OP_Cons (d_C_mdtxt2html x3 x3250 x3500) Curry_Prelude.OP_List) x3250 x3500
     (C_Strong x4) -> Curry_HTML.C_HtmlStruct (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (d_C_mdtxt2html x4 x3250 x3500) Curry_Prelude.OP_List)
     (C_HRef x5 x6) -> d_OP__case_19 x6 x5 (Curry_Prelude.d_OP_eq_eq x5 x6 x3250 x3500) x3250 x3500
     (C_Code x7) -> Curry_HTML.d_C_code (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlText x7) Curry_Prelude.OP_List) x3250 x3500
     (C_CodeBlock x8) -> Curry_HTML.d_C_verbatim x8 x3250 x3500
     (C_Quote x9) -> Curry_HTML.C_HtmlStruct (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))) Curry_Prelude.OP_List (Curry_Prelude.d_C_apply (d_C_mdDoc2html x3250 x3500) x9 x3250 x3500)
     (C_Par x10) -> Curry_HTML.d_C_par (Curry_Prelude.d_C_apply (d_C_mdDoc2html x3250 x3500) x10 x3250 x3500) x3250 x3500
     (C_UList x11) -> Curry_HTML.d_C_ulist (Curry_Prelude.d_C_map d_C_mdDoc2htmlWithoutPar x11 x3250 x3500) x3250 x3500
     (C_OList x12) -> Curry_HTML.d_C_olist (Curry_Prelude.d_C_map d_C_mdDoc2htmlWithoutPar x12 x3250 x3500) x3250 x3500
     C_HRule -> Curry_HTML.d_C_hrule x3250 x3500
     (C_Header x13 x14) -> Curry_HTML.C_HtmlStruct (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.d_C_show x13 x3250 x3500)) Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (d_C_mdtxt2html x14 x3250 x3500) Curry_Prelude.OP_List)
     (Choice_C_MarkdownElem x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mdElem2html x1002 x3250 x3500) (d_C_mdElem2html x1003 x3250 x3500)
     (Choices_C_MarkdownElem x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mdElem2html z x3250 x3500) x1002
     (Guard_C_MarkdownElem x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mdElem2html x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_MarkdownElem x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_mdElem2html :: C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_HTML.C_HtmlExp
nd_C_mdElem2html x1 x3000 x3250 x3500 = case x1 of
     (C_Text x2) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_mdtxt2html x2 x2000 x3250 x3500))
     (C_Emph x3) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_HTML.nd_C_emphasize (Curry_Prelude.OP_Cons (nd_C_mdtxt2html x3 x2000 x3250 x3500) Curry_Prelude.OP_List) x2001 x3250 x3500)))))
     (C_Strong x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_HTML.C_HtmlStruct (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (nd_C_mdtxt2html x4 x2000 x3250 x3500) Curry_Prelude.OP_List)))
     (C_HRef x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_19 x6 x5 (Curry_Prelude.d_OP_eq_eq x5 x6 x3250 x3500) x2000 x3250 x3500))
     (C_Code x7) -> let
          x2000 = x3000
           in (seq x2000 (Curry_HTML.nd_C_code (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlText x7) Curry_Prelude.OP_List) x2000 x3250 x3500))
     (C_CodeBlock x8) -> let
          x2000 = x3000
           in (seq x2000 (Curry_HTML.nd_C_verbatim x8 x2000 x3250 x3500))
     (C_Quote x9) -> let
          x2002 = x3000
           in (seq x2002 (Curry_HTML.C_HtmlStruct (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))) Curry_Prelude.OP_List (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_mdDoc2html x2000 x3250 x3500) x9 x2001 x3250 x3500))))))
     (C_Par x10) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_HTML.nd_C_par (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_mdDoc2html x2000 x3250 x3500) x10 x2001 x3250 x3500)))) x2003 x3250 x3500)))))
     (C_UList x11) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_HTML.nd_C_ulist (Curry_Prelude.nd_C_map (wrapNX id nd_C_mdDoc2htmlWithoutPar) x11 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (C_OList x12) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_HTML.nd_C_olist (Curry_Prelude.nd_C_map (wrapNX id nd_C_mdDoc2htmlWithoutPar) x12 x2000 x3250 x3500) x2001 x3250 x3500)))))
     C_HRule -> let
          x2000 = x3000
           in (seq x2000 (Curry_HTML.nd_C_hrule x2000 x3250 x3500))
     (C_Header x13 x14) -> let
          x2000 = x3000
           in (seq x2000 (Curry_HTML.C_HtmlStruct (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.d_C_show x13 x3250 x3500)) Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (nd_C_mdtxt2html x14 x2000 x3250 x3500) Curry_Prelude.OP_List)))
     (Choice_C_MarkdownElem x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_mdElem2html x1002 x3000 x3250 x3500) (nd_C_mdElem2html x1003 x3000 x3250 x3500)
     (Choices_C_MarkdownElem x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_mdElem2html z x3000 x3250 x3500) x1002
     (Guard_C_MarkdownElem x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_mdElem2html x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_MarkdownElem x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_mdDoc2htmlWithoutPar :: Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_C_mdDoc2htmlWithoutPar x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_18 x3 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mdDoc2htmlWithoutPar x1002 x3250 x3500) (d_C_mdDoc2htmlWithoutPar x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mdDoc2htmlWithoutPar z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mdDoc2htmlWithoutPar x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_mdDoc2htmlWithoutPar :: Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_C_mdDoc2htmlWithoutPar x1 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_18 x3 x2 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_mdDoc2htmlWithoutPar x1002 x3000 x3250 x3500) (nd_C_mdDoc2htmlWithoutPar x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_mdDoc2htmlWithoutPar z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_mdDoc2htmlWithoutPar x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_markdownText2HTML :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_C_markdownText2HTML x3250 x3500 = Curry_Prelude.d_OP_dot (d_C_mdDoc2html x3250 x3500) (d_C_fromMarkdownText x3250 x3500) x3250 x3500

nd_C_markdownText2HTML :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
nd_C_markdownText2HTML x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_dot (nd_C_mdDoc2html x2000 x3250 x3500) (nd_C_fromMarkdownText x2001 x3250 x3500) x2002 x3250 x3500))))))))

d_C_markdownText2CompleteHTML :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_markdownText2CompleteHTML x1 x2 x3250 x3500 = Curry_HTML.d_C_showHtmlPage (Curry_HTML.d_C_page x1 (Curry_Prelude.d_C_apply (d_C_markdownText2HTML x3250 x3500) x2 x3250 x3500) x3250 x3500) x3250 x3500

d_C_mdDoc2latex :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_mdDoc2latex x1 x3250 x3500 = Curry_Prelude.d_C_concatMap (d_C_mdElem2latex x1) x3250 x3500

nd_C_mdDoc2latex :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List C_MarkdownElem) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_mdDoc2latex x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_concatMap (wrapNX id (nd_C_mdElem2latex x1)) x2000 x3250 x3500))

d_C_mdElem2latex :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_mdElem2latex x1 x2 x3250 x3500 = case x2 of
     (C_Text x3) -> Curry_Prelude.d_C_apply x1 x3 x3250 x3500
     (C_Emph x4) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     (C_Strong x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x5 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     (C_HRef x6 x7) -> d_OP__case_5 x7 x6 x1 (Curry_Prelude.d_OP_eq_eq x6 x7 x3250 x3500) x3250 x3500
     (C_Code x8) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x8 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     (C_CodeBlock x9) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x9 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))) x3250 x3500) x3250 x3500
     (C_Quote x10) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_mdDoc2latex x1 x3250 x3500) x10 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))) x3250 x3500) x3250 x3500
     (C_Par x11) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_mdDoc2latex x1 x3250 x3500) x11 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3250 x3500
     (C_UList x12) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_OP_mdElem2latex_dot___hash_lambda10 x1) x3250 x3500) x12 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))) x3250 x3500) x3250 x3500
     (C_OList x13) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_OP_mdElem2latex_dot___hash_lambda11 x1) x3250 x3500) x13 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))) x3250 x3500) x3250 x3500
     C_HRule -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '3'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '4'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))
     (C_Header x14 x15) -> let
          x16 = x14
           in (d_OP__case_4 x16 x15 x1 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500)
     (Choice_C_MarkdownElem x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mdElem2latex x1 x1002 x3250 x3500) (d_C_mdElem2latex x1 x1003 x3250 x3500)
     (Choices_C_MarkdownElem x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mdElem2latex x1 z x3250 x3500) x1002
     (Guard_C_MarkdownElem x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mdElem2latex x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_MarkdownElem x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_mdElem2latex :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_mdElem2latex x1 x2 x3000 x3250 x3500 = case x2 of
     (C_Text x3) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500))
     (C_Emph x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x4 x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500))
     (C_Strong x5) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x5 x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500))
     (C_HRef x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x7 x6 x1 (Curry_Prelude.d_OP_eq_eq x6 x7 x3250 x3500) x2000 x3250 x3500))
     (C_Code x8) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x8 x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500))
     (C_CodeBlock x9) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x9 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))) x3250 x3500) x3250 x3500
     (C_Quote x10) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))) (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_mdDoc2latex x1 x2000 x3250 x3500) x10 x2001 x3250 x3500)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))) x3250 x3500) x3250 x3500))
     (C_Par x11) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_mdDoc2latex x1 x2000 x3250 x3500) x11 x2001 x3250 x3500)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3250 x3500))
     (C_UList x12) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapNX id (nd_OP_mdElem2latex_dot___hash_lambda10 x1)) x2000 x3250 x3500) x12 x2001 x3250 x3500)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))) x3250 x3500) x3250 x3500))
     (C_OList x13) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapNX id (nd_OP_mdElem2latex_dot___hash_lambda11 x1)) x2000 x3250 x3500) x13 x2001 x3250 x3500)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))) x3250 x3500) x3250 x3500))
     C_HRule -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '3'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '4'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))
     (C_Header x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (let
               x16 = x14
                in (nd_OP__case_4 x16 x15 x1 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Int 1#) x3250 x3500) x2000 x3250 x3500)))
     (Choice_C_MarkdownElem x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_mdElem2latex x1 x1002 x3000 x3250 x3500) (nd_C_mdElem2latex x1 x1003 x3000 x3250 x3500)
     (Choices_C_MarkdownElem x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_mdElem2latex x1 z x3000 x3250 x3500) x1002
     (Guard_C_MarkdownElem x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_mdElem2latex x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_MarkdownElem x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_mdElem2latex_dot___hash_lambda10 :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_mdElem2latex_dot___hash_lambda10 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_C_apply (d_C_mdDoc2latex x1 x3250 x3500) x2 x3250 x3500) x3250 x3500

nd_OP_mdElem2latex_dot___hash_lambda10 :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_mdElem2latex_dot___hash_lambda10 x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))) (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_mdDoc2latex x1 x2000 x3250 x3500) x2 x2001 x3250 x3500)))) x3250 x3500))

d_OP_mdElem2latex_dot___hash_lambda11 :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_mdElem2latex_dot___hash_lambda11 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_C_apply (d_C_mdDoc2latex x1 x3250 x3500) x2 x3250 x3500) x3250 x3500

nd_OP_mdElem2latex_dot___hash_lambda11 :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_mdElem2latex_dot___hash_lambda11 x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))) (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_mdDoc2latex x1 x2000 x3250 x3500) x2 x2001 x3250 x3500)))) x3250 x3500))

d_C_html2latex :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_html2latex x3250 x3500 = Curry_Prelude.d_OP_dot Curry_HTML.d_C_showLatexExps (Curry_Prelude.d_OP_dot Curry_HtmlParser.d_C_parseHtmlString d_C_removeEscapes x3250 x3500) x3250 x3500

nd_C_html2latex :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_html2latex x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id Curry_HTML.nd_C_showLatexExps) (Curry_Prelude.nd_OP_dot (wrapNX id Curry_HtmlParser.nd_C_parseHtmlString) (wrapDX id d_C_removeEscapes) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_markdownText2LaTeX :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_markdownText2LaTeX x3250 x3500 = Curry_Prelude.d_OP_dot (d_C_mdDoc2latex (d_C_html2latex x3250 x3500) x3250 x3500) (d_C_fromMarkdownText x3250 x3500) x3250 x3500

nd_C_markdownText2LaTeX :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_markdownText2LaTeX x3000 x3250 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2004 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2004 (seq x2006 (let
               x2002 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2002 (seq x2003 (Curry_Prelude.nd_OP_dot (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_mdDoc2latex (nd_C_html2latex x2000 x3250 x3500) x2001 x3250 x3500)))) (nd_C_fromMarkdownText x2003 x3250 x3500) x2004 x3250 x3500))))))))

d_C_markdownText2LaTeXWithFormat :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_markdownText2LaTeXWithFormat x1 x3250 x3500 = Curry_Prelude.d_OP_dot (d_C_mdDoc2latex x1 x3250 x3500) (d_C_fromMarkdownText x3250 x3500) x3250 x3500

nd_C_markdownText2LaTeXWithFormat :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_markdownText2LaTeXWithFormat x1 x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_dot (nd_C_mdDoc2latex x1 x2000 x3250 x3500) (nd_C_fromMarkdownText x2001 x3250 x3500) x2002 x3250 x3500))))))))

d_C_markdownText2CompleteLaTeX :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_markdownText2CompleteLaTeX x1 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (d_C_latexHeader x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_mdDoc2latex (d_C_html2latex x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (d_C_fromMarkdownText x3250 x3500) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))) x3250 x3500) x3250 x3500

d_C_latexHeader :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_latexHeader x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '8'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '6'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_C_formatMarkdownInputAsPDF :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_formatMarkdownInputAsPDF x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_getContents x3250 x3500) d_C_formatMarkdownAsPDF x3250 x3500

d_C_formatMarkdownFileAsPDF :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_formatMarkdownFileAsPDF x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3250 x3500) d_C_formatMarkdownAsPDF x3250 x3500

d_C_formatMarkdownAsPDF :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_formatMarkdownAsPDF x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_System.d_C_getPID x3250 x3500) (d_OP_formatMarkdownAsPDF_dot___hash_lambda13 x1) x3250 x3500

d_OP_formatMarkdownAsPDF_dot___hash_lambda13 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_formatMarkdownAsPDF_dot___hash_lambda13 x1 x2 x3250 x3500 = let
     x3 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_writeFile (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List)))) x3250 x3500) (d_C_markdownText2CompleteLaTeX x1 x3250 x3500) x3250 x3500) (d_C_pdflatexFile x3 x3250 x3500) x3250 x3500)

d_C_pdflatexFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_pdflatexFile x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_done x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_OP__case_4 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_4 x16 x15 x1 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x15 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_3 x16 x15 x1 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x16 x15 x1 x1002 x3250 x3500) (d_OP__case_4 x16 x15 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x16 x15 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x16 x15 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_4 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_4 x16 x15 x1 x17 x3000 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x15 x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x16 x15 x1 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Int 2#) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x16 x15 x1 x1002 x3000 x3250 x3500) (nd_OP__case_4 x16 x15 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x16 x15 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x16 x15 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_3 x16 x15 x1 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x15 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_2 x16 x15 x1 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Int 3#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x16 x15 x1 x1002 x3250 x3500) (d_OP__case_3 x16 x15 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x16 x15 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x16 x15 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_3 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_3 x16 x15 x1 x17 x3000 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x15 x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x16 x15 x1 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Int 3#) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x16 x15 x1 x1002 x3000 x3250 x3500) (nd_OP__case_3 x16 x15 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x16 x15 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x16 x15 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_2 x16 x15 x1 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x15 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_1 x16 x15 x1 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Int 4#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x16 x15 x1 x1002 x3250 x3500) (d_OP__case_2 x16 x15 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x16 x15 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x16 x15 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_2 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_2 x16 x15 x1 x17 x3000 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x15 x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x16 x15 x1 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Int 4#) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x16 x15 x1 x1002 x3000 x3250 x3500) (nd_OP__case_2 x16 x15 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x16 x15 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x16 x15 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_1 x16 x15 x1 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x15 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_0 x16 x15 x1 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Int 5#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x16 x15 x1 x1002 x3250 x3500) (d_OP__case_1 x16 x15 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x16 x15 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x16 x15 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_1 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_1 x16 x15 x1 x17 x3000 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x15 x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x16 x15 x1 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Int 5#) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x16 x15 x1 x1002 x3000 x3250 x3500) (nd_OP__case_1 x16 x15 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x16 x15 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x16 x15 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x16 x15 x1 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x15 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x15 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x16 x15 x1 x1002 x3250 x3500) (d_OP__case_0 x16 x15 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x16 x15 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x16 x15 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_0 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_0 x16 x15 x1 x17 x3000 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x15 x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x15 x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x16 x15 x1 x1002 x3000 x3250 x3500) (nd_OP__case_0 x16 x15 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x16 x15 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x16 x15 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_5 x7 x6 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x6 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x7 x6 x1 x1002 x3250 x3500) (d_OP__case_5 x7 x6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x7 x6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x7 x6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_5 x7 x6 x1 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x6 x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x7 x6 x1 x1002 x3000 x3250 x3500) (nd_OP__case_5 x7 x6 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x7 x6 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x7 x6 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.OP_List C_MarkdownElem -> C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_18 x3 x2 x3250 x3500 = case x2 of
     (C_Par x4) -> d_OP__case_17 x4 x3 x3250 x3500
     (C_Text x7) -> d_OP__case_16 x7 x3 x3250 x3500
     (C_Emph x10) -> d_OP__case_15 x10 x3 x3250 x3500
     (C_Strong x13) -> d_OP__case_14 x13 x3 x3250 x3500
     (C_Code x16) -> d_OP__case_13 x16 x3 x3250 x3500
     (C_HRef x19 x20) -> d_OP__case_12 x20 x19 x3 x3250 x3500
     (C_CodeBlock x23) -> d_OP__case_11 x23 x3 x3250 x3500
     (C_UList x26) -> d_OP__case_10 x26 x3 x3250 x3500
     (C_OList x29) -> d_OP__case_9 x29 x3 x3250 x3500
     (C_Quote x32) -> d_OP__case_8 x32 x3 x3250 x3500
     C_HRule -> d_OP__case_7 x3 x3250 x3500
     (C_Header x37 x38) -> d_OP__case_6 x38 x37 x3 x3250 x3500
     (Choice_C_MarkdownElem x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x3 x1002 x3250 x3500) (d_OP__case_18 x3 x1003 x3250 x3500)
     (Choices_C_MarkdownElem x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x3 z x3250 x3500) x1002
     (Guard_C_MarkdownElem x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_MarkdownElem x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_18 :: Curry_Prelude.OP_List C_MarkdownElem -> C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_18 x3 x2 x3000 x3250 x3500 = case x2 of
     (C_Par x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_17 x4 x3 x2000 x3250 x3500))
     (C_Text x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x7 x3 x2000 x3250 x3500))
     (C_Emph x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_15 x10 x3 x2000 x3250 x3500))
     (C_Strong x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x13 x3 x2000 x3250 x3500))
     (C_Code x16) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_13 x16 x3 x2000 x3250 x3500))
     (C_HRef x19 x20) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x20 x19 x3 x2000 x3250 x3500))
     (C_CodeBlock x23) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_11 x23 x3 x2000 x3250 x3500))
     (C_UList x26) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x26 x3 x2000 x3250 x3500))
     (C_OList x29) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x29 x3 x2000 x3250 x3500))
     (C_Quote x32) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x32 x3 x2000 x3250 x3500))
     C_HRule -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x3 x2000 x3250 x3500))
     (C_Header x37 x38) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x38 x37 x3 x2000 x3250 x3500))
     (Choice_C_MarkdownElem x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x3 x1002 x3000 x3250 x3500) (nd_OP__case_18 x3 x1003 x3000 x3250 x3500)
     (Choices_C_MarkdownElem x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x3 z x3000 x3250 x3500) x1002
     (Guard_C_MarkdownElem x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_MarkdownElem x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_6 x38 x37 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_Header x37 x38) x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x39 x40) -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_Header x37 x38) x3250 x3500) (d_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x39 x40) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x38 x37 x1002 x3250 x3500) (d_OP__case_6 x38 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x38 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x38 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_6 x38 x37 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_Header x37 x38) x2000 x3250 x3500) Curry_Prelude.OP_List))
     (Curry_Prelude.OP_Cons x39 x40) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_Header x37 x38) x2000 x3250 x3500) (nd_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x39 x40) x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x38 x37 x1002 x3000 x3250 x3500) (nd_OP__case_6 x38 x37 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x38 x37 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x38 x37 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_7 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_mdElem2html C_HRule x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x35 x36) -> Curry_Prelude.OP_Cons (d_C_mdElem2html C_HRule x3250 x3500) (d_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x35 x36) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1002 x3250 x3500) (d_OP__case_7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_7 :: Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_7 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (nd_C_mdElem2html C_HRule x2000 x3250 x3500) Curry_Prelude.OP_List))
     (Curry_Prelude.OP_Cons x35 x36) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_mdElem2html C_HRule x2000 x3250 x3500) (nd_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x35 x36) x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1002 x3000 x3250 x3500) (nd_OP__case_7 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.OP_List C_MarkdownElem -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_8 x32 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_Quote x32) x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x33 x34) -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_Quote x32) x3250 x3500) (d_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x33 x34) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x32 x1002 x3250 x3500) (d_OP__case_8 x32 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x32 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x32 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_8 :: Curry_Prelude.OP_List C_MarkdownElem -> Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_8 x32 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_Quote x32) x2000 x3250 x3500) Curry_Prelude.OP_List))
     (Curry_Prelude.OP_Cons x33 x34) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_Quote x32) x2000 x3250 x3500) (nd_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x33 x34) x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x32 x1002 x3000 x3250 x3500) (nd_OP__case_8 x32 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x32 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x32 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List C_MarkdownElem) -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_9 x29 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_OList x29) x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x30 x31) -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_OList x29) x3250 x3500) (d_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x30 x31) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x29 x1002 x3250 x3500) (d_OP__case_9 x29 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x29 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x29 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_9 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List C_MarkdownElem) -> Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_9 x29 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_OList x29) x2000 x3250 x3500) Curry_Prelude.OP_List))
     (Curry_Prelude.OP_Cons x30 x31) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_OList x29) x2000 x3250 x3500) (nd_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x30 x31) x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x29 x1002 x3000 x3250 x3500) (nd_OP__case_9 x29 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x29 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x29 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List C_MarkdownElem) -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_10 x26 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_UList x26) x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x27 x28) -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_UList x26) x3250 x3500) (d_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x27 x28) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x26 x1002 x3250 x3500) (d_OP__case_10 x26 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x26 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x26 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_10 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List C_MarkdownElem) -> Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_10 x26 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_UList x26) x2000 x3250 x3500) Curry_Prelude.OP_List))
     (Curry_Prelude.OP_Cons x27 x28) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_UList x26) x2000 x3250 x3500) (nd_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x27 x28) x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x26 x1002 x3000 x3250 x3500) (nd_OP__case_10 x26 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x26 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x26 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_11 x23 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_CodeBlock x23) x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x24 x25) -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_CodeBlock x23) x3250 x3500) (d_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x24 x25) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x23 x1002 x3250 x3500) (d_OP__case_11 x23 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x23 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x23 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_11 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_11 x23 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_CodeBlock x23) x2000 x3250 x3500) Curry_Prelude.OP_List))
     (Curry_Prelude.OP_Cons x24 x25) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_CodeBlock x23) x2000 x3250 x3500) (nd_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x24 x25) x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x23 x1002 x3000 x3250 x3500) (nd_OP__case_11 x23 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x23 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x23 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_12 x20 x19 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_HRef x19 x20) x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x21 x22) -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_HRef x19 x20) x3250 x3500) (d_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x21 x22) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x20 x19 x1002 x3250 x3500) (d_OP__case_12 x20 x19 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x20 x19 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x20 x19 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_12 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_12 x20 x19 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_HRef x19 x20) x2000 x3250 x3500) Curry_Prelude.OP_List))
     (Curry_Prelude.OP_Cons x21 x22) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_HRef x19 x20) x2000 x3250 x3500) (nd_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x21 x22) x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x20 x19 x1002 x3000 x3250 x3500) (nd_OP__case_12 x20 x19 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x20 x19 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x20 x19 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_13 x16 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_Code x16) x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x17 x18) -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_Code x16) x3250 x3500) (d_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x17 x18) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x16 x1002 x3250 x3500) (d_OP__case_13 x16 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x16 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x16 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_13 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_13 x16 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_Code x16) x2000 x3250 x3500) Curry_Prelude.OP_List))
     (Curry_Prelude.OP_Cons x17 x18) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_Code x16) x2000 x3250 x3500) (nd_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x17 x18) x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x16 x1002 x3000 x3250 x3500) (nd_OP__case_13 x16 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x16 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x16 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_14 x13 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_Strong x13) x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x14 x15) -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_Strong x13) x3250 x3500) (d_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x14 x15) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x13 x1002 x3250 x3500) (d_OP__case_14 x13 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x13 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_14 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_14 x13 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_Strong x13) x2000 x3250 x3500) Curry_Prelude.OP_List))
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_Strong x13) x2000 x3250 x3500) (nd_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x14 x15) x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x13 x1002 x3000 x3250 x3500) (nd_OP__case_14 x13 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x13 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x13 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_15 x10 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_Emph x10) x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x11 x12) -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_Emph x10) x3250 x3500) (d_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x11 x12) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x10 x1002 x3250 x3500) (d_OP__case_15 x10 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x10 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x10 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_15 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_15 x10 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_Emph x10) x2000 x3250 x3500) Curry_Prelude.OP_List))
     (Curry_Prelude.OP_Cons x11 x12) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_Emph x10) x2000 x3250 x3500) (nd_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x11 x12) x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x10 x1002 x3000 x3250 x3500) (nd_OP__case_15 x10 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x10 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x10 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_16 x7 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_Text x7) x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x8 x9) -> Curry_Prelude.OP_Cons (d_C_mdElem2html (C_Text x7) x3250 x3500) (d_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x8 x9) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x7 x1002 x3250 x3500) (d_OP__case_16 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_16 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_16 x7 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_Text x7) x2000 x3250 x3500) Curry_Prelude.OP_List))
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_mdElem2html (C_Text x7) x2000 x3250 x3500) (nd_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x8 x9) x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x7 x1002 x3000 x3250 x3500) (nd_OP__case_16 x7 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x7 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.OP_List C_MarkdownElem -> Curry_Prelude.OP_List C_MarkdownElem -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_17 x4 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_apply (d_C_mdDoc2html x3250 x3500) x4 x3250 x3500
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_mdDoc2html x3250 x3500) x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_HTML.d_C_breakline x3250 x3500) (d_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x5 x6) x3250 x3500)) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x4 x1002 x3250 x3500) (d_OP__case_17 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_17 :: Curry_Prelude.OP_List C_MarkdownElem -> Curry_Prelude.OP_List C_MarkdownElem -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_17 x4 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_mdDoc2html x2000 x3250 x3500) x4 x2001 x3250 x3500)))))
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2002 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2002 (seq x2005 (Curry_Prelude.d_OP_plus_plus (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_mdDoc2html x2000 x3250 x3500) x4 x2001 x3250 x3500)))) (let
                    x2003 = leftSupply x2005
                    x2004 = rightSupply x2005
                     in (seq x2003 (seq x2004 (Curry_Prelude.OP_Cons (Curry_HTML.nd_C_breakline x2003 x3250 x3500) (nd_C_mdDoc2htmlWithoutPar (Curry_Prelude.OP_Cons x5 x6) x2004 x3250 x3500))))) x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x4 x1002 x3000 x3250 x3500) (nd_OP__case_17 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_HTML.C_HtmlExp
d_OP__case_19 x6 x5 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_HTML.d_C_href x6 (Curry_Prelude.OP_Cons (Curry_HTML.d_C_code (Curry_Prelude.OP_Cons (d_C_mdtxt2html x5 x3250 x3500) Curry_Prelude.OP_List) x3250 x3500) Curry_Prelude.OP_List) x3250 x3500
     Curry_Prelude.C_False -> Curry_HTML.d_C_href x6 (Curry_Prelude.OP_Cons (d_C_mdtxt2html x5 x3250 x3500) Curry_Prelude.OP_List) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x6 x5 x1002 x3250 x3500) (d_OP__case_19 x6 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x6 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_19 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_HTML.C_HtmlExp
nd_OP__case_19 x6 x5 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_HTML.nd_C_href x6 (Curry_Prelude.OP_Cons (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_HTML.nd_C_code (Curry_Prelude.OP_Cons (nd_C_mdtxt2html x5 x2000 x3250 x3500) Curry_Prelude.OP_List) x2001 x3250 x3500)))) Curry_Prelude.OP_List) x2003 x3250 x3500)))))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_HTML.nd_C_href x6 (Curry_Prelude.OP_Cons (nd_C_mdtxt2html x5 x2000 x3250 x3500) Curry_Prelude.OP_List) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x6 x5 x1002 x3000 x3250 x3500) (nd_OP__case_19 x6 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x6 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x6 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_29 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_SourceMDElem
d_OP__case_29 x5 x1 x4 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_28 x2 x1 x4 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_25 x5 x1 x4 x2 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '_'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x5 x1 x4 x2 x1002 x3250 x3500) (d_OP__case_29 x5 x1 x4 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x5 x1 x4 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x5 x1 x4 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_SourceMDElem
d_OP__case_25 x5 x1 x4 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_24 x2 x1 x4 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_21 x5 x1 x4 x2 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '`'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x5 x1 x4 x2 x1002 x3250 x3500) (d_OP__case_25 x5 x1 x4 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x5 x1 x4 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x5 x1 x4 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_SourceMDElem
d_OP__case_21 x5 x1 x4 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_20 x1 x2 x4 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x5 x1 x4 x2 x1002 x3250 x3500) (d_OP__case_21 x5 x1 x4 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x5 x1 x4 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x5 x1 x4 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_SourceMDElem
d_OP__case_20 x1 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_SMDCode x2
     (Curry_Prelude.OP_Cons x16 x17) -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1 x2 x1002 x3250 x3500) (d_OP__case_20 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_SourceMDElem
d_OP__case_24 x2 x1 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x11 x12) -> let
          x13 = x11
           in (d_OP__case_23 x13 x1 x12 x2 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char '_'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> C_SMDEmph x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x2 x1 x1002 x3250 x3500) (d_OP__case_24 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_SourceMDElem
d_OP__case_23 x13 x1 x12 x2 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> d_OP__case_22 x1 x2 x12 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x13 x1 x12 x2 x1002 x3250 x3500) (d_OP__case_23 x13 x1 x12 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x13 x1 x12 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x13 x1 x12 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_SourceMDElem
d_OP__case_22 x1 x2 x12 x3250 x3500 = case x12 of
     Curry_Prelude.OP_List -> C_SMDStrong x2
     (Curry_Prelude.OP_Cons x14 x15) -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1 x2 x1002 x3250 x3500) (d_OP__case_22 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_SourceMDElem
d_OP__case_28 x2 x1 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x8 = x6
           in (d_OP__case_27 x8 x1 x7 x2 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char '*'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> C_SMDEmph x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x2 x1 x1002 x3250 x3500) (d_OP__case_28 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_SourceMDElem
d_OP__case_27 x8 x1 x7 x2 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_OP__case_26 x1 x2 x7 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x8 x1 x7 x2 x1002 x3250 x3500) (d_OP__case_27 x8 x1 x7 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x8 x1 x7 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x8 x1 x7 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_SourceMDElem
d_OP__case_26 x1 x2 x7 x3250 x3500 = case x7 of
     Curry_Prelude.OP_List -> C_SMDStrong x2
     (Curry_Prelude.OP_Cons x9 x10) -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x1 x2 x1002 x3250 x3500) (d_OP__case_26 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_34 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_34 x3 x1 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (d_C_text2MDElem x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) x2 x3250 x3500) x3250 x3500) (d_C_outsideMarkdownElem Curry_Prelude.OP_List (Curry_Prelude.d_C_drop (Curry_Prelude.d_C_length x1 x3250 x3500) x3 x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_33 x2 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_34 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_33 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_33 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (C_SMDText (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) x2 x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = x4
           in (d_OP__case_32 x6 x5 x2 x1 x4 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.C_Char '\\'#) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x2 x1 x1002 x3250 x3500) (d_OP__case_33 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_32 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_32 x6 x5 x2 x1 x4 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_OP__case_31 x2 x4 x1 x5 x3250 x3500
     Curry_Prelude.C_False -> d_C_insideMarkdownElem x1 (Curry_Prelude.OP_Cons x6 x2) x5 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x6 x5 x2 x1 x4 x1002 x3250 x3500) (d_OP__case_32 x6 x5 x2 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 x6 x5 x2 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x6 x5 x2 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_31 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_31 x2 x4 x1 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x7 x8) -> d_OP__case_30 x7 x8 x2 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x7 x3250 x3500) (d_C_markdownEscapeChars x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.OP_List -> d_C_insideMarkdownElem x1 (Curry_Prelude.OP_Cons x4 x2) Curry_Prelude.OP_List x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x2 x4 x1 x1002 x3250 x3500) (d_OP__case_31 x2 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x2 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x2 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_30 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_30 x7 x8 x2 x1 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_insideMarkdownElem x1 (Curry_Prelude.OP_Cons x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) x2)) x8 x3250 x3500
     Curry_Prelude.C_False -> d_C_insideMarkdownElem x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) x2) (Curry_Prelude.OP_Cons x7 x8) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x7 x8 x2 x1 x1002 x3250 x3500) (d_OP__case_30 x7 x8 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x7 x8 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x7 x8 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_35 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_35 x4 x3 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> d_C_outsideMarkdownElem Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) x1) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (C_SMDHRef x3 x3) (d_C_outsideMarkdownElem Curry_Prelude.OP_List (d_C_dropFirst x4 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x4 x3 x1 x1002 x3250 x3500) (d_OP__case_35 x4 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x4 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x4 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_37 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_37 x4 x3 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_outsideMarkdownElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) x1 x3250 x3500
     Curry_Prelude.C_False -> let
          x5 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char ')'#)) x3250 x3500) (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 2#) x4 x3250 x3500) x3250 x3500
          x6 = d_OP_tryParseLink_dot___hash_selFP42_hash_url x5 x3250 x3500
          x7 = d_OP_tryParseLink_dot___hash_selFP43_hash_mtxt x5 x3250 x3500
           in (d_OP__case_36 x7 x6 x3 x1 (Curry_Prelude.d_C_null x7 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x4 x3 x1 x1002 x3250 x3500) (d_OP__case_37 x4 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x4 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x4 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_36 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_36 x7 x6 x3 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_outsideMarkdownElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) x1 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (C_SMDHRef x3 x6) (d_C_outsideMarkdownElem Curry_Prelude.OP_List (Curry_Prelude.d_C_tail x7 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x7 x6 x3 x1 x1002 x3250 x3500) (d_OP__case_36 x7 x6 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x7 x6 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x7 x6 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_38 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_SourceMDElem -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_38 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (C_SMDText (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) x1 x3250 x3500)) x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x1 x2 x1002 x3250 x3500) (d_OP__case_38 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_51 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_51 x5 x4 x1 x3 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_50 x1 x3 x4 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_48 x5 x4 x1 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '*'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_51 x5 x4 x1 x3 x1002 x3250 x3500) (d_OP__case_51 x5 x4 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_51 x5 x4 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_51 x5 x4 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_48 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_48 x5 x4 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_47 x1 x4 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_45 x5 x4 x1 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '_'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_48 x5 x4 x1 x1002 x3250 x3500) (d_OP__case_48 x5 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_48 x5 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_48 x5 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_45 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_45 x5 x4 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_44 x1 x4 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_42 x5 x4 x1 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '`'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x5 x4 x1 x1002 x3250 x3500) (d_OP__case_45 x5 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 x5 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x5 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_42 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_42 x5 x4 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar (d_C_addPrevious x1) (d_C_insideMarkdownElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List) Curry_Prelude.OP_List x4 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_41 x5 x4 x1 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '['#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x5 x4 x1 x1002 x3250 x3500) (d_OP__case_42 x5 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x5 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x5 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_41 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_41 x5 x4 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar (d_C_addPrevious x1) (d_C_tryParseLink x4 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_40 x5 x4 x1 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '<'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x5 x4 x1 x1002 x3250 x3500) (d_OP__case_41 x5 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x5 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x5 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_40 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_40 x5 x4 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_39 x4 x1 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 4#) x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_outsideMarkdownElem (Curry_Prelude.OP_Cons x5 x1) x4 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x5 x4 x1 x1002 x3250 x3500) (d_OP__case_40 x5 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x5 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x5 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_39 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_39 x4 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar (d_C_addPrevious x1) (d_C_markdownHRef x4 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_outsideMarkdownElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) x1) x4 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x4 x1 x1002 x3250 x3500) (d_OP__case_39 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_44 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_44 x1 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x11 x12) -> let
          x13 = x11
           in (d_OP__case_43 x13 x4 x1 x12 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char '_'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_dollar (d_C_addPrevious x1) (d_C_insideMarkdownElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List) Curry_Prelude.OP_List Curry_Prelude.OP_List x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x1 x1002 x3250 x3500) (d_OP__case_44 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_43 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_43 x13 x4 x1 x12 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar (d_C_addPrevious x1) (d_C_insideMarkdownElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List)) Curry_Prelude.OP_List x12 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_dollar (d_C_addPrevious x1) (d_C_insideMarkdownElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List) Curry_Prelude.OP_List x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x13 x4 x1 x12 x1002 x3250 x3500) (d_OP__case_43 x13 x4 x1 x12 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x13 x4 x1 x12 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x13 x4 x1 x12 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_47 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_47 x1 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x10 = x8
           in (d_OP__case_46 x10 x4 x1 x9 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char '*'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_dollar (d_C_addPrevious x1) (d_C_insideMarkdownElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List) Curry_Prelude.OP_List Curry_Prelude.OP_List x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_47 x1 x1002 x3250 x3500) (d_OP__case_47 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_47 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_47 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_46 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_46 x10 x4 x1 x9 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar (d_C_addPrevious x1) (d_C_insideMarkdownElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List)) Curry_Prelude.OP_List x9 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_dollar (d_C_addPrevious x1) (d_C_insideMarkdownElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List) Curry_Prelude.OP_List x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_46 x10 x4 x1 x9 x1002 x3250 x3500) (d_OP__case_46 x10 x4 x1 x9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_46 x10 x4 x1 x9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_46 x10 x4 x1 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_50 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_50 x1 x3 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_49 x6 x7 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x6 x3250 x3500) (d_C_markdownEscapeChars x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.OP_List -> d_C_outsideMarkdownElem (Curry_Prelude.OP_Cons x3 x1) Curry_Prelude.OP_List x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_50 x1 x3 x1002 x3250 x3500) (d_OP__case_50 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_50 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_50 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_49 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_49 x6 x7 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_outsideMarkdownElem (Curry_Prelude.OP_Cons x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) x1)) x7 x3250 x3500
     Curry_Prelude.C_False -> d_C_outsideMarkdownElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) x1) (Curry_Prelude.OP_Cons x6 x7) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_49 x6 x7 x1 x1002 x3250 x3500) (d_OP__case_49 x6 x7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_49 x6 x7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_49 x6 x7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_54 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_54 x4 x3 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> d_OP__case_53 x2 x3 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x4 (d_C_removeEscapes x3 x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_54 x4 x3 x2 x1002 x3250 x3500) (d_OP__case_54 x4 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_54 x4 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_54 x4 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_53 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_53 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP__case_52 x5 x6 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x5 x3250 x3500) (d_C_markdownEscapeChars x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x2 (d_C_removeEscapes Curry_Prelude.OP_List x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_53 x2 x1002 x3250 x3500) (d_OP__case_53 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_53 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_53 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_52 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_52 x5 x6 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x5 (d_C_removeEscapes x6 x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (d_C_removeEscapes (Curry_Prelude.OP_Cons x5 x6) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_52 x5 x6 x1002 x3250 x3500) (d_OP__case_52 x5 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_52 x5 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_52 x5 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_58 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_SourceMDElem) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_58 x2 x4 x3 x1 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x5 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '\n'#)) x3250 x3500) (Curry_Prelude.d_C_drop x2 x4 x3250 x3500) x3250 x3500
          x6 = d_OP_markdownItem_dot___hash_selFP35_hash_fstline x5 x3250 x3500
          x7 = d_OP_markdownItem_dot___hash_selFP36_hash_remtxt x5 x3250 x3500
           in (d_OP__case_57 x7 x6 x3 x2 x1 (Curry_Prelude.d_C_null x7 x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> let
          x8 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '\n'#)) x3250 x3500) x4 x3250 x3500
          x9 = d_OP_markdownItem_dot___hash_selFP38_hash_fstline x8 x3250 x3500
          x10 = d_OP_markdownItem_dot___hash_selFP39_hash_remtxt x8 x3250 x3500
           in (d_OP__case_56 x9 x4 x3 x1 x10 x2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Char.d_C_isSpace x3250 x3500) x9 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_58 x2 x4 x3 x1 x1002 x3250 x3500) (d_OP__case_58 x2 x4 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_58 x2 x4 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_58 x2 x4 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_58 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_SourceMDElem -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
nd_OP__case_58 x2 x4 x3 x1 x11 x3000 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x5 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_break (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) (Curry_Prelude.C_Char '\n'#))) x2000 x3250 x3500) (Curry_Prelude.d_C_drop x2 x4 x3250 x3500) x2001 x3250 x3500)))
                    x6 = d_OP_markdownItem_dot___hash_selFP35_hash_fstline x5 x3250 x3500
                    x7 = d_OP_markdownItem_dot___hash_selFP36_hash_remtxt x5 x3250 x3500
                     in (nd_OP__case_57 x7 x6 x3 x2 x1 (Curry_Prelude.d_C_null x7 x3250 x3500) x2003 x3250 x3500))))))
     Curry_Prelude.C_False -> let
          x2008 = x3000
           in (seq x2008 (let
               x2002 = leftSupply x2008
               x2007 = rightSupply x2008
                in (seq x2002 (seq x2007 (let
                    x8 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_break (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) (Curry_Prelude.C_Char '\n'#))) x2000 x3250 x3500) x4 x2001 x3250 x3500)))
                    x9 = d_OP_markdownItem_dot___hash_selFP38_hash_fstline x8 x3250 x3500
                    x10 = d_OP_markdownItem_dot___hash_selFP39_hash_remtxt x8 x3250 x3500
                     in (let
                         x2006 = leftSupply x2007
                         x2005 = rightSupply x2007
                          in (seq x2006 (seq x2005 (nd_OP__case_56 x9 x4 x3 x1 x10 x2 (let
                              x2004 = leftSupply x2005
                              x2003 = rightSupply x2005
                               in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_all (wrapDX id Curry_Char.d_C_isSpace) x2003 x3250 x3500) x9 x2004 x3250 x3500)))) x2006 x3250 x3500)))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_58 x2 x4 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_58 x2 x4 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_58 x2 x4 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_58 x2 x4 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_56 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_SourceMDElem) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_56 x9 x4 x3 x1 x10 x2 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> d_OP__case_55 x10 x3 x2 x1 (Curry_Prelude.d_C_null x10 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) (d_C_markdownText x4 x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_56 x9 x4 x3 x1 x10 x2 x1002 x3250 x3500) (d_OP__case_56 x9 x4 x3 x1 x10 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_56 x9 x4 x3 x1 x10 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_56 x9 x4 x3 x1 x10 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_56 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_SourceMDElem -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
nd_OP__case_56 x9 x4 x3 x1 x10 x2 x11 x3000 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_55 x10 x3 x2 x1 (Curry_Prelude.d_C_null x10 x3250 x3500) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) (d_C_markdownText x4 x3250 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_56 x9 x4 x3 x1 x10 x2 x1002 x3000 x3250 x3500) (nd_OP__case_56 x9 x4 x3 x1 x10 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_56 x9 x4 x3 x1 x10 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_56 x9 x4 x3 x1 x10 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_55 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_SourceMDElem) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_55 x10 x3 x2 x1 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_C_markdownItem x1 x2 (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.d_C_tail x10 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_55 x10 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_55 x10 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_55 x10 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_55 x10 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_55 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_SourceMDElem -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
nd_OP__case_55 x10 x3 x2 x1 x11 x3000 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) Curry_Prelude.OP_List))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_markdownItem x1 x2 (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.d_C_tail x10 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_55 x10 x3 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_55 x10 x3 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_55 x10 x3 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_55 x10 x3 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_57 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_SourceMDElem) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_57 x7 x6 x3 x2 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply x1 (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) x6) x3250 x3500) x3250 x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_C_markdownItem x1 x2 (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) x6) x3250 x3500) (Curry_Prelude.d_C_tail x7 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_57 x7 x6 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_57 x7 x6 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_57 x7 x6 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_57 x7 x6 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_57 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_SourceMDElem -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
nd_OP__case_57 x7 x6 x3 x2 x1 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_Prelude.nd_C_apply x1 (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) x6) x3250 x3500) x2000 x3250 x3500) Curry_Prelude.OP_List))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_markdownItem x1 x2 (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) x6) x3250 x3500) (Curry_Prelude.d_C_tail x7 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_57 x7 x6 x3 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_57 x7 x6 x3 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_57 x7 x6 x3 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_57 x7 x6 x3 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_60 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_60 x3 x1 x2 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x4 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '\n'#)) x3250 x3500) (Curry_Prelude.d_C_drop x1 x3 x3250 x3500) x3250 x3500
          x5 = d_OP_markdownCodeBlock_dot___hash_selFP32_hash_fstline x4 x3250 x3500
          x6 = d_OP_markdownCodeBlock_dot___hash_selFP33_hash_remtxt x4 x3250 x3500
           in (d_OP__case_59 x6 x5 x2 x1 (Curry_Prelude.d_C_null x6 x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (C_SMDCodeBlock x2) (d_C_markdownText x3 x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_60 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_60 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_60 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_60 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_59 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_59 x6 x5 x2 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_SMDCodeBlock (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (d_C_removeEscapes x5 x3250 x3500)) x3250 x3500)) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_C_markdownCodeBlock x1 (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (d_C_removeEscapes x5 x3250 x3500)) x3250 x3500) (Curry_Prelude.d_C_tail x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_59 x6 x5 x2 x1 x1002 x3250 x3500) (d_OP__case_59 x6 x5 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_59 x6 x5 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_59 x6 x5 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_61 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_61 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 1#) x2 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_61 x2 x1002 x3250 x3500) (d_OP__case_61 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_61 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_61 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_63 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_63 x3 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x4 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '\n'#)) x3250 x3500) (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 2#) x3 x3250 x3500) x3250 x3500
          x5 = d_OP_markdownQuote_dot___hash_selFP29_hash_fstline x4 x3250 x3500
          x6 = d_OP_markdownQuote_dot___hash_selFP30_hash_remtxt x4 x3250 x3500
           in (d_OP__case_62 x6 x5 x1 (Curry_Prelude.d_C_null x6 x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (C_SMDQuote (Curry_Prelude.d_C_apply (d_C_fromMarkdownText x3250 x3500) x1 x3250 x3500)) (d_C_markdownText x3 x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_63 x3 x1 x1002 x3250 x3500) (d_OP__case_63 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_63 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_63 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_62 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_62 x6 x5 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_SMDQuote (Curry_Prelude.d_C_apply (d_C_fromMarkdownText x3250 x3500) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) x5) x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_C_markdownQuote (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) x5) x3250 x3500) (Curry_Prelude.d_C_tail x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_62 x6 x5 x1 x1002 x3250 x3500) (d_OP__case_62 x6 x5 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_62 x6 x5 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_62 x6 x5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_66 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_66 x6 x7 x2 x5 x4 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_SMDPar (d_C_groupMarkDownElems (d_C_outsideMarkdownElem Curry_Prelude.OP_List x1 x3250 x3500) x3250 x3500)) (d_C_markdownText x2 x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_65 x5 x4 x1 (Curry_Prelude.d_C_null x5 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_66 x6 x7 x2 x5 x4 x1 x1002 x3250 x3500) (d_OP__case_66 x6 x7 x2 x5 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_66 x6 x7 x2 x5 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_66 x6 x7 x2 x5 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_65 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_65 x5 x4 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_SMDPar (d_C_groupMarkDownElems (d_C_outsideMarkdownElem Curry_Prelude.OP_List (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) x4) x3250 x3500) x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_64 x5 x4 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_65 x5 x4 x1 x1002 x3250 x3500) (d_OP__case_65 x5 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_65 x5 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_65 x5 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_64 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_64 x5 x4 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_markdownPar (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) x4) x3250 x3500) (Curry_Prelude.d_C_tail x5 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_64 x5 x4 x1 x1002 x3250 x3500) (d_OP__case_64 x5 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_64 x5 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_64 x5 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_67 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_67 x5 x4 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> x5
     Curry_Prelude.C_False -> Curry_Prelude.C_Int 0#
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_67 x5 x4 x1002 x3250 x3500) (d_OP__case_67 x5 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_67 x5 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_67 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_68 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_68 x8 x7 x9 x4 x1 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_plus x1 x9 x3250 x3500) (Curry_Prelude.d_C_length x7 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Int 0#
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_68 x8 x7 x9 x4 x1 x1002 x3250 x3500) (d_OP__case_68 x8 x7 x9 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_68 x8 x7 x9 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_68 x8 x7 x9 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_69 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_69 x4 x3 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus (Curry_Prelude.d_C_length x3 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Int 0#
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_69 x4 x3 x1002 x3250 x3500) (d_OP__case_69 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_69 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_69 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_70 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_70 x6 x5 x2 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_markdownPar x1 x2 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (C_SMDHeader x6 (d_C_dropFirst x5 x3250 x3500)) (d_C_markdownText x2 x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_70 x6 x5 x2 x1 x1002 x3250 x3500) (d_OP__case_70 x6 x5 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_70 x6 x5 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_70 x6 x5 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_71 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP__case_71 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_C_tail x1 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_71 x1 x1002 x3250 x3500) (d_OP__case_71 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_71 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_71 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_81 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_81 x1 x6 x7 x10 x9 x8 x2 x5 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> d_C_markdownText x2 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_80 x7 x1 x10 x9 x8 x2 x5 x6 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_81 x1 x6 x7 x10 x9 x8 x2 x5 x1002 x3250 x3500) (d_OP__case_81 x1 x6 x7 x10 x9 x8 x2 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_81 x1 x6 x7 x10 x9 x8 x2 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_81 x1 x6 x7 x10 x9 x8 x2 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_80 :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_80 x7 x1 x10 x9 x8 x2 x5 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_SMDHeader (Curry_Prelude.C_Int 1#) x1) (d_C_markdownText (d_C_dropFirst x5 x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_79 x1 x10 x9 x8 x2 x5 x7 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_80 x7 x1 x10 x9 x8 x2 x5 x1002 x3250 x3500) (d_OP__case_80 x7 x1 x10 x9 x8 x2 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_80 x7 x1 x10 x9 x8 x2 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_80 x7 x1 x10 x9 x8 x2 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_79 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_79 x1 x10 x9 x8 x2 x5 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_SMDHeader (Curry_Prelude.C_Int 2#) x1) (d_C_markdownText (d_C_dropFirst x5 x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_78 x1 x10 x9 x8 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 1#) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_79 x1 x10 x9 x8 x2 x5 x1002 x3250 x3500) (d_OP__case_79 x1 x10 x9 x8 x2 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_79 x1 x10 x9 x8 x2 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_79 x1 x10 x9 x8 x2 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_78 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_78 x1 x10 x9 x8 x2 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> d_C_tryMDHeader x1 x2 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_77 x1 x10 x9 x8 x2 (d_C_isHRule x1 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_78 x1 x10 x9 x8 x2 x1002 x3250 x3500) (d_OP__case_78 x1 x10 x9 x8 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_78 x1 x10 x9 x8 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_78 x1 x10 x9 x8 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_77 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_77 x1 x10 x9 x8 x2 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons C_SMDHRule (d_C_markdownText x2 x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_76 x1 x10 x9 x8 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 2#) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_77 x1 x10 x9 x8 x2 x1002 x3250 x3500) (d_OP__case_77 x1 x10 x9 x8 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_77 x1 x10 x9 x8 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_77 x1 x10 x9 x8 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_76 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_76 x1 x10 x9 x8 x2 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> d_C_markdownQuote (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 2#) x1 x3250 x3500) x2 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_75 x10 x9 x8 x2 x1 (Curry_Prelude.d_OP_gt x10 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_76 x1 x10 x9 x8 x2 x1002 x3250 x3500) (d_OP__case_76 x1 x10 x9 x8 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_76 x1 x10 x9 x8 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_76 x1 x10 x9 x8 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_75 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_75 x10 x9 x8 x2 x1 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> d_C_markdownCodeBlock x10 (d_C_removeEscapes (Curry_Prelude.d_C_drop x10 x1 x3250 x3500) x3250 x3500) x2 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_74 x9 x8 x2 x1 (Curry_Prelude.d_OP_gt x9 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_75 x10 x9 x8 x2 x1 x1002 x3250 x3500) (d_OP__case_75 x10 x9 x8 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_75 x10 x9 x8 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_75 x10 x9 x8 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_74 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_74 x9 x8 x2 x1 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> d_C_markdownItem (acceptCs id C_SMDUItem) x9 (Curry_Prelude.d_C_drop x9 x1 x3250 x3500) x2 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_73 x8 x2 x1 (Curry_Prelude.d_OP_gt x8 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_74 x9 x8 x2 x1 x1002 x3250 x3500) (d_OP__case_74 x9 x8 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_74 x9 x8 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_74 x9 x8 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_73 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_73 x8 x2 x1 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_markdownItem (acceptCs id C_SMDOItem) x8 (Curry_Prelude.d_C_drop x8 x1 x3250 x3500) x2 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_72 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_73 x8 x2 x1 x1002 x3250 x3500) (d_OP__case_73 x8 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_73 x8 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_73 x8 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_72 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceMDElem
d_OP__case_72 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> d_C_markdownPar x1 x2 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_72 x2 x1 x1002 x3250 x3500) (d_OP__case_72 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_72 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_72 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_82 :: C_SourceMDElem -> (C_SourceMDElem -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List C_SourceMDElem -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.OP_List (Curry_Prelude.OP_List C_MarkdownElem) -> Cover -> ConstStore -> C_MarkdownElem) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_MarkdownElem
d_OP__case_82 x5 x2 x6 x3 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_joinItems x1 x2 (Curry_Prelude.OP_Cons (d_C_textOfItem x5 x3250 x3500) x3) x6 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) (Curry_Prelude.d_C_map (d_C_fromMarkdownText x3250 x3500) x3 x3250 x3500) x3250 x3500) x3250 x3500) (d_C_groupMarkDownElems (Curry_Prelude.OP_Cons x5 x6) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_82 x5 x2 x6 x3 x1 x1002 x3250 x3500) (d_OP__case_82 x5 x2 x6 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_82 x5 x2 x6 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_82 x5 x2 x6 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_82 :: C_SourceMDElem -> Func C_SourceMDElem Curry_Prelude.C_Bool -> Curry_Prelude.OP_List C_SourceMDElem -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List C_MarkdownElem)) C_MarkdownElem -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_MarkdownElem
nd_OP__case_82 x5 x2 x6 x3 x1 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_joinItems x1 x2 (Curry_Prelude.OP_Cons (d_C_textOfItem x5 x3250 x3500) x3) x6 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2008 = x3000
           in (seq x2008 (Curry_Prelude.OP_Cons (let
               x2007 = leftSupply x2008
               x2005 = rightSupply x2008
                in (seq x2007 (seq x2005 (Curry_Prelude.nd_C_apply x1 (let
                    x2004 = leftSupply x2005
                    x2006 = rightSupply x2005
                     in (seq x2004 (seq x2006 (let
                         x2000 = leftSupply x2006
                         x2003 = rightSupply x2006
                          in (seq x2000 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2000 x3250 x3500) (let
                              x2002 = leftSupply x2003
                              x2001 = rightSupply x2003
                               in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_map (nd_C_fromMarkdownText x2001 x3250 x3500) x3 x2002 x3250 x3500)))) x2004 x3250 x3500))))))) x2007 x3250 x3500)))) (d_C_groupMarkDownElems (Curry_Prelude.OP_Cons x5 x6) x3250 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_82 x5 x2 x6 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_82 x5 x2 x6 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_82 x5 x2 x6 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_82 x5 x2 x6 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_83 :: Curry_Prelude.OP_List C_SourceMDElem -> C_SourceMDElem -> Cover -> ConstStore -> Curry_Prelude.OP_List C_MarkdownElem
d_OP__case_83 x3 x2 x3250 x3500 = case x2 of
     (C_SMDUItem x4) -> d_C_joinItems (acceptCs id C_UList) d_C_isSMDUItem (Curry_Prelude.OP_Cons x4 Curry_Prelude.OP_List) x3 x3250 x3500
     (C_SMDOItem x5) -> d_C_joinItems (acceptCs id C_OList) d_C_isSMDOItem (Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List) x3 x3250 x3500
     (C_SMDText x6) -> Curry_Prelude.OP_Cons (C_Text x6) (d_C_groupMarkDownElems x3 x3250 x3500)
     (C_SMDEmph x7) -> Curry_Prelude.OP_Cons (C_Emph x7) (d_C_groupMarkDownElems x3 x3250 x3500)
     (C_SMDStrong x8) -> Curry_Prelude.OP_Cons (C_Strong x8) (d_C_groupMarkDownElems x3 x3250 x3500)
     (C_SMDCode x9) -> Curry_Prelude.OP_Cons (C_Code x9) (d_C_groupMarkDownElems x3 x3250 x3500)
     (C_SMDHRef x10 x11) -> Curry_Prelude.OP_Cons (C_HRef x10 x11) (d_C_groupMarkDownElems x3 x3250 x3500)
     (C_SMDPar x12) -> Curry_Prelude.OP_Cons (C_Par x12) (d_C_groupMarkDownElems x3 x3250 x3500)
     (C_SMDCodeBlock x13) -> Curry_Prelude.OP_Cons (C_CodeBlock x13) (d_C_groupMarkDownElems x3 x3250 x3500)
     (C_SMDQuote x14) -> Curry_Prelude.OP_Cons (C_Quote x14) (d_C_groupMarkDownElems x3 x3250 x3500)
     C_SMDHRule -> Curry_Prelude.OP_Cons C_HRule (d_C_groupMarkDownElems x3 x3250 x3500)
     (C_SMDHeader x15 x16) -> Curry_Prelude.OP_Cons (C_Header x15 x16) (d_C_groupMarkDownElems x3 x3250 x3500)
     (Choice_C_SourceMDElem x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_83 x3 x1002 x3250 x3500) (d_OP__case_83 x3 x1003 x3250 x3500)
     (Choices_C_SourceMDElem x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_83 x3 z x3250 x3500) x1002
     (Guard_C_SourceMDElem x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_83 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SourceMDElem x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
