{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_HtmlParser (C_HtmlToken, d_C_readHtmlFile, nd_C_readHtmlFile, d_C_parseHtmlString, nd_C_parseHtmlString) where

import Basics
import qualified Curry_Char
import qualified Curry_HTML
import qualified Curry_Prelude
data C_HtmlToken
     = C_HText (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_HElem (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
     | Choice_C_HtmlToken Cover ID C_HtmlToken C_HtmlToken
     | Choices_C_HtmlToken Cover ID ([C_HtmlToken])
     | Fail_C_HtmlToken Cover FailInfo
     | Guard_C_HtmlToken Cover Constraints C_HtmlToken

instance Show C_HtmlToken where
  showsPrec d (Choice_C_HtmlToken cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_HtmlToken cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_HtmlToken cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_HtmlToken cd info) = showChar '!'
  showsPrec _ (C_HText x1) = (showString "(HText") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_HElem x1 x2) = (showString "(HElem") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_HtmlToken where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_HText x1,r1) | (_,r0) <- readQualified "HtmlParser" "HText" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_HElem x1 x2,r2) | (_,r0) <- readQualified "HtmlParser" "HElem" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)


instance NonDet C_HtmlToken where
  choiceCons = Choice_C_HtmlToken
  choicesCons = Choices_C_HtmlToken
  failCons = Fail_C_HtmlToken
  guardCons = Guard_C_HtmlToken
  try (Choice_C_HtmlToken cd i x y) = tryChoice cd i x y
  try (Choices_C_HtmlToken cd i xs) = tryChoices cd i xs
  try (Fail_C_HtmlToken cd info) = Fail cd info
  try (Guard_C_HtmlToken cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_HtmlToken cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_HtmlToken cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_HtmlToken cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_HtmlToken cd i _) = error ("HtmlParser.HtmlToken.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_HtmlToken cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_HtmlToken cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_HtmlToken where
  generate s c = Choices_C_HtmlToken c (freeID [1,2] s) [(C_HText (generate (leftSupply s) c)),(C_HElem (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_HtmlToken where
  ($!!) cont (C_HText x1) d cs = (((\y1 d cs -> cont (C_HText y1) d cs) $!! x1) d) cs
  ($!!) cont (C_HElem x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_HElem y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_HtmlToken cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_HtmlToken cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_HtmlToken cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_HtmlToken cd info) _ _ = failCons cd info
  ($##) cont (C_HText x1) d cs = (((\y1 d cs -> cont (C_HText y1) d cs) $## x1) d) cs
  ($##) cont (C_HElem x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_HElem y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_HtmlToken cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_HtmlToken cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_HtmlToken cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_HtmlToken cd info) _ _ = failCons cd info
  searchNF search cont (C_HText x1) = search (\y1 -> cont (C_HText y1)) x1
  searchNF search cont (C_HElem x1 x2) = search (\y1 -> search (\y2 -> cont (C_HElem y1 y2)) x2) x1
  searchNF _ _ x = error ("HtmlParser.HtmlToken.searchNF: no constructor: " ++ (show x))


instance Unifiable C_HtmlToken where
  (=.=) (C_HText x1) (C_HText y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_HElem x1 x2) (C_HElem y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_HText x1) (C_HText y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_HElem x1 x2) (C_HElem y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_HText x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_HElem x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_HtmlToken cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_HtmlToken cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_HtmlToken cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_HtmlToken cd i _) = error ("HtmlParser.HtmlToken.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_HtmlToken cd info) = [(Unsolvable info)]
  bind d i (Guard_C_HtmlToken cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_HText x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_HElem x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_HtmlToken cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_HtmlToken cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_HtmlToken cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_HtmlToken cd i _) = error ("HtmlParser.HtmlToken.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_HtmlToken cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_HtmlToken cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_HtmlToken where
  (=?=) (Choice_C_HtmlToken cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_HtmlToken cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_HtmlToken cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_HtmlToken cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_HtmlToken cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_HtmlToken cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_HtmlToken cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_HtmlToken cd info) _ _ = failCons cd info
  (=?=) (C_HText x1) (C_HText y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_HElem x1 x2) (C_HElem y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_HtmlToken cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_HtmlToken cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_HtmlToken cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_HtmlToken cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_HtmlToken cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_HtmlToken cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_HtmlToken cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_HtmlToken cd info) _ _ = failCons cd info
  (<?=) (C_HText x1) (C_HText y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_HText _) (C_HElem _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_HElem x1 x2) (C_HElem y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_readHtmlFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
d_C_readHtmlFile x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return d_C_parseHtmlString x3250 x3500) x3250 x3500

nd_C_readHtmlFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
nd_C_readHtmlFile x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3250 x3500) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) (wrapNX id nd_C_parseHtmlString) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_parseHtmlString :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_C_parseHtmlString x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) (d_C_parseHtmlTokens Curry_Prelude.OP_List (d_C_scanHtmlString x1 x3250 x3500) x3250 x3500) x3250 x3500

nd_C_parseHtmlString :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_C_parseHtmlString x1 x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2000 x3250 x3500) (nd_C_parseHtmlTokens Curry_Prelude.OP_List (d_C_scanHtmlString x1 x3250 x3500) x2001 x3250 x3500) x2002 x3250 x3500))))))))

d_C_parseHtmlTokens :: Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Curry_Prelude.OP_List C_HtmlToken -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_C_parseHtmlTokens x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> x1
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_24 x4 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_parseHtmlTokens x1 x1002 x3250 x3500) (d_C_parseHtmlTokens x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_parseHtmlTokens x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_parseHtmlTokens x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_parseHtmlTokens :: Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Curry_Prelude.OP_List C_HtmlToken -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_C_parseHtmlTokens x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> x1
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_24 x4 x1 x3 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_parseHtmlTokens x1 x1002 x3000 x3250 x3500) (nd_C_parseHtmlTokens x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_parseHtmlTokens x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_parseHtmlTokens x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs x1002 x3250 x3500) (d_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs x1002 x3000 x3250 x3500) (nd_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems x1002 x3250 x3500) (d_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems x1002 x3000 x3250 x3500) (nd_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest x1002 x3250 x3500) (d_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest x1002 x3000 x3250 x3500) (nd_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_splitHtmlElems :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
d_C_splitHtmlElems x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple3 Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_21 x1 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_splitHtmlElems x1 x1002 x3250 x3500) (d_C_splitHtmlElems x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_splitHtmlElems x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_splitHtmlElems x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_splitHtmlElems :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
nd_C_splitHtmlElems x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple3 Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_21 x1 x4 x3 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_splitHtmlElems x1 x1002 x3000 x3250 x3500) (nd_C_splitHtmlElems x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_splitHtmlElems x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_splitHtmlElems x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitHtmlElems_dot___hash_selFP6_hash_largs :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_splitHtmlElems_dot___hash_selFP6_hash_largs x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitHtmlElems_dot___hash_selFP6_hash_largs x1002 x3250 x3500) (d_OP_splitHtmlElems_dot___hash_selFP6_hash_largs x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitHtmlElems_dot___hash_selFP6_hash_largs z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitHtmlElems_dot___hash_selFP6_hash_largs x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_splitHtmlElems_dot___hash_selFP6_hash_largs :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_splitHtmlElems_dot___hash_selFP6_hash_largs x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_splitHtmlElems_dot___hash_selFP6_hash_largs x1002 x3000 x3250 x3500) (nd_OP_splitHtmlElems_dot___hash_selFP6_hash_largs x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_splitHtmlElems_dot___hash_selFP6_hash_largs z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_splitHtmlElems_dot___hash_selFP6_hash_largs x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitHtmlElems_dot___hash_selFP7_hash_elems :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP_splitHtmlElems_dot___hash_selFP7_hash_elems x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitHtmlElems_dot___hash_selFP7_hash_elems x1002 x3250 x3500) (d_OP_splitHtmlElems_dot___hash_selFP7_hash_elems x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitHtmlElems_dot___hash_selFP7_hash_elems z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitHtmlElems_dot___hash_selFP7_hash_elems x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_splitHtmlElems_dot___hash_selFP7_hash_elems :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP_splitHtmlElems_dot___hash_selFP7_hash_elems x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_splitHtmlElems_dot___hash_selFP7_hash_elems x1002 x3000 x3250 x3500) (nd_OP_splitHtmlElems_dot___hash_selFP7_hash_elems x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_splitHtmlElems_dot___hash_selFP7_hash_elems z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_splitHtmlElems_dot___hash_selFP7_hash_elems x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitHtmlElems_dot___hash_selFP8_hash_rest :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP_splitHtmlElems_dot___hash_selFP8_hash_rest x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitHtmlElems_dot___hash_selFP8_hash_rest x1002 x3250 x3500) (d_OP_splitHtmlElems_dot___hash_selFP8_hash_rest x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitHtmlElems_dot___hash_selFP8_hash_rest z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitHtmlElems_dot___hash_selFP8_hash_rest x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_splitHtmlElems_dot___hash_selFP8_hash_rest :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP_splitHtmlElems_dot___hash_selFP8_hash_rest x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_splitHtmlElems_dot___hash_selFP8_hash_rest x1002 x3000 x3250 x3500) (nd_OP_splitHtmlElems_dot___hash_selFP8_hash_rest x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_splitHtmlElems_dot___hash_selFP8_hash_rest z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_splitHtmlElems_dot___hash_selFP8_hash_rest x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitHtmlElems_dot___hash_selFP10_hash_largs :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_splitHtmlElems_dot___hash_selFP10_hash_largs x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitHtmlElems_dot___hash_selFP10_hash_largs x1002 x3250 x3500) (d_OP_splitHtmlElems_dot___hash_selFP10_hash_largs x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitHtmlElems_dot___hash_selFP10_hash_largs z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitHtmlElems_dot___hash_selFP10_hash_largs x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_splitHtmlElems_dot___hash_selFP10_hash_largs :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_splitHtmlElems_dot___hash_selFP10_hash_largs x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_splitHtmlElems_dot___hash_selFP10_hash_largs x1002 x3000 x3250 x3500) (nd_OP_splitHtmlElems_dot___hash_selFP10_hash_largs x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_splitHtmlElems_dot___hash_selFP10_hash_largs z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_splitHtmlElems_dot___hash_selFP10_hash_largs x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitHtmlElems_dot___hash_selFP11_hash_elems :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP_splitHtmlElems_dot___hash_selFP11_hash_elems x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitHtmlElems_dot___hash_selFP11_hash_elems x1002 x3250 x3500) (d_OP_splitHtmlElems_dot___hash_selFP11_hash_elems x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitHtmlElems_dot___hash_selFP11_hash_elems z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitHtmlElems_dot___hash_selFP11_hash_elems x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_splitHtmlElems_dot___hash_selFP11_hash_elems :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP_splitHtmlElems_dot___hash_selFP11_hash_elems x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_splitHtmlElems_dot___hash_selFP11_hash_elems x1002 x3000 x3250 x3500) (nd_OP_splitHtmlElems_dot___hash_selFP11_hash_elems x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_splitHtmlElems_dot___hash_selFP11_hash_elems z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_splitHtmlElems_dot___hash_selFP11_hash_elems x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitHtmlElems_dot___hash_selFP12_hash_rest :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP_splitHtmlElems_dot___hash_selFP12_hash_rest x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitHtmlElems_dot___hash_selFP12_hash_rest x1002 x3250 x3500) (d_OP_splitHtmlElems_dot___hash_selFP12_hash_rest x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitHtmlElems_dot___hash_selFP12_hash_rest z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitHtmlElems_dot___hash_selFP12_hash_rest x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_splitHtmlElems_dot___hash_selFP12_hash_rest :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP_splitHtmlElems_dot___hash_selFP12_hash_rest x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_splitHtmlElems_dot___hash_selFP12_hash_rest x1002 x3000 x3250 x3500) (nd_OP_splitHtmlElems_dot___hash_selFP12_hash_rest x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_splitHtmlElems_dot___hash_selFP12_hash_rest z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_splitHtmlElems_dot___hash_selFP12_hash_rest x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitHtmlElems_dot___hash_selFP14_hash_largs :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_splitHtmlElems_dot___hash_selFP14_hash_largs x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitHtmlElems_dot___hash_selFP14_hash_largs x1002 x3250 x3500) (d_OP_splitHtmlElems_dot___hash_selFP14_hash_largs x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitHtmlElems_dot___hash_selFP14_hash_largs z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitHtmlElems_dot___hash_selFP14_hash_largs x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_splitHtmlElems_dot___hash_selFP14_hash_largs :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_splitHtmlElems_dot___hash_selFP14_hash_largs x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_splitHtmlElems_dot___hash_selFP14_hash_largs x1002 x3000 x3250 x3500) (nd_OP_splitHtmlElems_dot___hash_selFP14_hash_largs x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_splitHtmlElems_dot___hash_selFP14_hash_largs z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_splitHtmlElems_dot___hash_selFP14_hash_largs x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitHtmlElems_dot___hash_selFP15_hash_elems :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP_splitHtmlElems_dot___hash_selFP15_hash_elems x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitHtmlElems_dot___hash_selFP15_hash_elems x1002 x3250 x3500) (d_OP_splitHtmlElems_dot___hash_selFP15_hash_elems x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitHtmlElems_dot___hash_selFP15_hash_elems z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitHtmlElems_dot___hash_selFP15_hash_elems x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_splitHtmlElems_dot___hash_selFP15_hash_elems :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP_splitHtmlElems_dot___hash_selFP15_hash_elems x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_splitHtmlElems_dot___hash_selFP15_hash_elems x1002 x3000 x3250 x3500) (nd_OP_splitHtmlElems_dot___hash_selFP15_hash_elems x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_splitHtmlElems_dot___hash_selFP15_hash_elems z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_splitHtmlElems_dot___hash_selFP15_hash_elems x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitHtmlElems_dot___hash_selFP16_hash_rest :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP_splitHtmlElems_dot___hash_selFP16_hash_rest x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitHtmlElems_dot___hash_selFP16_hash_rest x1002 x3250 x3500) (d_OP_splitHtmlElems_dot___hash_selFP16_hash_rest x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitHtmlElems_dot___hash_selFP16_hash_rest z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitHtmlElems_dot___hash_selFP16_hash_rest x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_splitHtmlElems_dot___hash_selFP16_hash_rest :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP_splitHtmlElems_dot___hash_selFP16_hash_rest x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_splitHtmlElems_dot___hash_selFP16_hash_rest x1002 x3000 x3250 x3500) (nd_OP_splitHtmlElems_dot___hash_selFP16_hash_rest x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_splitHtmlElems_dot___hash_selFP16_hash_rest z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_splitHtmlElems_dot___hash_selFP16_hash_rest x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_scanHtmlString :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_C_scanHtmlString x1 x3250 x3500 = d_OP_scanHtmlString_dot_scanHtml_dot_31 x1 x3250 x3500

d_OP_scanHtmlString_dot_scanHtml_dot_31 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_OP_scanHtmlString_dot_scanHtml_dot_31 x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_18 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '<'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanHtmlString_dot_scanHtml_dot_31 x1002 x3250 x3500) (d_OP_scanHtmlString_dot_scanHtml_dot_31 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanHtmlString_dot_scanHtml_dot_31 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanHtmlString_dot_scanHtml_dot_31 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP18_hash_initxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP18_hash_initxt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP18_hash_initxt x1002 x3250 x3500) (d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP18_hash_initxt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP18_hash_initxt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP18_hash_initxt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP19_hash_remtag :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP19_hash_remtag x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP19_hash_remtag x1002 x3250 x3500) (d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP19_hash_remtag x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP19_hash_remtag z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP19_hash_remtag x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_scanHtmlElem :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_C_scanHtmlElem x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (C_HText (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) x1)) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_15 x3 x4 x1 (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Char '>'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_scanHtmlElem x1 x1002 x3250 x3500) (d_C_scanHtmlElem x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_scanHtmlElem x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_scanHtmlElem x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_scanHtmlElem_dot___hash_selFP21_hash_args :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_scanHtmlElem_dot___hash_selFP21_hash_args x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanHtmlElem_dot___hash_selFP21_hash_args x1002 x3250 x3500) (d_OP_scanHtmlElem_dot___hash_selFP21_hash_args x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanHtmlElem_dot___hash_selFP21_hash_args z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanHtmlElem_dot___hash_selFP21_hash_args x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_scanHtmlElem_dot___hash_selFP22_hash_rest :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_scanHtmlElem_dot___hash_selFP22_hash_rest x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanHtmlElem_dot___hash_selFP22_hash_rest x1002 x3250 x3500) (d_OP_scanHtmlElem_dot___hash_selFP22_hash_rest x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanHtmlElem_dot___hash_selFP22_hash_rest z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanHtmlElem_dot___hash_selFP22_hash_rest x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_scanHtmlComment :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_C_scanHtmlComment x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_10 x3 x2 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '-'#) x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 2#) x3 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_scanHtmlComment x1002 x3250 x3500) (d_C_scanHtmlComment x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_scanHtmlComment z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_scanHtmlComment x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_scanHtmlPre :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_C_scanHtmlPre x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_9 x4 x3 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Char '<'#) x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 5#) (Curry_Prelude.d_C_map Curry_Char.d_C_toLower x4 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_scanHtmlPre x1 x1002 x3250 x3500) (d_C_scanHtmlPre x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_scanHtmlPre x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_scanHtmlPre x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_string2args :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_string2args x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x4 = d_C_splitAtElement Curry_Char.d_C_isSpace (Curry_Prelude.OP_Cons x2 x3) x3250 x3500
          x5 = d_OP_string2args_dot___hash_selFP24_hash_arg1 x4 x3250 x3500
          x6 = d_OP_string2args_dot___hash_selFP25_hash_rest x4 x3250 x3500
           in (Curry_Prelude.OP_Cons (d_C_deleteApo (d_C_splitAtElement (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '='#)) x5 x3250 x3500) x3250 x3500) (d_C_string2args (Curry_Prelude.d_C_dropWhile Curry_Char.d_C_isSpace x6 x3250 x3500) x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_string2args x1002 x3250 x3500) (d_C_string2args x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_string2args z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_string2args x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_string2args_dot___hash_selFP24_hash_arg1 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_string2args_dot___hash_selFP24_hash_arg1 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_string2args_dot___hash_selFP24_hash_arg1 x1002 x3250 x3500) (d_OP_string2args_dot___hash_selFP24_hash_arg1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_string2args_dot___hash_selFP24_hash_arg1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_string2args_dot___hash_selFP24_hash_arg1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_string2args_dot___hash_selFP25_hash_rest :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_string2args_dot___hash_selFP25_hash_rest x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_string2args_dot___hash_selFP25_hash_rest x1002 x3250 x3500) (d_OP_string2args_dot___hash_selFP25_hash_rest x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_string2args_dot___hash_selFP25_hash_rest z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_string2args_dot___hash_selFP25_hash_rest x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_deleteApo :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_deleteApo x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_8 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deleteApo x1002 x3250 x3500) (d_C_deleteApo x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deleteApo z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deleteApo x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_deleteLastApo :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_deleteLastApo x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_4 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deleteLastApo x1002 x3250 x3500) (d_C_deleteLastApo x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deleteLastApo z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deleteLastApo x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_splitAtElement :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
d_C_splitAtElement x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_2 x3 x1 x4 (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_splitAtElement x1 x1002 x3250 x3500) (d_C_splitAtElement x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_splitAtElement x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_splitAtElement x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_splitAtElement :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Bool -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_C_splitAtElement x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_2 x3 x1 x4 (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_splitAtElement x1 x1002 x3000 x3250 x3500) (nd_C_splitAtElement x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_splitAtElement x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_splitAtElement x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitAtElement_dot___hash_selFP27_hash_first :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_splitAtElement_dot___hash_selFP27_hash_first x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitAtElement_dot___hash_selFP27_hash_first x1002 x3250 x3500) (d_OP_splitAtElement_dot___hash_selFP27_hash_first x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitAtElement_dot___hash_selFP27_hash_first z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitAtElement_dot___hash_selFP27_hash_first x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitAtElement_dot___hash_selFP28_hash_rest :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_splitAtElement_dot___hash_selFP28_hash_rest x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitAtElement_dot___hash_selFP28_hash_rest x1002 x3250 x3500) (d_OP_splitAtElement_dot___hash_selFP28_hash_rest x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitAtElement_dot___hash_selFP28_hash_rest z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitAtElement_dot___hash_selFP28_hash_rest x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_skipFirstNewLine :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_skipFirstNewLine x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_1 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\n'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_skipFirstNewLine x1002 x3250 x3500) (d_C_skipFirstNewLine x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_skipFirstNewLine z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_skipFirstNewLine x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_1 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> d_OP__case_0 x2 x3 (Curry_Char.d_C_isSpace x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x2 x3 x1002 x3250 x3500) (d_OP__case_1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_skipFirstNewLine x3 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x2 x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x3 x1002 x3250 x3500) (d_OP__case_0 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.Curry t0 => t0 -> (t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
d_OP__case_2 x3 x1 x4 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x4
     Curry_Prelude.C_False -> let
          x5 = d_C_splitAtElement x1 x4 x3250 x3500
          x6 = d_OP_splitAtElement_dot___hash_selFP27_hash_first x5 x3250 x3500
          x7 = d_OP_splitAtElement_dot___hash_selFP28_hash_rest x5 x3250 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x3 x6) x7)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x3 x1 x4 x1002 x3250 x3500) (d_OP__case_2 x3 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x3 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x3 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_2 :: Curry_Prelude.Curry t0 => t0 -> Func t0 Curry_Prelude.C_Bool -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_OP__case_2 x3 x1 x4 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x4
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (let
               x5 = nd_C_splitAtElement x1 x4 x2000 x3250 x3500
               x6 = d_OP_splitAtElement_dot___hash_selFP27_hash_first x5 x3250 x3500
               x7 = d_OP_splitAtElement_dot___hash_selFP28_hash_rest x5 x3250 x3500
                in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x3 x6) x7)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x3 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_2 x3 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x3 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x3 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_4 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> d_OP__case_3 x2 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '"'#) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.OP_Cons x2 (d_C_deleteLastApo (Curry_Prelude.OP_Cons x4 x5) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x2 x1002 x3250 x3500) (d_OP__case_4 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_3 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x2 x1002 x3250 x3500) (d_OP__case_3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_8 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_map Curry_Char.d_C_toLower x2 x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_7 x4 x5 x2 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x2 x1002 x3250 x3500) (d_OP__case_8 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_7 x4 x5 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_map Curry_Char.d_C_toLower x2 x3250 x3500) (d_C_deleteLastApo x5 x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_6 x4 x5 x2 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x4 x5 x2 x1002 x3250 x3500) (d_OP__case_7 x4 x5 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x4 x5 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x4 x5 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_6 x4 x5 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_map Curry_Char.d_C_toLower x2 x3250 x3500) (d_C_deleteLastApo x5 x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_5 x5 x4 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x4 x5 x2 x1002 x3250 x3500) (d_OP__case_6 x4 x5 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x4 x5 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x4 x5 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_5 x5 x4 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_map Curry_Char.d_C_toLower x2 x3250 x3500) (Curry_Prelude.OP_Cons x4 x5)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x5 x4 x2 x1002 x3250 x3500) (d_OP__case_5 x5 x4 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x5 x4 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x5 x4 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_OP__case_9 x4 x3 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_HElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (C_HText (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) x1 x3250 x3500)) (Curry_Prelude.OP_Cons (C_HElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List) (d_C_scanHtmlString (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 5#) x4 x3250 x3500) x3250 x3500)))
     Curry_Prelude.C_False -> d_C_scanHtmlPre (Curry_Prelude.OP_Cons x3 x1) x4 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x4 x3 x1 x1002 x3250 x3500) (d_OP__case_9 x4 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x4 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x4 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_OP__case_10 x3 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_scanHtmlString (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 2#) x3 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_scanHtmlComment x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x3 x2 x1002 x3250 x3500) (d_OP__case_10 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_OP__case_15 x3 x4 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_HElem x1 Curry_Prelude.OP_List) (d_C_scanHtmlString x4 x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_14 x3 x4 x1 (Curry_Char.d_C_isSpace x3 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x3 x4 x1 x1002 x3250 x3500) (d_OP__case_15 x3 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x3 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x3 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_OP__case_14 x3 x4 x1 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x5 = d_C_splitAtElement (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '>'#)) (Curry_Prelude.d_C_dropWhile Curry_Char.d_C_isSpace x4 x3250 x3500) x3250 x3500
          x6 = d_OP_scanHtmlElem_dot___hash_selFP21_hash_args x5 x3250 x3500
          x7 = d_OP_scanHtmlElem_dot___hash_selFP22_hash_rest x5 x3250 x3500
          x8 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) x6 x3250 x3500
           in (d_OP__case_13 x8 x6 x7 x1 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_null x6 x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_head x8 x3250 x3500) (Curry_Prelude.C_Char '/'#) x3250 x3500) x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_12 x4 x3 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Char '/'#) x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x4 x3250 x3500) (Curry_Prelude.C_Char '>'#) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x3 x4 x1 x1002 x3250 x3500) (d_OP__case_14 x3 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x3 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x3 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_OP__case_12 x4 x3 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_HElem x1 Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (C_HElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) x1) Curry_Prelude.OP_List) (d_C_scanHtmlString (Curry_Prelude.d_C_tail x4 x3250 x3500) x3250 x3500))
     Curry_Prelude.C_False -> d_OP__case_11 x4 x3 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x4 x3 x1 x1002 x3250 x3500) (d_OP__case_12 x4 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x4 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x4 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_OP__case_11 x4 x3 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> d_C_scanHtmlElem (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Char.d_C_toLower x3 x3250 x3500) Curry_Prelude.OP_List) x3250 x3500) x4 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x4 x3 x1 x1002 x3250 x3500) (d_OP__case_11 x4 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x4 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x4 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_OP__case_13 x8 x6 x7 x1 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_HElem x1 (d_C_string2args x6 x3250 x3500)) (d_C_scanHtmlString x7 x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (C_HElem x1 (d_C_string2args (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) (Curry_Prelude.d_C_tail x8 x3250 x3500) x3250 x3500) x3250 x3500)) (Curry_Prelude.OP_Cons (C_HElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) x1) Curry_Prelude.OP_List) (d_C_scanHtmlString x7 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x8 x6 x7 x1 x1002 x3250 x3500) (d_OP__case_13 x8 x6 x7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x8 x6 x7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x8 x6 x7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_OP__case_18 x2 x3 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_OP__case_17 x3 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 3#) x3 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> let
          x4 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '<'#)) x3250 x3500) (Curry_Prelude.OP_Cons x2 x3) x3250 x3500
          x5 = d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP18_hash_initxt x4 x3250 x3500
          x6 = d_OP_scanHtmlString_dot_scanHtml_dot_31_dot___hash_selFP19_hash_remtag x4 x3250 x3500
           in (Curry_Prelude.OP_Cons (C_HText x5) (d_OP_scanHtmlString_dot_scanHtml_dot_31 x6 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x2 x3 x1002 x3250 x3500) (d_OP__case_18 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_OP__case_17 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_scanHtmlComment x3 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_16 x3 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 4#) (Curry_Prelude.d_C_map Curry_Char.d_C_toLower x3 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x3 x1002 x3250 x3500) (d_OP__case_17 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List C_HtmlToken
d_OP__case_16 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_scanHtmlPre Curry_Prelude.OP_List (d_C_skipFirstNewLine (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 4#) x3 x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_scanHtmlElem Curry_Prelude.OP_List x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x3 x1002 x3250 x3500) (d_OP__case_16 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Curry_HTML.C_HtmlExp -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
d_OP__case_21 x1 x4 x3 x3250 x3500 = case x3 of
     (Curry_HTML.C_HtmlText x5) -> let
          x6 = d_C_splitHtmlElems x1 x4 x3250 x3500
          x7 = d_OP_splitHtmlElems_dot___hash_selFP6_hash_largs x6 x3250 x3500
          x8 = d_OP_splitHtmlElems_dot___hash_selFP7_hash_elems x6 x3250 x3500
          x9 = d_OP_splitHtmlElems_dot___hash_selFP8_hash_rest x6 x3250 x3500
           in (Curry_Prelude.OP_Tuple3 x7 (Curry_Prelude.d_OP_plus_plus x8 (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlText x5) Curry_Prelude.OP_List) x3250 x3500) x9)
     (Curry_HTML.C_HtmlStruct x10 x11 x12) -> d_OP__case_20 x10 x1 x4 x11 x12 x3250 x3500
     (Curry_HTML.Choice_C_HtmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x1 x4 x1002 x3250 x3500) (d_OP__case_21 x1 x4 x1003 x3250 x3500)
     (Curry_HTML.Choices_C_HtmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x1 x4 z x3250 x3500) x1002
     (Curry_HTML.Guard_C_HtmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_HTML.Fail_C_HtmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_21 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Curry_HTML.C_HtmlExp -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
nd_OP__case_21 x1 x4 x3 x3000 x3250 x3500 = case x3 of
     (Curry_HTML.C_HtmlText x5) -> let
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
                              x6 = nd_C_splitHtmlElems x1 x4 x2000 x3250 x3500
                              x7 = nd_OP_splitHtmlElems_dot___hash_selFP6_hash_largs x6 x2001 x3250 x3500
                              x8 = nd_OP_splitHtmlElems_dot___hash_selFP7_hash_elems x6 x2002 x3250 x3500
                              x9 = nd_OP_splitHtmlElems_dot___hash_selFP8_hash_rest x6 x2003 x3250 x3500
                               in (Curry_Prelude.OP_Tuple3 x7 (Curry_Prelude.d_OP_plus_plus x8 (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlText x5) Curry_Prelude.OP_List) x3250 x3500) x9))))))))))))
     (Curry_HTML.C_HtmlStruct x10 x11 x12) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_20 x10 x1 x4 x11 x12 x2000 x3250 x3500))
     (Curry_HTML.Choice_C_HtmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_21 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_HTML.Choices_C_HtmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_HTML.Guard_C_HtmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_HTML.Fail_C_HtmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
d_OP__case_20 x10 x1 x4 x11 x12 x3250 x3500 = case x12 of
     (Curry_Prelude.OP_Cons x13 x14) -> let
          x15 = d_C_splitHtmlElems x1 x4 x3250 x3500
          x16 = d_OP_splitHtmlElems_dot___hash_selFP10_hash_largs x15 x3250 x3500
          x17 = d_OP_splitHtmlElems_dot___hash_selFP11_hash_elems x15 x3250 x3500
          x18 = d_OP_splitHtmlElems_dot___hash_selFP12_hash_rest x15 x3250 x3500
           in (Curry_Prelude.OP_Tuple3 x16 (Curry_Prelude.d_OP_plus_plus x17 (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlStruct x10 x11 x12) Curry_Prelude.OP_List) x3250 x3500) x18)
     Curry_Prelude.OP_List -> d_OP__case_19 x10 x1 x4 x11 (Curry_Prelude.d_OP_eq_eq x1 x10 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x10 x1 x4 x11 x1002 x3250 x3500) (d_OP__case_20 x10 x1 x4 x11 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x10 x1 x4 x11 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x10 x1 x4 x11 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_20 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
nd_OP__case_20 x10 x1 x4 x11 x12 x3000 x3250 x3500 = case x12 of
     (Curry_Prelude.OP_Cons x13 x14) -> let
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
                              x15 = nd_C_splitHtmlElems x1 x4 x2000 x3250 x3500
                              x16 = nd_OP_splitHtmlElems_dot___hash_selFP10_hash_largs x15 x2001 x3250 x3500
                              x17 = nd_OP_splitHtmlElems_dot___hash_selFP11_hash_elems x15 x2002 x3250 x3500
                              x18 = nd_OP_splitHtmlElems_dot___hash_selFP12_hash_rest x15 x2003 x3250 x3500
                               in (Curry_Prelude.OP_Tuple3 x16 (Curry_Prelude.d_OP_plus_plus x17 (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlStruct x10 x11 x12) Curry_Prelude.OP_List) x3250 x3500) x18))))))))))))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_19 x10 x1 x4 x11 (Curry_Prelude.d_OP_eq_eq x1 x10 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x10 x1 x4 x11 x1002 x3000 x3250 x3500) (nd_OP__case_20 x10 x1 x4 x11 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x10 x1 x4 x11 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x10 x1 x4 x11 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
d_OP__case_19 x10 x1 x4 x11 x23 x3250 x3500 = case x23 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple3 x11 Curry_Prelude.OP_List x4
     Curry_Prelude.C_False -> let
          x19 = d_C_splitHtmlElems x1 x4 x3250 x3500
          x20 = d_OP_splitHtmlElems_dot___hash_selFP14_hash_largs x19 x3250 x3500
          x21 = d_OP_splitHtmlElems_dot___hash_selFP15_hash_elems x19 x3250 x3500
          x22 = d_OP_splitHtmlElems_dot___hash_selFP16_hash_rest x19 x3250 x3500
           in (Curry_Prelude.OP_Tuple3 x20 (Curry_Prelude.d_OP_plus_plus x21 (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlStruct x10 x11 Curry_Prelude.OP_List) Curry_Prelude.OP_List) x3250 x3500) x22)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x10 x1 x4 x11 x1002 x3250 x3500) (d_OP__case_19 x10 x1 x4 x11 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x10 x1 x4 x11 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x10 x1 x4 x11 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_19 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
nd_OP__case_19 x10 x1 x4 x11 x23 x3000 x3250 x3500 = case x23 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple3 x11 Curry_Prelude.OP_List x4
     Curry_Prelude.C_False -> let
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
                              x19 = nd_C_splitHtmlElems x1 x4 x2000 x3250 x3500
                              x20 = nd_OP_splitHtmlElems_dot___hash_selFP14_hash_largs x19 x2001 x3250 x3500
                              x21 = nd_OP_splitHtmlElems_dot___hash_selFP15_hash_elems x19 x2002 x3250 x3500
                              x22 = nd_OP_splitHtmlElems_dot___hash_selFP16_hash_rest x19 x2003 x3250 x3500
                               in (Curry_Prelude.OP_Tuple3 x20 (Curry_Prelude.d_OP_plus_plus x21 (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlStruct x10 x11 Curry_Prelude.OP_List) Curry_Prelude.OP_List) x3250 x3500) x22))))))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x10 x1 x4 x11 x1002 x3000 x3250 x3500) (nd_OP__case_19 x10 x1 x4 x11 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x10 x1 x4 x11 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x10 x1 x4 x11 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.OP_List C_HtmlToken -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> C_HtmlToken -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_24 x4 x1 x3 x3250 x3500 = case x3 of
     (C_HText x5) -> d_C_parseHtmlTokens (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlText x5) x1) x4 x3250 x3500
     (C_HElem x6 x7) -> d_OP__case_23 x4 x1 x7 x6 x3250 x3500
     (Choice_C_HtmlToken x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x4 x1 x1002 x3250 x3500) (d_OP__case_24 x4 x1 x1003 x3250 x3500)
     (Choices_C_HtmlToken x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x4 x1 z x3250 x3500) x1002
     (Guard_C_HtmlToken x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_HtmlToken x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_24 :: Curry_Prelude.OP_List C_HtmlToken -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> C_HtmlToken -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_24 x4 x1 x3 x3000 x3250 x3500 = case x3 of
     (C_HText x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_parseHtmlTokens (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlText x5) x1) x4 x2000 x3250 x3500))
     (C_HElem x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_23 x4 x1 x7 x6 x2000 x3250 x3500))
     (Choice_C_HtmlToken x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x4 x1 x1002 x3000 x3250 x3500) (nd_OP__case_24 x4 x1 x1003 x3000 x3250 x3500)
     (Choices_C_HtmlToken x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x4 x1 z x3000 x3250 x3500) x1002
     (Guard_C_HtmlToken x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x4 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_HtmlToken x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.OP_List C_HtmlToken -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_23 x4 x1 x7 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_22 x8 x4 x1 x7 x9 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char '/'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x4 x1 x7 x1002 x3250 x3500) (d_OP__case_23 x4 x1 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x4 x1 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x4 x1 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_23 :: Curry_Prelude.OP_List C_HtmlToken -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_23 x4 x1 x7 x6 x3000 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_22 x8 x4 x1 x7 x9 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char '/'#) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x4 x1 x7 x1002 x3000 x3250 x3500) (nd_OP__case_23 x4 x1 x7 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x4 x1 x7 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x4 x1 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_HtmlToken -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP__case_22 x8 x4 x1 x7 x9 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> let
          x10 = d_C_splitHtmlElems x9 x1 x3250 x3500
          x11 = d_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs x10 x3250 x3500
          x12 = d_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems x10 x3250 x3500
          x13 = d_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest x10 x3250 x3500
           in (d_C_parseHtmlTokens (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlStruct x9 x11 x12) Curry_Prelude.OP_List) x13 x3250 x3500) x4 x3250 x3500)
     Curry_Prelude.C_False -> d_C_parseHtmlTokens (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlStruct (Curry_Prelude.OP_Cons x8 x9) x7 Curry_Prelude.OP_List) x1) x4 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x8 x4 x1 x7 x9 x1002 x3250 x3500) (d_OP__case_22 x8 x4 x1 x7 x9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x8 x4 x1 x7 x9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x8 x4 x1 x7 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_22 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_HtmlToken -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP__case_22 x8 x4 x1 x7 x9 x14 x3000 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> let
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
                               in (seq x2003 (seq x2004 (let
                                   x10 = nd_C_splitHtmlElems x9 x1 x2000 x3250 x3500
                                   x11 = nd_OP_parseHtmlTokens_dot___hash_selFP2_hash_structargs x10 x2001 x3250 x3500
                                   x12 = nd_OP_parseHtmlTokens_dot___hash_selFP3_hash_elems x10 x2002 x3250 x3500
                                   x13 = nd_OP_parseHtmlTokens_dot___hash_selFP4_hash_rest x10 x2003 x3250 x3500
                                    in (nd_C_parseHtmlTokens (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlStruct x9 x11 x12) Curry_Prelude.OP_List) x13 x3250 x3500) x4 x2004 x3250 x3500)))))))))))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_parseHtmlTokens (Curry_Prelude.OP_Cons (Curry_HTML.C_HtmlStruct (Curry_Prelude.OP_Cons x8 x9) x7 Curry_Prelude.OP_List) x1) x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x8 x4 x1 x7 x9 x1002 x3000 x3250 x3500) (nd_OP__case_22 x8 x4 x1 x7 x9 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x8 x4 x1 x7 x9 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x8 x4 x1 x7 x9 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
