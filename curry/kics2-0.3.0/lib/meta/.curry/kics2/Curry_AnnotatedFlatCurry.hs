{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AnnotatedFlatCurry (C_AProg (..), C_AFuncDecl (..), C_ARule (..), C_AExpr (..), C_ABranchExpr (..), C_APattern (..), C_Arity) where

import Basics
import qualified Curry_FlatCurry
import qualified Curry_Prelude
type C_Arity = Curry_Prelude.C_Int

data C_AProg t0
     = C_AProg (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List (C_AFuncDecl t0)) (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl)
     | Choice_C_AProg Cover ID (C_AProg t0) (C_AProg t0)
     | Choices_C_AProg Cover ID ([C_AProg t0])
     | Fail_C_AProg Cover FailInfo
     | Guard_C_AProg Cover Constraints (C_AProg t0)

instance Show t0 => Show (C_AProg t0) where
  showsPrec d (Choice_C_AProg cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_AProg cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_AProg cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_AProg cd info) = showChar '!'
  showsPrec _ (C_AProg x1 x2 x3 x4 x5) = (showString "(AProg") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . (showChar ')')))))))))))


instance Read t0 => Read (C_AProg t0) where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_AProg x1 x2 x3 x4 x5,r5) | (_,r0) <- readQualified "AnnotatedFlatCurry" "AProg" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4]) s


instance NonDet (C_AProg t0) where
  choiceCons = Choice_C_AProg
  choicesCons = Choices_C_AProg
  failCons = Fail_C_AProg
  guardCons = Guard_C_AProg
  try (Choice_C_AProg cd i x y) = tryChoice cd i x y
  try (Choices_C_AProg cd i xs) = tryChoices cd i xs
  try (Fail_C_AProg cd info) = Fail cd info
  try (Guard_C_AProg cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_AProg cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_AProg cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_AProg cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_AProg cd i _) = error ("AnnotatedFlatCurry.AProg.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_AProg cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_AProg cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_AProg t0) where
  generate s c = Choices_C_AProg c (freeID [5] s) [(C_AProg (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm t0 => NormalForm (C_AProg t0) where
  ($!!) cont (C_AProg x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_AProg y1 y2 y3 y4 y5) d cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_AProg cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_AProg cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_AProg cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_AProg cd info) _ _ = failCons cd info
  ($##) cont (C_AProg x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_AProg y1 y2 y3 y4 y5) d cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_AProg cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_AProg cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_AProg cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_AProg cd info) _ _ = failCons cd info
  searchNF search cont (C_AProg x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (C_AProg y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("AnnotatedFlatCurry.AProg.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_AProg t0) where
  (=.=) (C_AProg x1 x2 x3 x4 x5) (C_AProg y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_AProg x1 x2 x3 x4 x5) (C_AProg y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_AProg x3 x4 x5 x6 x7) = ((i :=: (ChooseN 0 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind d i (Choice_C_AProg cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_AProg cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_AProg cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_AProg cd i _) = error ("AnnotatedFlatCurry.AProg.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_AProg cd info) = [(Unsolvable info)]
  bind d i (Guard_C_AProg cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_AProg x3 x4 x5 x6 x7) = [(i :=: (ChooseN 0 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind d i (Choice_C_AProg cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_AProg cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_AProg cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_AProg cd i _) = error ("AnnotatedFlatCurry.AProg.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_AProg cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_AProg cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_AProg t0) where
  (=?=) (Choice_C_AProg cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_AProg cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_AProg cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_AProg cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_AProg cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_AProg cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_AProg cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_AProg cd info) _ _ = failCons cd info
  (=?=) (C_AProg x1 x2 x3 x4 x5) (C_AProg y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.=?= y5) d) cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_C_AProg cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_AProg cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_AProg cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_AProg cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_AProg cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_AProg cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_AProg cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_AProg cd info) _ _ = failCons cd info
  (<?=) (C_AProg x1 x2 x3 x4 x5) (C_AProg y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.<?= y5) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data C_AFuncDecl t0
     = C_AFunc (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int Curry_FlatCurry.C_Visibility Curry_FlatCurry.C_TypeExpr (C_ARule t0)
     | Choice_C_AFuncDecl Cover ID (C_AFuncDecl t0) (C_AFuncDecl t0)
     | Choices_C_AFuncDecl Cover ID ([C_AFuncDecl t0])
     | Fail_C_AFuncDecl Cover FailInfo
     | Guard_C_AFuncDecl Cover Constraints (C_AFuncDecl t0)

instance Show t0 => Show (C_AFuncDecl t0) where
  showsPrec d (Choice_C_AFuncDecl cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_AFuncDecl cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_AFuncDecl cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_AFuncDecl cd info) = showChar '!'
  showsPrec _ (C_AFunc x1 x2 x3 x4 x5) = (showString "(AFunc") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . (showChar ')')))))))))))


instance Read t0 => Read (C_AFuncDecl t0) where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_AFunc x1 x2 x3 x4 x5,r5) | (_,r0) <- readQualified "AnnotatedFlatCurry" "AFunc" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4]) s


instance NonDet (C_AFuncDecl t0) where
  choiceCons = Choice_C_AFuncDecl
  choicesCons = Choices_C_AFuncDecl
  failCons = Fail_C_AFuncDecl
  guardCons = Guard_C_AFuncDecl
  try (Choice_C_AFuncDecl cd i x y) = tryChoice cd i x y
  try (Choices_C_AFuncDecl cd i xs) = tryChoices cd i xs
  try (Fail_C_AFuncDecl cd info) = Fail cd info
  try (Guard_C_AFuncDecl cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_AFuncDecl cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_AFuncDecl cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_AFuncDecl cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_AFuncDecl cd i _) = error ("AnnotatedFlatCurry.AFuncDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_AFuncDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_AFuncDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_AFuncDecl t0) where
  generate s c = Choices_C_AFuncDecl c (freeID [5] s) [(C_AFunc (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm t0 => NormalForm (C_AFuncDecl t0) where
  ($!!) cont (C_AFunc x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_AFunc y1 y2 y3 y4 y5) d cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_AFuncDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_AFuncDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_AFuncDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_AFuncDecl cd info) _ _ = failCons cd info
  ($##) cont (C_AFunc x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_AFunc y1 y2 y3 y4 y5) d cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_AFuncDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_AFuncDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_AFuncDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_AFuncDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_AFunc x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (C_AFunc y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("AnnotatedFlatCurry.AFuncDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_AFuncDecl t0) where
  (=.=) (C_AFunc x1 x2 x3 x4 x5) (C_AFunc y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_AFunc x1 x2 x3 x4 x5) (C_AFunc y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_AFunc x3 x4 x5 x6 x7) = ((i :=: (ChooseN 0 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind d i (Choice_C_AFuncDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_AFuncDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_AFuncDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_AFuncDecl cd i _) = error ("AnnotatedFlatCurry.AFuncDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_AFuncDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_AFuncDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_AFunc x3 x4 x5 x6 x7) = [(i :=: (ChooseN 0 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind d i (Choice_C_AFuncDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_AFuncDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_AFuncDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_AFuncDecl cd i _) = error ("AnnotatedFlatCurry.AFuncDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_AFuncDecl cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_AFuncDecl cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_AFuncDecl t0) where
  (=?=) (Choice_C_AFuncDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_AFuncDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_AFuncDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_AFuncDecl cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_AFuncDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_AFuncDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_AFuncDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_AFuncDecl cd info) _ _ = failCons cd info
  (=?=) (C_AFunc x1 x2 x3 x4 x5) (C_AFunc y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.=?= y5) d) cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_C_AFuncDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_AFuncDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_AFuncDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_AFuncDecl cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_AFuncDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_AFuncDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_AFuncDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_AFuncDecl cd info) _ _ = failCons cd info
  (<?=) (C_AFunc x1 x2 x3 x4 x5) (C_AFunc y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.<?= y5) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data C_ARule t0
     = C_ARule t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0)) (C_AExpr t0)
     | C_AExternal t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_ARule Cover ID (C_ARule t0) (C_ARule t0)
     | Choices_C_ARule Cover ID ([C_ARule t0])
     | Fail_C_ARule Cover FailInfo
     | Guard_C_ARule Cover Constraints (C_ARule t0)

instance Show t0 => Show (C_ARule t0) where
  showsPrec d (Choice_C_ARule cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ARule cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ARule cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ARule cd info) = showChar '!'
  showsPrec _ (C_ARule x1 x2 x3) = (showString "(ARule") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_AExternal x1 x2) = (showString "(AExternal") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read t0 => Read (C_ARule t0) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_ARule x1 x2 x3,r3) | (_,r0) <- readQualified "AnnotatedFlatCurry" "ARule" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ (readParen (d > 10) (\r -> [ (C_AExternal x1 x2,r2) | (_,r0) <- readQualified "AnnotatedFlatCurry" "AExternal" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)


instance NonDet (C_ARule t0) where
  choiceCons = Choice_C_ARule
  choicesCons = Choices_C_ARule
  failCons = Fail_C_ARule
  guardCons = Guard_C_ARule
  try (Choice_C_ARule cd i x y) = tryChoice cd i x y
  try (Choices_C_ARule cd i xs) = tryChoices cd i xs
  try (Fail_C_ARule cd info) = Fail cd info
  try (Guard_C_ARule cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ARule cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ARule cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ARule cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ARule cd i _) = error ("AnnotatedFlatCurry.ARule.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ARule cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ARule cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_ARule t0) where
  generate s c = Choices_C_ARule c (freeID [3,2] s) [(C_ARule (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_AExternal (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm t0 => NormalForm (C_ARule t0) where
  ($!!) cont (C_ARule x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_ARule y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_AExternal x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_AExternal y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_ARule cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_ARule cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_ARule cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_ARule cd info) _ _ = failCons cd info
  ($##) cont (C_ARule x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_ARule y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_AExternal x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_AExternal y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_ARule cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_ARule cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_ARule cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_ARule cd info) _ _ = failCons cd info
  searchNF search cont (C_ARule x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_ARule y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_AExternal x1 x2) = search (\y1 -> search (\y2 -> cont (C_AExternal y1 y2)) x2) x1
  searchNF _ _ x = error ("AnnotatedFlatCurry.ARule.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_ARule t0) where
  (=.=) (C_ARule x1 x2 x3) (C_ARule y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_AExternal x1 x2) (C_AExternal y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_ARule x1 x2 x3) (C_ARule y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_AExternal x1 x2) (C_AExternal y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_ARule x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_AExternal x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_ARule cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_ARule cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_ARule cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_ARule cd i _) = error ("AnnotatedFlatCurry.ARule.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_ARule cd info) = [(Unsolvable info)]
  bind d i (Guard_C_ARule cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_ARule x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_AExternal x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_ARule cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_ARule cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_ARule cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_ARule cd i _) = error ("AnnotatedFlatCurry.ARule.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_ARule cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_ARule cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_ARule t0) where
  (=?=) (Choice_C_ARule cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_ARule cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_ARule cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_ARule cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_ARule cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_ARule cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_ARule cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_ARule cd info) _ _ = failCons cd info
  (=?=) (C_ARule x1 x2 x3) (C_ARule y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_AExternal x1 x2) (C_AExternal y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_ARule cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_ARule cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_ARule cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_ARule cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_ARule cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_ARule cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_ARule cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_ARule cd info) _ _ = failCons cd info
  (<?=) (C_ARule x1 x2 x3) (C_ARule y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_ARule _ _ _) (C_AExternal _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AExternal x1 x2) (C_AExternal y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_AExpr t0
     = C_AVar t0 Curry_Prelude.C_Int
     | C_ALit t0 Curry_FlatCurry.C_Literal
     | C_AComb t0 Curry_FlatCurry.C_CombType (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) (Curry_Prelude.OP_List (C_AExpr t0))
     | C_ALet t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (C_AExpr t0))) (C_AExpr t0)
     | C_AFree t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0)) (C_AExpr t0)
     | C_AOr t0 (C_AExpr t0) (C_AExpr t0)
     | C_ACase t0 Curry_FlatCurry.C_CaseType (C_AExpr t0) (Curry_Prelude.OP_List (C_ABranchExpr t0))
     | C_ATyped t0 (C_AExpr t0) Curry_FlatCurry.C_TypeExpr
     | Choice_C_AExpr Cover ID (C_AExpr t0) (C_AExpr t0)
     | Choices_C_AExpr Cover ID ([C_AExpr t0])
     | Fail_C_AExpr Cover FailInfo
     | Guard_C_AExpr Cover Constraints (C_AExpr t0)

instance Show t0 => Show (C_AExpr t0) where
  showsPrec d (Choice_C_AExpr cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_AExpr cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_AExpr cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_AExpr cd info) = showChar '!'
  showsPrec _ (C_AVar x1 x2) = (showString "(AVar") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_ALit x1 x2) = (showString "(ALit") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_AComb x1 x2 x3 x4) = (showString "(AComb") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))
  showsPrec _ (C_ALet x1 x2 x3) = (showString "(ALet") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_AFree x1 x2 x3) = (showString "(AFree") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_AOr x1 x2 x3) = (showString "(AOr") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_ACase x1 x2 x3 x4) = (showString "(ACase") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))
  showsPrec _ (C_ATyped x1 x2 x3) = (showString "(ATyped") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read t0 => Read (C_AExpr t0) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_AVar x1 x2,r2) | (_,r0) <- readQualified "AnnotatedFlatCurry" "AVar" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_ALit x1 x2,r2) | (_,r0) <- readQualified "AnnotatedFlatCurry" "ALit" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_AComb x1 x2 x3 x4,r4) | (_,r0) <- readQualified "AnnotatedFlatCurry" "AComb" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s) ++ ((readParen (d > 10) (\r -> [ (C_ALet x1 x2 x3,r3) | (_,r0) <- readQualified "AnnotatedFlatCurry" "ALet" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ ((readParen (d > 10) (\r -> [ (C_AFree x1 x2 x3,r3) | (_,r0) <- readQualified "AnnotatedFlatCurry" "AFree" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ ((readParen (d > 10) (\r -> [ (C_AOr x1 x2 x3,r3) | (_,r0) <- readQualified "AnnotatedFlatCurry" "AOr" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ ((readParen (d > 10) (\r -> [ (C_ACase x1 x2 x3 x4,r4) | (_,r0) <- readQualified "AnnotatedFlatCurry" "ACase" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s) ++ (readParen (d > 10) (\r -> [ (C_ATyped x1 x2 x3,r3) | (_,r0) <- readQualified "AnnotatedFlatCurry" "ATyped" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s)))))))


instance NonDet (C_AExpr t0) where
  choiceCons = Choice_C_AExpr
  choicesCons = Choices_C_AExpr
  failCons = Fail_C_AExpr
  guardCons = Guard_C_AExpr
  try (Choice_C_AExpr cd i x y) = tryChoice cd i x y
  try (Choices_C_AExpr cd i xs) = tryChoices cd i xs
  try (Fail_C_AExpr cd info) = Fail cd info
  try (Guard_C_AExpr cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_AExpr cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_AExpr cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_AExpr cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_AExpr cd i _) = error ("AnnotatedFlatCurry.AExpr.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_AExpr cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_AExpr cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_AExpr t0) where
  generate s c = Choices_C_AExpr c (freeID [2,2,4,3,3,3,4,3] s) [(C_AVar (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_ALit (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_AComb (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c)),(C_ALet (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_AFree (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_AOr (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_ACase (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c)),(C_ATyped (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance NormalForm t0 => NormalForm (C_AExpr t0) where
  ($!!) cont (C_AVar x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_AVar y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_ALit x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ALit y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_AComb x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_AComb y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_ALet x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_ALet y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_AFree x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AFree y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_AOr x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AOr y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_ACase x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_ACase y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_ATyped x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_ATyped y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_AExpr cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_AExpr cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_AExpr cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_AExpr cd info) _ _ = failCons cd info
  ($##) cont (C_AVar x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_AVar y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_ALit x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ALit y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_AComb x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_AComb y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_ALet x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_ALet y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_AFree x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AFree y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_AOr x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AOr y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_ACase x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_ACase y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_ATyped x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_ATyped y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_AExpr cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_AExpr cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_AExpr cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_AExpr cd info) _ _ = failCons cd info
  searchNF search cont (C_AVar x1 x2) = search (\y1 -> search (\y2 -> cont (C_AVar y1 y2)) x2) x1
  searchNF search cont (C_ALit x1 x2) = search (\y1 -> search (\y2 -> cont (C_ALit y1 y2)) x2) x1
  searchNF search cont (C_AComb x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_AComb y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF search cont (C_ALet x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_ALet y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_AFree x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_AFree y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_AOr x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_AOr y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_ACase x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_ACase y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF search cont (C_ATyped x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_ATyped y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("AnnotatedFlatCurry.AExpr.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_AExpr t0) where
  (=.=) (C_AVar x1 x2) (C_AVar y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_ALit x1 x2) (C_ALit y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_AComb x1 x2 x3 x4) (C_AComb y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (C_ALet x1 x2 x3) (C_ALet y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_AFree x1 x2 x3) (C_AFree y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_AOr x1 x2 x3) (C_AOr y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_ACase x1 x2 x3 x4) (C_ACase y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (C_ATyped x1 x2 x3) (C_ATyped y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_AVar x1 x2) (C_AVar y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_ALit x1 x2) (C_ALit y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_AComb x1 x2 x3 x4) (C_AComb y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (C_ALet x1 x2 x3) (C_ALet y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_AFree x1 x2 x3) (C_AFree y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_AOr x1 x2 x3) (C_AOr y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_ACase x1 x2 x3 x4) (C_ACase y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (C_ATyped x1 x2 x3) (C_ATyped y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_AVar x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_ALit x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_AComb x3 x4 x5 x6) = ((i :=: (ChooseN 2 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind cd i (C_ALet x3 x4 x5) = ((i :=: (ChooseN 3 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_AFree x3 x4 x5) = ((i :=: (ChooseN 4 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_AOr x3 x4 x5) = ((i :=: (ChooseN 5 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_ACase x3 x4 x5 x6) = ((i :=: (ChooseN 6 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind cd i (C_ATyped x3 x4 x5) = ((i :=: (ChooseN 7 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_AExpr cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_AExpr cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_AExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_AExpr cd i _) = error ("AnnotatedFlatCurry.AExpr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_AExpr cd info) = [(Unsolvable info)]
  bind d i (Guard_C_AExpr cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_AVar x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_ALit x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_AComb x3 x4 x5 x6) = [(i :=: (ChooseN 2 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind cd i (C_ALet x3 x4 x5) = [(i :=: (ChooseN 3 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_AFree x3 x4 x5) = [(i :=: (ChooseN 4 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_AOr x3 x4 x5) = [(i :=: (ChooseN 5 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_ACase x3 x4 x5 x6) = [(i :=: (ChooseN 6 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind cd i (C_ATyped x3 x4 x5) = [(i :=: (ChooseN 7 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_AExpr cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_AExpr cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_AExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_AExpr cd i _) = error ("AnnotatedFlatCurry.AExpr.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_AExpr cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_AExpr cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_AExpr t0) where
  (=?=) (Choice_C_AExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_AExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_AExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_AExpr cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_AExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_AExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_AExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_AExpr cd info) _ _ = failCons cd info
  (=?=) (C_AVar x1 x2) (C_AVar y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_ALit x1 x2) (C_ALit y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_AComb x1 x2 x3 x4) (C_AComb y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (=?=) (C_ALet x1 x2 x3) (C_ALet y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_AFree x1 x2 x3) (C_AFree y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_AOr x1 x2 x3) (C_AOr y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_ACase x1 x2 x3 x4) (C_ACase y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (=?=) (C_ATyped x1 x2 x3) (C_ATyped y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_AExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_AExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_AExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_AExpr cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_AExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_AExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_AExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_AExpr cd info) _ _ = failCons cd info
  (<?=) (C_AVar x1 x2) (C_AVar y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_AVar _ _) (C_ALit _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AVar _ _) (C_AComb _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AVar _ _) (C_ALet _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AVar _ _) (C_AFree _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AVar _ _) (C_AOr _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AVar _ _) (C_ACase _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AVar _ _) (C_ATyped _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ALit x1 x2) (C_ALit y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_ALit _ _) (C_AComb _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ALit _ _) (C_ALet _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ALit _ _) (C_AFree _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ALit _ _) (C_AOr _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ALit _ _) (C_ACase _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ALit _ _) (C_ATyped _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AComb x1 x2 x3 x4) (C_AComb y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (C_AComb _ _ _ _) (C_ALet _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AComb _ _ _ _) (C_AFree _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AComb _ _ _ _) (C_AOr _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AComb _ _ _ _) (C_ACase _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AComb _ _ _ _) (C_ATyped _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ALet x1 x2 x3) (C_ALet y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_ALet _ _ _) (C_AFree _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ALet _ _ _) (C_AOr _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ALet _ _ _) (C_ACase _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ALet _ _ _) (C_ATyped _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AFree x1 x2 x3) (C_AFree y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_AFree _ _ _) (C_AOr _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AFree _ _ _) (C_ACase _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AFree _ _ _) (C_ATyped _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AOr x1 x2 x3) (C_AOr y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_AOr _ _ _) (C_ACase _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AOr _ _ _) (C_ATyped _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ACase x1 x2 x3 x4) (C_ACase y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (C_ACase _ _ _ _) (C_ATyped _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ATyped x1 x2 x3) (C_ATyped y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_ABranchExpr t0
     = C_ABranch (C_APattern t0) (C_AExpr t0)
     | Choice_C_ABranchExpr Cover ID (C_ABranchExpr t0) (C_ABranchExpr t0)
     | Choices_C_ABranchExpr Cover ID ([C_ABranchExpr t0])
     | Fail_C_ABranchExpr Cover FailInfo
     | Guard_C_ABranchExpr Cover Constraints (C_ABranchExpr t0)

instance Show t0 => Show (C_ABranchExpr t0) where
  showsPrec d (Choice_C_ABranchExpr cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ABranchExpr cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ABranchExpr cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ABranchExpr cd info) = showChar '!'
  showsPrec _ (C_ABranch x1 x2) = (showString "(ABranch") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read t0 => Read (C_ABranchExpr t0) where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_ABranch x1 x2,r2) | (_,r0) <- readQualified "AnnotatedFlatCurry" "ABranch" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s


instance NonDet (C_ABranchExpr t0) where
  choiceCons = Choice_C_ABranchExpr
  choicesCons = Choices_C_ABranchExpr
  failCons = Fail_C_ABranchExpr
  guardCons = Guard_C_ABranchExpr
  try (Choice_C_ABranchExpr cd i x y) = tryChoice cd i x y
  try (Choices_C_ABranchExpr cd i xs) = tryChoices cd i xs
  try (Fail_C_ABranchExpr cd info) = Fail cd info
  try (Guard_C_ABranchExpr cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ABranchExpr cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ABranchExpr cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ABranchExpr cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ABranchExpr cd i _) = error ("AnnotatedFlatCurry.ABranchExpr.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ABranchExpr cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ABranchExpr cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_ABranchExpr t0) where
  generate s c = Choices_C_ABranchExpr c (freeID [2] s) [(C_ABranch (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm t0 => NormalForm (C_ABranchExpr t0) where
  ($!!) cont (C_ABranch x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ABranch y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_ABranchExpr cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_ABranchExpr cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_ABranchExpr cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_ABranchExpr cd info) _ _ = failCons cd info
  ($##) cont (C_ABranch x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ABranch y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_ABranchExpr cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_ABranchExpr cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_ABranchExpr cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_ABranchExpr cd info) _ _ = failCons cd info
  searchNF search cont (C_ABranch x1 x2) = search (\y1 -> search (\y2 -> cont (C_ABranch y1 y2)) x2) x1
  searchNF _ _ x = error ("AnnotatedFlatCurry.ABranchExpr.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_ABranchExpr t0) where
  (=.=) (C_ABranch x1 x2) (C_ABranch y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_ABranch x1 x2) (C_ABranch y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_ABranch x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_ABranchExpr cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_ABranchExpr cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_ABranchExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_ABranchExpr cd i _) = error ("AnnotatedFlatCurry.ABranchExpr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_ABranchExpr cd info) = [(Unsolvable info)]
  bind d i (Guard_C_ABranchExpr cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_ABranch x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_ABranchExpr cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_ABranchExpr cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_ABranchExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_ABranchExpr cd i _) = error ("AnnotatedFlatCurry.ABranchExpr.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_ABranchExpr cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_ABranchExpr cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_ABranchExpr t0) where
  (=?=) (Choice_C_ABranchExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_ABranchExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_ABranchExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_ABranchExpr cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_ABranchExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_ABranchExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_ABranchExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_ABranchExpr cd info) _ _ = failCons cd info
  (=?=) (C_ABranch x1 x2) (C_ABranch y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (<?=) (Choice_C_ABranchExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_ABranchExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_ABranchExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_ABranchExpr cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_ABranchExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_ABranchExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_ABranchExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_ABranchExpr cd info) _ _ = failCons cd info
  (<?=) (C_ABranch x1 x2) (C_ABranch y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs


data C_APattern t0
     = C_APattern t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0))
     | C_ALPattern t0 Curry_FlatCurry.C_Literal
     | Choice_C_APattern Cover ID (C_APattern t0) (C_APattern t0)
     | Choices_C_APattern Cover ID ([C_APattern t0])
     | Fail_C_APattern Cover FailInfo
     | Guard_C_APattern Cover Constraints (C_APattern t0)

instance Show t0 => Show (C_APattern t0) where
  showsPrec d (Choice_C_APattern cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_APattern cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_APattern cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_APattern cd info) = showChar '!'
  showsPrec _ (C_APattern x1 x2 x3) = (showString "(APattern") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_ALPattern x1 x2) = (showString "(ALPattern") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read t0 => Read (C_APattern t0) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_APattern x1 x2 x3,r3) | (_,r0) <- readQualified "AnnotatedFlatCurry" "APattern" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ (readParen (d > 10) (\r -> [ (C_ALPattern x1 x2,r2) | (_,r0) <- readQualified "AnnotatedFlatCurry" "ALPattern" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)


instance NonDet (C_APattern t0) where
  choiceCons = Choice_C_APattern
  choicesCons = Choices_C_APattern
  failCons = Fail_C_APattern
  guardCons = Guard_C_APattern
  try (Choice_C_APattern cd i x y) = tryChoice cd i x y
  try (Choices_C_APattern cd i xs) = tryChoices cd i xs
  try (Fail_C_APattern cd info) = Fail cd info
  try (Guard_C_APattern cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_APattern cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_APattern cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_APattern cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_APattern cd i _) = error ("AnnotatedFlatCurry.APattern.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_APattern cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_APattern cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_APattern t0) where
  generate s c = Choices_C_APattern c (freeID [3,2] s) [(C_APattern (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_ALPattern (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm t0 => NormalForm (C_APattern t0) where
  ($!!) cont (C_APattern x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_APattern y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_ALPattern x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ALPattern y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_APattern cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_APattern cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_APattern cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_APattern cd info) _ _ = failCons cd info
  ($##) cont (C_APattern x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_APattern y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_ALPattern x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ALPattern y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_APattern cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_APattern cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_APattern cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_APattern cd info) _ _ = failCons cd info
  searchNF search cont (C_APattern x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_APattern y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_ALPattern x1 x2) = search (\y1 -> search (\y2 -> cont (C_ALPattern y1 y2)) x2) x1
  searchNF _ _ x = error ("AnnotatedFlatCurry.APattern.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_APattern t0) where
  (=.=) (C_APattern x1 x2 x3) (C_APattern y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_ALPattern x1 x2) (C_ALPattern y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_APattern x1 x2 x3) (C_APattern y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_ALPattern x1 x2) (C_ALPattern y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_APattern x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_ALPattern x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_APattern cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_APattern cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_APattern cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_APattern cd i _) = error ("AnnotatedFlatCurry.APattern.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_APattern cd info) = [(Unsolvable info)]
  bind d i (Guard_C_APattern cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_APattern x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_ALPattern x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_APattern cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_APattern cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_APattern cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_APattern cd i _) = error ("AnnotatedFlatCurry.APattern.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_APattern cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_APattern cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_APattern t0) where
  (=?=) (Choice_C_APattern cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_APattern cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_APattern cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_APattern cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_APattern cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_APattern cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_APattern cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_APattern cd info) _ _ = failCons cd info
  (=?=) (C_APattern x1 x2 x3) (C_APattern y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_ALPattern x1 x2) (C_ALPattern y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_APattern cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_APattern cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_APattern cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_APattern cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_APattern cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_APattern cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_APattern cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_APattern cd info) _ _ = failCons cd info
  (<?=) (C_APattern x1 x2 x3) (C_APattern y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_APattern _ _ _) (C_ALPattern _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ALPattern x1 x2) (C_ALPattern y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False

