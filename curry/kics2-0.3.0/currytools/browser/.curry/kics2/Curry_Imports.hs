{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Imports (C_InterfaceOrFlatProg (..), d_C_getImportedInterfaces, d_C_moduleImports, d_C_ifOrProg, nd_C_ifOrProg, d_C_progOfIFFP, d_C_readFlatCurryFileInLoadPath, nd_C_readFlatCurryFileInLoadPath, d_C_findFileInLoadPath) where

import Basics
import qualified Curry_Directory
import qualified Curry_Distribution
import qualified Curry_FileGoodies
import qualified Curry_FlatCurry
import qualified Curry_FlatCurryGoodies
import qualified Curry_FlatCurryRead
import qualified Curry_Prelude
import qualified Curry_System
import qualified Curry_Maybe
data C_InterfaceOrFlatProg
     = C_IF Curry_FlatCurry.C_Prog
     | C_FP Curry_FlatCurry.C_Prog
     | Choice_C_InterfaceOrFlatProg Cover ID C_InterfaceOrFlatProg C_InterfaceOrFlatProg
     | Choices_C_InterfaceOrFlatProg Cover ID ([C_InterfaceOrFlatProg])
     | Fail_C_InterfaceOrFlatProg Cover FailInfo
     | Guard_C_InterfaceOrFlatProg Cover Constraints C_InterfaceOrFlatProg

instance Show C_InterfaceOrFlatProg where
  showsPrec d (Choice_C_InterfaceOrFlatProg cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_InterfaceOrFlatProg cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_InterfaceOrFlatProg cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_InterfaceOrFlatProg cd info) = showChar '!'
  showsPrec _ (C_IF x1) = (showString "(IF") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_FP x1) = (showString "(FP") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_InterfaceOrFlatProg where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_IF x1,r1) | (_,r0) <- readQualified "Imports" "IF" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_FP x1,r1) | (_,r0) <- readQualified "Imports" "FP" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet C_InterfaceOrFlatProg where
  choiceCons = Choice_C_InterfaceOrFlatProg
  choicesCons = Choices_C_InterfaceOrFlatProg
  failCons = Fail_C_InterfaceOrFlatProg
  guardCons = Guard_C_InterfaceOrFlatProg
  try (Choice_C_InterfaceOrFlatProg cd i x y) = tryChoice cd i x y
  try (Choices_C_InterfaceOrFlatProg cd i xs) = tryChoices cd i xs
  try (Fail_C_InterfaceOrFlatProg cd info) = Fail cd info
  try (Guard_C_InterfaceOrFlatProg cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_InterfaceOrFlatProg cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_InterfaceOrFlatProg cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_InterfaceOrFlatProg cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_InterfaceOrFlatProg cd i _) = error ("Imports.InterfaceOrFlatProg.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_InterfaceOrFlatProg cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_InterfaceOrFlatProg cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_InterfaceOrFlatProg where
  generate s c = Choices_C_InterfaceOrFlatProg c (freeID [1,1] s) [(C_IF (generate (leftSupply s) c)),(C_FP (generate (leftSupply s) c))]


instance NormalForm C_InterfaceOrFlatProg where
  ($!!) cont (C_IF x1) d cs = (((\y1 d cs -> cont (C_IF y1) d cs) $!! x1) d) cs
  ($!!) cont (C_FP x1) d cs = (((\y1 d cs -> cont (C_FP y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_InterfaceOrFlatProg cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_InterfaceOrFlatProg cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_InterfaceOrFlatProg cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_InterfaceOrFlatProg cd info) _ _ = failCons cd info
  ($##) cont (C_IF x1) d cs = (((\y1 d cs -> cont (C_IF y1) d cs) $## x1) d) cs
  ($##) cont (C_FP x1) d cs = (((\y1 d cs -> cont (C_FP y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_InterfaceOrFlatProg cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_InterfaceOrFlatProg cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_InterfaceOrFlatProg cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_InterfaceOrFlatProg cd info) _ _ = failCons cd info
  searchNF search cont (C_IF x1) = search (\y1 -> cont (C_IF y1)) x1
  searchNF search cont (C_FP x1) = search (\y1 -> cont (C_FP y1)) x1
  searchNF _ _ x = error ("Imports.InterfaceOrFlatProg.searchNF: no constructor: " ++ (show x))


instance Unifiable C_InterfaceOrFlatProg where
  (=.=) (C_IF x1) (C_IF y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_FP x1) (C_FP y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_IF x1) (C_IF y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_FP x1) (C_FP y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_IF x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_FP x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_InterfaceOrFlatProg cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_InterfaceOrFlatProg cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_InterfaceOrFlatProg cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_InterfaceOrFlatProg cd i _) = error ("Imports.InterfaceOrFlatProg.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_InterfaceOrFlatProg cd info) = [(Unsolvable info)]
  bind d i (Guard_C_InterfaceOrFlatProg cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_IF x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_FP x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_InterfaceOrFlatProg cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_InterfaceOrFlatProg cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_InterfaceOrFlatProg cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_InterfaceOrFlatProg cd i _) = error ("Imports.InterfaceOrFlatProg.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_InterfaceOrFlatProg cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_InterfaceOrFlatProg cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_InterfaceOrFlatProg where
  (=?=) (Choice_C_InterfaceOrFlatProg cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_InterfaceOrFlatProg cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_InterfaceOrFlatProg cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_InterfaceOrFlatProg cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_InterfaceOrFlatProg cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_InterfaceOrFlatProg cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_InterfaceOrFlatProg cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_InterfaceOrFlatProg cd info) _ _ = failCons cd info
  (=?=) (C_IF x1) (C_IF y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_FP x1) (C_FP y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_InterfaceOrFlatProg cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_InterfaceOrFlatProg cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_InterfaceOrFlatProg cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_InterfaceOrFlatProg cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_InterfaceOrFlatProg cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_InterfaceOrFlatProg cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_InterfaceOrFlatProg cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_InterfaceOrFlatProg cd info) _ _ = failCons cd info
  (<?=) (C_IF x1) (C_IF y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_IF _) (C_FP _) _ _ = Curry_Prelude.C_True
  (<?=) (C_FP x1) (C_FP y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_getImportedInterfaces :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_InterfaceOrFlatProg))
d_C_getImportedInterfaces x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_System.d_C_getArgs x3250 x3500) (d_OP_getImportedInterfaces_dot___hash_lambda1 x1) x3250 x3500

d_OP_getImportedInterfaces_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_InterfaceOrFlatProg))
d_OP_getImportedInterfaces_dot___hash_lambda1 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_FlatCurryRead.d_C_readFlatCurryIntWithImports (d_OP__case_1 x2 x1 (Curry_Prelude.d_C_null x2 x3250 x3500) x3250 x3500) x3250 x3500) d_OP_getImportedInterfaces_dot___hash_lambda1_dot___hash_lambda2 x3250 x3500

d_OP_getImportedInterfaces_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_InterfaceOrFlatProg))
d_OP_getImportedInterfaces_dot___hash_lambda1_dot___hash_lambda2 x1 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_C_map d_OP_getImportedInterfaces_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x3250 x3500) x3250 x3500

d_OP_getImportedInterfaces_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 :: Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_InterfaceOrFlatProg
d_OP_getImportedInterfaces_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x3250 x3500 = Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_progName x3250 x3500) x1 x3250 x3500) (C_IF x1)

d_C_moduleImports :: Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_moduleImports x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> Curry_Prelude.OP_Tuple2 x2 x3
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_moduleImports x1002 x3250 x3500) (d_C_moduleImports x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_moduleImports z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_moduleImports x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_ifOrProg :: Curry_Prelude.Curry t0 => (Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> t0) -> (Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> t0) -> C_InterfaceOrFlatProg -> Cover -> ConstStore -> t0
d_C_ifOrProg x1 x2 x3 x3250 x3500 = case x3 of
     (C_IF x4) -> Curry_Prelude.d_C_apply x1 x4 x3250 x3500
     (C_FP x5) -> Curry_Prelude.d_C_apply x2 x5 x3250 x3500
     (Choice_C_InterfaceOrFlatProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_ifOrProg x1 x2 x1002 x3250 x3500) (d_C_ifOrProg x1 x2 x1003 x3250 x3500)
     (Choices_C_InterfaceOrFlatProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_ifOrProg x1 x2 z x3250 x3500) x1002
     (Guard_C_InterfaceOrFlatProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_ifOrProg x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_InterfaceOrFlatProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_ifOrProg :: Curry_Prelude.Curry t0 => Func Curry_FlatCurry.C_Prog t0 -> Func Curry_FlatCurry.C_Prog t0 -> C_InterfaceOrFlatProg -> IDSupply -> Cover -> ConstStore -> t0
nd_C_ifOrProg x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (C_IF x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_apply x1 x4 x2000 x3250 x3500))
     (C_FP x5) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_apply x2 x5 x2000 x3250 x3500))
     (Choice_C_InterfaceOrFlatProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_ifOrProg x1 x2 x1002 x3000 x3250 x3500) (nd_C_ifOrProg x1 x2 x1003 x3000 x3250 x3500)
     (Choices_C_InterfaceOrFlatProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_ifOrProg x1 x2 z x3000 x3250 x3500) x1002
     (Guard_C_InterfaceOrFlatProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_ifOrProg x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_InterfaceOrFlatProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_progOfIFFP :: C_InterfaceOrFlatProg -> Cover -> ConstStore -> Curry_FlatCurry.C_Prog
d_C_progOfIFFP x1 x3250 x3500 = case x1 of
     (C_IF x2) -> x2
     (C_FP x3) -> x3
     (Choice_C_InterfaceOrFlatProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_progOfIFFP x1002 x3250 x3500) (d_C_progOfIFFP x1003 x3250 x3500)
     (Choices_C_InterfaceOrFlatProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_progOfIFFP z x3250 x3500) x1002
     (Guard_C_InterfaceOrFlatProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_progOfIFFP x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_InterfaceOrFlatProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_readFlatCurryFileInLoadPath :: Curry_Prelude.Curry t0 => (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_C_readFlatCurryFileInLoadPath x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_findFileInLoadPath x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List) x3250 x3500) (d_OP_readFlatCurryFileInLoadPath_dot___hash_lambda4 x2 x1) x3250 x3500

nd_C_readFlatCurryFileInLoadPath :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
nd_C_readFlatCurryFileInLoadPath x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_findFileInLoadPath x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List) x3250 x3500) (wrapNX id (nd_OP_readFlatCurryFileInLoadPath_dot___hash_lambda4 x2 x1)) x2000 x3250 x3500))

d_OP_readFlatCurryFileInLoadPath_dot___hash_lambda4 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO t0) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_readFlatCurryFileInLoadPath_dot___hash_lambda4 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) (d_C_readFlatCurryFileAndReport x2 x1) x3 x3250 x3500

nd_OP_readFlatCurryFileInLoadPath_dot___hash_lambda4 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t0) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
nd_OP_readFlatCurryFileInLoadPath_dot___hash_lambda4 x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_maybe (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))))))))))) x3250 x3500) x3250 x3500) x2000 x3250 x3500) (wrapNX id (nd_C_readFlatCurryFileAndReport x2 x1)) x3 x2001 x3250 x3500)))))

d_C_readFlatCurryFileAndReport :: Curry_Prelude.Curry t0 => (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_C_readFlatCurryFileAndReport x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_fileSize x3 x3250 x3500) (d_OP_readFlatCurryFileAndReport_dot___hash_lambda5 x3 x2 x1) x3250 x3500

nd_C_readFlatCurryFileAndReport :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
nd_C_readFlatCurryFileAndReport x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Directory.d_C_fileSize x3 x3250 x3500) (wrapNX id (nd_OP_readFlatCurryFileAndReport_dot___hash_lambda5 x3 x2 x1)) x2000 x3250 x3500))

d_OP_readFlatCurryFileAndReport_dot___hash_lambda5 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO t0) -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_readFlatCurryFileAndReport_dot___hash_lambda5 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurryFile x1 x3250 x3500) d_OP_readFlatCurryFileAndReport_dot___hash_lambda5_dot___hash_lambda6 x3250 x3500) x3250 x3500

nd_OP_readFlatCurryFileAndReport_dot___hash_lambda5 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t0) -> Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
nd_OP_readFlatCurryFileAndReport_dot___hash_lambda5 x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_OP_dollar x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500) (Curry_Prelude.nd_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurryFile x1 x3250 x3500) (wrapDX id d_OP_readFlatCurryFileAndReport_dot___hash_lambda5_dot___hash_lambda6) x2001 x3250 x3500) x3250 x3500)))))

d_OP_readFlatCurryFileAndReport_dot___hash_lambda5_dot___hash_lambda6 :: Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_readFlatCurryFileAndReport_dot___hash_lambda5_dot___hash_lambda6 x1 x3250 x3500 = Curry_Prelude.d_C_seq (Curry_Prelude.d_OP_eq_eq x1 x1 x3250 x3500) (Curry_Prelude.d_C_return x1 x3250 x3500) x3250 x3500

d_C_findFileInLoadPath :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_findFileInLoadPath x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getLocalLoadPath x2 x3250 x3500) (d_OP_findFileInLoadPath_dot___hash_lambda7 x1 x2) x3250 x3500

d_OP_findFileInLoadPath_dot___hash_lambda7 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_findFileInLoadPath_dot___hash_lambda7 x1 x2 x3 x3250 x3500 = Curry_FileGoodies.d_C_lookupFileInPath x1 x2 x3 x3250 x3500

d_C_getLocalLoadPath :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getLocalLoadPath x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_getLoadPath x3250 x3500) d_OP_getLocalLoadPath_dot___hash_lambda8 x3250 x3500

d_OP_getLocalLoadPath_dot___hash_lambda8 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getLocalLoadPath_dot___hash_lambda8 x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_System.d_C_getArgs x3250 x3500) (d_OP_getLocalLoadPath_dot___hash_lambda8_dot___hash_lambda9 x1) x3250 x3500

d_OP_getLocalLoadPath_dot___hash_lambda8_dot___hash_lambda9 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getLocalLoadPath_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x3250 x3500 = d_OP__case_0 x2 x1 (Curry_Prelude.d_C_null x2 x3250 x3500) x3250 x3500

d_OP__case_0 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_0 x2 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return x1 x3250 x3500
     Curry_Prelude.C_False -> let
          x3 = Curry_FileGoodies.d_C_dirName (Curry_Prelude.d_C_head x2 x3250 x3500) x3250 x3500
           in (Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons x3 (Curry_Prelude.OP_Cons (Curry_Distribution.d_C_addCurrySubdir x3 x3250 x3500) (Curry_Prelude.d_C_tail x1 x3250 x3500))) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x1 x1002 x3250 x3500) (d_OP__case_0 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_1 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_FileGoodies.d_C_dirName (Curry_Prelude.d_C_head x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x2 x1 x1002 x3250 x3500) (d_OP__case_1 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo