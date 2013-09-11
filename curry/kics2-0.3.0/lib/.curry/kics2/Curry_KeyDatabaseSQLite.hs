{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_KeyDatabaseSQLite (C_TError (..), C_TErrorKind (..), C_Query, C_Transaction, C_TransResult, C_Dynamic, C_ColVal, d_C_runQ, d_C_transformQ, nd_C_transformQ, d_C_runT, d_C_runJustT, d_C_getDB, d_C_returnT, nd_C_returnT, d_C_doneT, d_C_errorT, nd_C_errorT, d_C_failT, nd_C_failT, d_OP_bar_gt_gt_eq, nd_OP_bar_gt_gt_eq, d_OP_bar_gt_gt, d_C_sequenceT, nd_C_sequenceT, d_C_sequenceT_, nd_C_sequenceT_, d_C_mapT, nd_C_mapT, d_C_mapT_, nd_C_mapT_, d_C_persistentSQLite, d_C_existsDBKey, nd_C_existsDBKey, d_C_allDBKeys, nd_C_allDBKeys, d_C_allDBInfos, nd_C_allDBInfos, d_C_allDBKeyInfos, nd_C_allDBKeyInfos, d_OP_at_eq, d_C_someDBKeys, nd_C_someDBKeys, d_C_someDBInfos, nd_C_someDBInfos, d_C_someDBKeyInfos, nd_C_someDBKeyInfos, d_C_someDBKeyProjections, nd_C_someDBKeyProjections, d_C_getDBInfo, nd_C_getDBInfo, d_C_getDBInfos, nd_C_getDBInfos, d_C_deleteDBEntry, nd_C_deleteDBEntry, d_C_deleteDBEntries, nd_C_deleteDBEntries, d_C_updateDBEntry, nd_C_updateDBEntry, d_C_newDBEntry, nd_C_newDBEntry, d_C_newDBKeyEntry, nd_C_newDBKeyEntry, d_C_cleanDB, nd_C_cleanDB, d_C_closeDBHandles, d_C_showTError) where

import Basics
import qualified Curry_Global
import qualified Curry_IO
import qualified Curry_IOExts
import qualified Curry_List
import qualified Curry_Maybe
import qualified Curry_Prelude
import qualified Curry_ReadNumeric
import qualified Curry_ReadShowTerm
type C_DBFile = Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_TableName = Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_ColName = Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_Key = Curry_Prelude.C_Int

type C_KeyPred t0 = Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic

type C_Row = Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_Stack = Curry_Prelude.OP_List Curry_Prelude.C_Char

data C_Query t0
     = C_Query (Curry_Prelude.C_IO t0)
     | Choice_C_Query Cover ID (C_Query t0) (C_Query t0)
     | Choices_C_Query Cover ID ([C_Query t0])
     | Fail_C_Query Cover FailInfo
     | Guard_C_Query Cover Constraints (C_Query t0)

instance Show t0 => Show (C_Query t0) where
  showsPrec d (Choice_C_Query cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Query cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Query cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Query cd info) = showChar '!'
  showsPrec _ (C_Query x1) = (showString "(Query") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read t0 => Read (C_Query t0) where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Query x1,r1) | (_,r0) <- readQualified "KeyDatabaseSQLite" "Query" r, (x1,r1) <- readsPrec 11 r0]) s


instance NonDet (C_Query t0) where
  choiceCons = Choice_C_Query
  choicesCons = Choices_C_Query
  failCons = Fail_C_Query
  guardCons = Guard_C_Query
  try (Choice_C_Query cd i x y) = tryChoice cd i x y
  try (Choices_C_Query cd i xs) = tryChoices cd i xs
  try (Fail_C_Query cd info) = Fail cd info
  try (Guard_C_Query cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Query cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Query cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Query cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Query cd i _) = error ("KeyDatabaseSQLite.Query.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Query cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Query cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_Query t0) where
  generate s c = Choices_C_Query c (freeID [1] s) [(C_Query (generate (leftSupply s) c))]


instance NormalForm t0 => NormalForm (C_Query t0) where
  ($!!) cont (C_Query x1) d cs = (((\y1 d cs -> cont (C_Query y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Query cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Query cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Query cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Query cd info) _ _ = failCons cd info
  ($##) cont (C_Query x1) d cs = (((\y1 d cs -> cont (C_Query y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Query cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Query cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Query cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Query cd info) _ _ = failCons cd info
  searchNF search cont (C_Query x1) = search (\y1 -> cont (C_Query y1)) x1
  searchNF _ _ x = error ("KeyDatabaseSQLite.Query.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_Query t0) where
  (=.=) (C_Query x1) (C_Query y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Query x1) (C_Query y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Query x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Query cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Query cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Query cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Query cd i _) = error ("KeyDatabaseSQLite.Query.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Query cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Query cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Query x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Query cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Query cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Query cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Query cd i _) = error ("KeyDatabaseSQLite.Query.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Query cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Query cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_Query t0) where
  (=?=) (Choice_C_Query cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Query cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Query cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Query cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Query cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Query cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Query cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Query cd info) _ _ = failCons cd info
  (=?=) (C_Query x1) (C_Query y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (<?=) (Choice_C_Query cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Query cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Query cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Query cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Query cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Query cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Query cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Query cd info) _ _ = failCons cd info
  (<?=) (C_Query x1) (C_Query y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs


data C_Transaction t0
     = C_Trans (Curry_Prelude.C_IO (C_TransResult t0))
     | Choice_C_Transaction Cover ID (C_Transaction t0) (C_Transaction t0)
     | Choices_C_Transaction Cover ID ([C_Transaction t0])
     | Fail_C_Transaction Cover FailInfo
     | Guard_C_Transaction Cover Constraints (C_Transaction t0)

instance Show t0 => Show (C_Transaction t0) where
  showsPrec d (Choice_C_Transaction cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Transaction cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Transaction cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Transaction cd info) = showChar '!'
  showsPrec _ (C_Trans x1) = (showString "(Trans") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read t0 => Read (C_Transaction t0) where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Trans x1,r1) | (_,r0) <- readQualified "KeyDatabaseSQLite" "Trans" r, (x1,r1) <- readsPrec 11 r0]) s


instance NonDet (C_Transaction t0) where
  choiceCons = Choice_C_Transaction
  choicesCons = Choices_C_Transaction
  failCons = Fail_C_Transaction
  guardCons = Guard_C_Transaction
  try (Choice_C_Transaction cd i x y) = tryChoice cd i x y
  try (Choices_C_Transaction cd i xs) = tryChoices cd i xs
  try (Fail_C_Transaction cd info) = Fail cd info
  try (Guard_C_Transaction cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Transaction cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Transaction cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Transaction cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Transaction cd i _) = error ("KeyDatabaseSQLite.Transaction.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Transaction cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Transaction cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_Transaction t0) where
  generate s c = Choices_C_Transaction c (freeID [1] s) [(C_Trans (generate (leftSupply s) c))]


instance NormalForm t0 => NormalForm (C_Transaction t0) where
  ($!!) cont (C_Trans x1) d cs = (((\y1 d cs -> cont (C_Trans y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Transaction cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Transaction cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Transaction cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Transaction cd info) _ _ = failCons cd info
  ($##) cont (C_Trans x1) d cs = (((\y1 d cs -> cont (C_Trans y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Transaction cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Transaction cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Transaction cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Transaction cd info) _ _ = failCons cd info
  searchNF search cont (C_Trans x1) = search (\y1 -> cont (C_Trans y1)) x1
  searchNF _ _ x = error ("KeyDatabaseSQLite.Transaction.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_Transaction t0) where
  (=.=) (C_Trans x1) (C_Trans y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Trans x1) (C_Trans y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Trans x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Transaction cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Transaction cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Transaction cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Transaction cd i _) = error ("KeyDatabaseSQLite.Transaction.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Transaction cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Transaction cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Trans x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Transaction cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Transaction cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Transaction cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Transaction cd i _) = error ("KeyDatabaseSQLite.Transaction.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Transaction cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Transaction cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_Transaction t0) where
  (=?=) (Choice_C_Transaction cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Transaction cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Transaction cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Transaction cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Transaction cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Transaction cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Transaction cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Transaction cd info) _ _ = failCons cd info
  (=?=) (C_Trans x1) (C_Trans y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (<?=) (Choice_C_Transaction cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Transaction cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Transaction cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Transaction cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Transaction cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Transaction cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Transaction cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Transaction cd info) _ _ = failCons cd info
  (<?=) (C_Trans x1) (C_Trans y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs


data C_TransResult t0
     = C_OK t0
     | C_Error C_TError
     | Choice_C_TransResult Cover ID (C_TransResult t0) (C_TransResult t0)
     | Choices_C_TransResult Cover ID ([C_TransResult t0])
     | Fail_C_TransResult Cover FailInfo
     | Guard_C_TransResult Cover Constraints (C_TransResult t0)

instance Show t0 => Show (C_TransResult t0) where
  showsPrec d (Choice_C_TransResult cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_TransResult cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_TransResult cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_TransResult cd info) = showChar '!'
  showsPrec _ (C_OK x1) = (showString "(OK") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Error x1) = (showString "(Error") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read t0 => Read (C_TransResult t0) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_OK x1,r1) | (_,r0) <- readQualified "KeyDatabaseSQLite" "OK" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_Error x1,r1) | (_,r0) <- readQualified "KeyDatabaseSQLite" "Error" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet (C_TransResult t0) where
  choiceCons = Choice_C_TransResult
  choicesCons = Choices_C_TransResult
  failCons = Fail_C_TransResult
  guardCons = Guard_C_TransResult
  try (Choice_C_TransResult cd i x y) = tryChoice cd i x y
  try (Choices_C_TransResult cd i xs) = tryChoices cd i xs
  try (Fail_C_TransResult cd info) = Fail cd info
  try (Guard_C_TransResult cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_TransResult cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_TransResult cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_TransResult cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_TransResult cd i _) = error ("KeyDatabaseSQLite.TransResult.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_TransResult cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_TransResult cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_TransResult t0) where
  generate s c = Choices_C_TransResult c (freeID [1,1] s) [(C_OK (generate (leftSupply s) c)),(C_Error (generate (leftSupply s) c))]


instance NormalForm t0 => NormalForm (C_TransResult t0) where
  ($!!) cont (C_OK x1) d cs = (((\y1 d cs -> cont (C_OK y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Error x1) d cs = (((\y1 d cs -> cont (C_Error y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_TransResult cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_TransResult cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_TransResult cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_TransResult cd info) _ _ = failCons cd info
  ($##) cont (C_OK x1) d cs = (((\y1 d cs -> cont (C_OK y1) d cs) $## x1) d) cs
  ($##) cont (C_Error x1) d cs = (((\y1 d cs -> cont (C_Error y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_TransResult cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_TransResult cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_TransResult cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_TransResult cd info) _ _ = failCons cd info
  searchNF search cont (C_OK x1) = search (\y1 -> cont (C_OK y1)) x1
  searchNF search cont (C_Error x1) = search (\y1 -> cont (C_Error y1)) x1
  searchNF _ _ x = error ("KeyDatabaseSQLite.TransResult.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_TransResult t0) where
  (=.=) (C_OK x1) (C_OK y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Error x1) (C_Error y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_OK x1) (C_OK y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Error x1) (C_Error y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_OK x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Error x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_TransResult cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_TransResult cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_TransResult cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_TransResult cd i _) = error ("KeyDatabaseSQLite.TransResult.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_TransResult cd info) = [(Unsolvable info)]
  bind d i (Guard_C_TransResult cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_OK x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Error x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_TransResult cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_TransResult cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_TransResult cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_TransResult cd i _) = error ("KeyDatabaseSQLite.TransResult.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_TransResult cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_TransResult cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_TransResult t0) where
  (=?=) (Choice_C_TransResult cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_TransResult cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_TransResult cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_TransResult cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_TransResult cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_TransResult cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_TransResult cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_TransResult cd info) _ _ = failCons cd info
  (=?=) (C_OK x1) (C_OK y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Error x1) (C_Error y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_TransResult cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_TransResult cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_TransResult cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_TransResult cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_TransResult cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_TransResult cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_TransResult cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_TransResult cd info) _ _ = failCons cd info
  (<?=) (C_OK x1) (C_OK y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_OK _) (C_Error _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Error x1) (C_Error y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_Dynamic
     = C_DBInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | Choice_C_Dynamic Cover ID C_Dynamic C_Dynamic
     | Choices_C_Dynamic Cover ID ([C_Dynamic])
     | Fail_C_Dynamic Cover FailInfo
     | Guard_C_Dynamic Cover Constraints C_Dynamic

instance Show C_Dynamic where
  showsPrec d (Choice_C_Dynamic cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Dynamic cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Dynamic cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Dynamic cd info) = showChar '!'
  showsPrec _ (C_DBInfo x1 x2 x3) = (showString "(DBInfo") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_Dynamic where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_DBInfo x1 x2 x3,r3) | (_,r0) <- readQualified "KeyDatabaseSQLite" "DBInfo" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_Dynamic where
  choiceCons = Choice_C_Dynamic
  choicesCons = Choices_C_Dynamic
  failCons = Fail_C_Dynamic
  guardCons = Guard_C_Dynamic
  try (Choice_C_Dynamic cd i x y) = tryChoice cd i x y
  try (Choices_C_Dynamic cd i xs) = tryChoices cd i xs
  try (Fail_C_Dynamic cd info) = Fail cd info
  try (Guard_C_Dynamic cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Dynamic cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Dynamic cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Dynamic cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Dynamic cd i _) = error ("KeyDatabaseSQLite.Dynamic.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Dynamic cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Dynamic cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Dynamic where
  generate s c = Choices_C_Dynamic c (freeID [3] s) [(C_DBInfo (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance NormalForm C_Dynamic where
  ($!!) cont (C_DBInfo x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_DBInfo y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Dynamic cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Dynamic cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Dynamic cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Dynamic cd info) _ _ = failCons cd info
  ($##) cont (C_DBInfo x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_DBInfo y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Dynamic cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Dynamic cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Dynamic cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Dynamic cd info) _ _ = failCons cd info
  searchNF search cont (C_DBInfo x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_DBInfo y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("KeyDatabaseSQLite.Dynamic.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Dynamic where
  (=.=) (C_DBInfo x1 x2 x3) (C_DBInfo y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_DBInfo x1 x2 x3) (C_DBInfo y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_DBInfo x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_Dynamic cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Dynamic cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Dynamic cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Dynamic cd i _) = error ("KeyDatabaseSQLite.Dynamic.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Dynamic cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Dynamic cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_DBInfo x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_Dynamic cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Dynamic cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Dynamic cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Dynamic cd i _) = error ("KeyDatabaseSQLite.Dynamic.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Dynamic cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Dynamic cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Dynamic where
  (=?=) (Choice_C_Dynamic cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Dynamic cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Dynamic cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Dynamic cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Dynamic cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Dynamic cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Dynamic cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Dynamic cd info) _ _ = failCons cd info
  (=?=) (C_DBInfo x1 x2 x3) (C_DBInfo y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (<?=) (Choice_C_Dynamic cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Dynamic cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Dynamic cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Dynamic cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Dynamic cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Dynamic cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Dynamic cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Dynamic cd info) _ _ = failCons cd info
  (<?=) (C_DBInfo x1 x2 x3) (C_DBInfo y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs


data C_ColVal
     = C_ColVal Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_ColVal Cover ID C_ColVal C_ColVal
     | Choices_C_ColVal Cover ID ([C_ColVal])
     | Fail_C_ColVal Cover FailInfo
     | Guard_C_ColVal Cover Constraints C_ColVal

instance Show C_ColVal where
  showsPrec d (Choice_C_ColVal cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ColVal cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ColVal cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ColVal cd info) = showChar '!'
  showsPrec _ (C_ColVal x1 x2) = (showString "(ColVal") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_ColVal where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_ColVal x1 x2,r2) | (_,r0) <- readQualified "KeyDatabaseSQLite" "ColVal" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s


instance NonDet C_ColVal where
  choiceCons = Choice_C_ColVal
  choicesCons = Choices_C_ColVal
  failCons = Fail_C_ColVal
  guardCons = Guard_C_ColVal
  try (Choice_C_ColVal cd i x y) = tryChoice cd i x y
  try (Choices_C_ColVal cd i xs) = tryChoices cd i xs
  try (Fail_C_ColVal cd info) = Fail cd info
  try (Guard_C_ColVal cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ColVal cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ColVal cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ColVal cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ColVal cd i _) = error ("KeyDatabaseSQLite.ColVal.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ColVal cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ColVal cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_ColVal where
  generate s c = Choices_C_ColVal c (freeID [2] s) [(C_ColVal (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_ColVal where
  ($!!) cont (C_ColVal x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ColVal y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_ColVal cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_ColVal cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_ColVal cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_ColVal cd info) _ _ = failCons cd info
  ($##) cont (C_ColVal x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ColVal y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_ColVal cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_ColVal cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_ColVal cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_ColVal cd info) _ _ = failCons cd info
  searchNF search cont (C_ColVal x1 x2) = search (\y1 -> search (\y2 -> cont (C_ColVal y1 y2)) x2) x1
  searchNF _ _ x = error ("KeyDatabaseSQLite.ColVal.searchNF: no constructor: " ++ (show x))


instance Unifiable C_ColVal where
  (=.=) (C_ColVal x1 x2) (C_ColVal y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_ColVal x1 x2) (C_ColVal y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_ColVal x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_ColVal cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_ColVal cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_ColVal cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_ColVal cd i _) = error ("KeyDatabaseSQLite.ColVal.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_ColVal cd info) = [(Unsolvable info)]
  bind d i (Guard_C_ColVal cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_ColVal x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_ColVal cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_ColVal cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_ColVal cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_ColVal cd i _) = error ("KeyDatabaseSQLite.ColVal.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_ColVal cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_ColVal cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_ColVal where
  (=?=) (Choice_C_ColVal cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_ColVal cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_ColVal cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_ColVal cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_ColVal cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_ColVal cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_ColVal cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_ColVal cd info) _ _ = failCons cd info
  (=?=) (C_ColVal x1 x2) (C_ColVal y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (<?=) (Choice_C_ColVal cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_ColVal cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_ColVal cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_ColVal cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_ColVal cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_ColVal cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_ColVal cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_ColVal cd info) _ _ = failCons cd info
  (<?=) (C_ColVal x1 x2) (C_ColVal y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs


data C_TError
     = C_TError C_TErrorKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_TError Cover ID C_TError C_TError
     | Choices_C_TError Cover ID ([C_TError])
     | Fail_C_TError Cover FailInfo
     | Guard_C_TError Cover Constraints C_TError

instance Show C_TError where
  showsPrec d (Choice_C_TError cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_TError cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_TError cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_TError cd info) = showChar '!'
  showsPrec _ (C_TError x1 x2) = (showString "(TError") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_TError where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_TError x1 x2,r2) | (_,r0) <- readQualified "KeyDatabaseSQLite" "TError" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s


instance NonDet C_TError where
  choiceCons = Choice_C_TError
  choicesCons = Choices_C_TError
  failCons = Fail_C_TError
  guardCons = Guard_C_TError
  try (Choice_C_TError cd i x y) = tryChoice cd i x y
  try (Choices_C_TError cd i xs) = tryChoices cd i xs
  try (Fail_C_TError cd info) = Fail cd info
  try (Guard_C_TError cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_TError cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_TError cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_TError cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_TError cd i _) = error ("KeyDatabaseSQLite.TError.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_TError cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_TError cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_TError where
  generate s c = Choices_C_TError c (freeID [2] s) [(C_TError (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_TError where
  ($!!) cont (C_TError x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_TError y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_TError cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_TError cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_TError cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_TError cd info) _ _ = failCons cd info
  ($##) cont (C_TError x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_TError y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_TError cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_TError cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_TError cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_TError cd info) _ _ = failCons cd info
  searchNF search cont (C_TError x1 x2) = search (\y1 -> search (\y2 -> cont (C_TError y1 y2)) x2) x1
  searchNF _ _ x = error ("KeyDatabaseSQLite.TError.searchNF: no constructor: " ++ (show x))


instance Unifiable C_TError where
  (=.=) (C_TError x1 x2) (C_TError y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_TError x1 x2) (C_TError y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_TError x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_TError cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_TError cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_TError cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_TError cd i _) = error ("KeyDatabaseSQLite.TError.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_TError cd info) = [(Unsolvable info)]
  bind d i (Guard_C_TError cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_TError x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_TError cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_TError cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_TError cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_TError cd i _) = error ("KeyDatabaseSQLite.TError.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_TError cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_TError cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_TError where
  (=?=) (Choice_C_TError cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_TError cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_TError cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_TError cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_TError cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_TError cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_TError cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_TError cd info) _ _ = failCons cd info
  (=?=) (C_TError x1 x2) (C_TError y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (<?=) (Choice_C_TError cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_TError cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_TError cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_TError cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_TError cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_TError cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_TError cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_TError cd info) _ _ = failCons cd info
  (<?=) (C_TError x1 x2) (C_TError y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs


data C_TErrorKind
     = C_KeyNotExistsError
     | C_NoRelationshipError
     | C_DuplicateKeyError
     | C_KeyRequiredError
     | C_UniqueError
     | C_MinError
     | C_MaxError
     | C_UserDefinedError
     | C_ExecutionError
     | Choice_C_TErrorKind Cover ID C_TErrorKind C_TErrorKind
     | Choices_C_TErrorKind Cover ID ([C_TErrorKind])
     | Fail_C_TErrorKind Cover FailInfo
     | Guard_C_TErrorKind Cover Constraints C_TErrorKind

instance Show C_TErrorKind where
  showsPrec d (Choice_C_TErrorKind cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_TErrorKind cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_TErrorKind cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_TErrorKind cd info) = showChar '!'
  showsPrec _ C_KeyNotExistsError = showString "KeyNotExistsError"
  showsPrec _ C_NoRelationshipError = showString "NoRelationshipError"
  showsPrec _ C_DuplicateKeyError = showString "DuplicateKeyError"
  showsPrec _ C_KeyRequiredError = showString "KeyRequiredError"
  showsPrec _ C_UniqueError = showString "UniqueError"
  showsPrec _ C_MinError = showString "MinError"
  showsPrec _ C_MaxError = showString "MaxError"
  showsPrec _ C_UserDefinedError = showString "UserDefinedError"
  showsPrec _ C_ExecutionError = showString "ExecutionError"


instance Read C_TErrorKind where
  readsPrec _ s = (readParen False (\r -> [ (C_KeyNotExistsError,r0) | (_,r0) <- readQualified "KeyDatabaseSQLite" "KeyNotExistsError" r]) s) ++ ((readParen False (\r -> [ (C_NoRelationshipError,r0) | (_,r0) <- readQualified "KeyDatabaseSQLite" "NoRelationshipError" r]) s) ++ ((readParen False (\r -> [ (C_DuplicateKeyError,r0) | (_,r0) <- readQualified "KeyDatabaseSQLite" "DuplicateKeyError" r]) s) ++ ((readParen False (\r -> [ (C_KeyRequiredError,r0) | (_,r0) <- readQualified "KeyDatabaseSQLite" "KeyRequiredError" r]) s) ++ ((readParen False (\r -> [ (C_UniqueError,r0) | (_,r0) <- readQualified "KeyDatabaseSQLite" "UniqueError" r]) s) ++ ((readParen False (\r -> [ (C_MinError,r0) | (_,r0) <- readQualified "KeyDatabaseSQLite" "MinError" r]) s) ++ ((readParen False (\r -> [ (C_MaxError,r0) | (_,r0) <- readQualified "KeyDatabaseSQLite" "MaxError" r]) s) ++ ((readParen False (\r -> [ (C_UserDefinedError,r0) | (_,r0) <- readQualified "KeyDatabaseSQLite" "UserDefinedError" r]) s) ++ (readParen False (\r -> [ (C_ExecutionError,r0) | (_,r0) <- readQualified "KeyDatabaseSQLite" "ExecutionError" r]) s))))))))


instance NonDet C_TErrorKind where
  choiceCons = Choice_C_TErrorKind
  choicesCons = Choices_C_TErrorKind
  failCons = Fail_C_TErrorKind
  guardCons = Guard_C_TErrorKind
  try (Choice_C_TErrorKind cd i x y) = tryChoice cd i x y
  try (Choices_C_TErrorKind cd i xs) = tryChoices cd i xs
  try (Fail_C_TErrorKind cd info) = Fail cd info
  try (Guard_C_TErrorKind cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_TErrorKind cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_TErrorKind cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_TErrorKind cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_TErrorKind cd i _) = error ("KeyDatabaseSQLite.TErrorKind.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_TErrorKind cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_TErrorKind cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_TErrorKind where
  generate s c = Choices_C_TErrorKind c (freeID [0,0,0,0,0,0,0,0,0] s) [C_KeyNotExistsError,C_NoRelationshipError,C_DuplicateKeyError,C_KeyRequiredError,C_UniqueError,C_MinError,C_MaxError,C_UserDefinedError,C_ExecutionError]


instance NormalForm C_TErrorKind where
  ($!!) cont C_KeyNotExistsError d cs = cont C_KeyNotExistsError d cs
  ($!!) cont C_NoRelationshipError d cs = cont C_NoRelationshipError d cs
  ($!!) cont C_DuplicateKeyError d cs = cont C_DuplicateKeyError d cs
  ($!!) cont C_KeyRequiredError d cs = cont C_KeyRequiredError d cs
  ($!!) cont C_UniqueError d cs = cont C_UniqueError d cs
  ($!!) cont C_MinError d cs = cont C_MinError d cs
  ($!!) cont C_MaxError d cs = cont C_MaxError d cs
  ($!!) cont C_UserDefinedError d cs = cont C_UserDefinedError d cs
  ($!!) cont C_ExecutionError d cs = cont C_ExecutionError d cs
  ($!!) cont (Choice_C_TErrorKind cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_TErrorKind cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_TErrorKind cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_TErrorKind cd info) _ _ = failCons cd info
  ($##) cont C_KeyNotExistsError d cs = cont C_KeyNotExistsError d cs
  ($##) cont C_NoRelationshipError d cs = cont C_NoRelationshipError d cs
  ($##) cont C_DuplicateKeyError d cs = cont C_DuplicateKeyError d cs
  ($##) cont C_KeyRequiredError d cs = cont C_KeyRequiredError d cs
  ($##) cont C_UniqueError d cs = cont C_UniqueError d cs
  ($##) cont C_MinError d cs = cont C_MinError d cs
  ($##) cont C_MaxError d cs = cont C_MaxError d cs
  ($##) cont C_UserDefinedError d cs = cont C_UserDefinedError d cs
  ($##) cont C_ExecutionError d cs = cont C_ExecutionError d cs
  ($##) cont (Choice_C_TErrorKind cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_TErrorKind cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_TErrorKind cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_TErrorKind cd info) _ _ = failCons cd info
  searchNF _ cont C_KeyNotExistsError = cont C_KeyNotExistsError
  searchNF _ cont C_NoRelationshipError = cont C_NoRelationshipError
  searchNF _ cont C_DuplicateKeyError = cont C_DuplicateKeyError
  searchNF _ cont C_KeyRequiredError = cont C_KeyRequiredError
  searchNF _ cont C_UniqueError = cont C_UniqueError
  searchNF _ cont C_MinError = cont C_MinError
  searchNF _ cont C_MaxError = cont C_MaxError
  searchNF _ cont C_UserDefinedError = cont C_UserDefinedError
  searchNF _ cont C_ExecutionError = cont C_ExecutionError
  searchNF _ _ x = error ("KeyDatabaseSQLite.TErrorKind.searchNF: no constructor: " ++ (show x))


instance Unifiable C_TErrorKind where
  (=.=) C_KeyNotExistsError C_KeyNotExistsError d cs = C_Success
  (=.=) C_NoRelationshipError C_NoRelationshipError d cs = C_Success
  (=.=) C_DuplicateKeyError C_DuplicateKeyError d cs = C_Success
  (=.=) C_KeyRequiredError C_KeyRequiredError d cs = C_Success
  (=.=) C_UniqueError C_UniqueError d cs = C_Success
  (=.=) C_MinError C_MinError d cs = C_Success
  (=.=) C_MaxError C_MaxError d cs = C_Success
  (=.=) C_UserDefinedError C_UserDefinedError d cs = C_Success
  (=.=) C_ExecutionError C_ExecutionError d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_KeyNotExistsError C_KeyNotExistsError d cs = C_Success
  (=.<=) C_NoRelationshipError C_NoRelationshipError d cs = C_Success
  (=.<=) C_DuplicateKeyError C_DuplicateKeyError d cs = C_Success
  (=.<=) C_KeyRequiredError C_KeyRequiredError d cs = C_Success
  (=.<=) C_UniqueError C_UniqueError d cs = C_Success
  (=.<=) C_MinError C_MinError d cs = C_Success
  (=.<=) C_MaxError C_MaxError d cs = C_Success
  (=.<=) C_UserDefinedError C_UserDefinedError d cs = C_Success
  (=.<=) C_ExecutionError C_ExecutionError d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_KeyNotExistsError = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_NoRelationshipError = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_DuplicateKeyError = ((i :=: (ChooseN 2 0)):(concat []))
  bind cd i C_KeyRequiredError = ((i :=: (ChooseN 3 0)):(concat []))
  bind cd i C_UniqueError = ((i :=: (ChooseN 4 0)):(concat []))
  bind cd i C_MinError = ((i :=: (ChooseN 5 0)):(concat []))
  bind cd i C_MaxError = ((i :=: (ChooseN 6 0)):(concat []))
  bind cd i C_UserDefinedError = ((i :=: (ChooseN 7 0)):(concat []))
  bind cd i C_ExecutionError = ((i :=: (ChooseN 8 0)):(concat []))
  bind d i (Choice_C_TErrorKind cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_TErrorKind cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_TErrorKind cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_TErrorKind cd i _) = error ("KeyDatabaseSQLite.TErrorKind.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_TErrorKind cd info) = [(Unsolvable info)]
  bind d i (Guard_C_TErrorKind cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_KeyNotExistsError = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_NoRelationshipError = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_DuplicateKeyError = [(i :=: (ChooseN 2 0))]
  lazyBind cd i C_KeyRequiredError = [(i :=: (ChooseN 3 0))]
  lazyBind cd i C_UniqueError = [(i :=: (ChooseN 4 0))]
  lazyBind cd i C_MinError = [(i :=: (ChooseN 5 0))]
  lazyBind cd i C_MaxError = [(i :=: (ChooseN 6 0))]
  lazyBind cd i C_UserDefinedError = [(i :=: (ChooseN 7 0))]
  lazyBind cd i C_ExecutionError = [(i :=: (ChooseN 8 0))]
  lazyBind d i (Choice_C_TErrorKind cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_TErrorKind cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_TErrorKind cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_TErrorKind cd i _) = error ("KeyDatabaseSQLite.TErrorKind.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_TErrorKind cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_TErrorKind cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_TErrorKind where
  (=?=) (Choice_C_TErrorKind cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_TErrorKind cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_TErrorKind cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_TErrorKind cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_TErrorKind cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_TErrorKind cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_TErrorKind cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_TErrorKind cd info) _ _ = failCons cd info
  (=?=) C_KeyNotExistsError C_KeyNotExistsError d cs = Curry_Prelude.C_True
  (=?=) C_NoRelationshipError C_NoRelationshipError d cs = Curry_Prelude.C_True
  (=?=) C_DuplicateKeyError C_DuplicateKeyError d cs = Curry_Prelude.C_True
  (=?=) C_KeyRequiredError C_KeyRequiredError d cs = Curry_Prelude.C_True
  (=?=) C_UniqueError C_UniqueError d cs = Curry_Prelude.C_True
  (=?=) C_MinError C_MinError d cs = Curry_Prelude.C_True
  (=?=) C_MaxError C_MaxError d cs = Curry_Prelude.C_True
  (=?=) C_UserDefinedError C_UserDefinedError d cs = Curry_Prelude.C_True
  (=?=) C_ExecutionError C_ExecutionError d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_TErrorKind cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_TErrorKind cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_TErrorKind cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_TErrorKind cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_TErrorKind cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_TErrorKind cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_TErrorKind cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_TErrorKind cd info) _ _ = failCons cd info
  (<?=) C_KeyNotExistsError C_KeyNotExistsError d cs = Curry_Prelude.C_True
  (<?=) C_KeyNotExistsError C_NoRelationshipError _ _ = Curry_Prelude.C_True
  (<?=) C_KeyNotExistsError C_DuplicateKeyError _ _ = Curry_Prelude.C_True
  (<?=) C_KeyNotExistsError C_KeyRequiredError _ _ = Curry_Prelude.C_True
  (<?=) C_KeyNotExistsError C_UniqueError _ _ = Curry_Prelude.C_True
  (<?=) C_KeyNotExistsError C_MinError _ _ = Curry_Prelude.C_True
  (<?=) C_KeyNotExistsError C_MaxError _ _ = Curry_Prelude.C_True
  (<?=) C_KeyNotExistsError C_UserDefinedError _ _ = Curry_Prelude.C_True
  (<?=) C_KeyNotExistsError C_ExecutionError _ _ = Curry_Prelude.C_True
  (<?=) C_NoRelationshipError C_NoRelationshipError d cs = Curry_Prelude.C_True
  (<?=) C_NoRelationshipError C_DuplicateKeyError _ _ = Curry_Prelude.C_True
  (<?=) C_NoRelationshipError C_KeyRequiredError _ _ = Curry_Prelude.C_True
  (<?=) C_NoRelationshipError C_UniqueError _ _ = Curry_Prelude.C_True
  (<?=) C_NoRelationshipError C_MinError _ _ = Curry_Prelude.C_True
  (<?=) C_NoRelationshipError C_MaxError _ _ = Curry_Prelude.C_True
  (<?=) C_NoRelationshipError C_UserDefinedError _ _ = Curry_Prelude.C_True
  (<?=) C_NoRelationshipError C_ExecutionError _ _ = Curry_Prelude.C_True
  (<?=) C_DuplicateKeyError C_DuplicateKeyError d cs = Curry_Prelude.C_True
  (<?=) C_DuplicateKeyError C_KeyRequiredError _ _ = Curry_Prelude.C_True
  (<?=) C_DuplicateKeyError C_UniqueError _ _ = Curry_Prelude.C_True
  (<?=) C_DuplicateKeyError C_MinError _ _ = Curry_Prelude.C_True
  (<?=) C_DuplicateKeyError C_MaxError _ _ = Curry_Prelude.C_True
  (<?=) C_DuplicateKeyError C_UserDefinedError _ _ = Curry_Prelude.C_True
  (<?=) C_DuplicateKeyError C_ExecutionError _ _ = Curry_Prelude.C_True
  (<?=) C_KeyRequiredError C_KeyRequiredError d cs = Curry_Prelude.C_True
  (<?=) C_KeyRequiredError C_UniqueError _ _ = Curry_Prelude.C_True
  (<?=) C_KeyRequiredError C_MinError _ _ = Curry_Prelude.C_True
  (<?=) C_KeyRequiredError C_MaxError _ _ = Curry_Prelude.C_True
  (<?=) C_KeyRequiredError C_UserDefinedError _ _ = Curry_Prelude.C_True
  (<?=) C_KeyRequiredError C_ExecutionError _ _ = Curry_Prelude.C_True
  (<?=) C_UniqueError C_UniqueError d cs = Curry_Prelude.C_True
  (<?=) C_UniqueError C_MinError _ _ = Curry_Prelude.C_True
  (<?=) C_UniqueError C_MaxError _ _ = Curry_Prelude.C_True
  (<?=) C_UniqueError C_UserDefinedError _ _ = Curry_Prelude.C_True
  (<?=) C_UniqueError C_ExecutionError _ _ = Curry_Prelude.C_True
  (<?=) C_MinError C_MinError d cs = Curry_Prelude.C_True
  (<?=) C_MinError C_MaxError _ _ = Curry_Prelude.C_True
  (<?=) C_MinError C_UserDefinedError _ _ = Curry_Prelude.C_True
  (<?=) C_MinError C_ExecutionError _ _ = Curry_Prelude.C_True
  (<?=) C_MaxError C_MaxError d cs = Curry_Prelude.C_True
  (<?=) C_MaxError C_UserDefinedError _ _ = Curry_Prelude.C_True
  (<?=) C_MaxError C_ExecutionError _ _ = Curry_Prelude.C_True
  (<?=) C_UserDefinedError C_UserDefinedError d cs = Curry_Prelude.C_True
  (<?=) C_UserDefinedError C_ExecutionError _ _ = Curry_Prelude.C_True
  (<?=) C_ExecutionError C_ExecutionError d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_path'to'sqlite3 :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_path'to'sqlite3 x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '3'#) Curry_Prelude.OP_List))))))

d_C_runQ :: Curry_Prelude.Curry t0 => C_Query t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_runQ x1 x3250 x3500 = case x1 of
     (C_Query x2) -> x2
     (Choice_C_Query x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_runQ x1002 x3250 x3500) (d_C_runQ x1003 x3250 x3500)
     (Choices_C_Query x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_runQ z x3250 x3500) x1002
     (Guard_C_Query x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_runQ x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Query x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_transformQ :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> C_Query t0 -> Cover -> ConstStore -> C_Query t1
d_C_transformQ x1 x2 x3250 x3500 = C_Query (Curry_Prelude.d_OP_gt_gt_eq (d_C_runQ x2 x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return x1 x3250 x3500) x3250 x3500)

nd_C_transformQ :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 t1 -> C_Query t0 -> IDSupply -> Cover -> ConstStore -> C_Query t1
nd_C_transformQ x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (C_Query (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_runQ x2 x3250 x3500) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) x1 x2000 x3250 x3500) x2001 x3250 x3500))))))

d_C_unTrans :: Curry_Prelude.Curry t0 => C_Transaction t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (C_TransResult t0)
d_C_unTrans x1 x3250 x3500 = case x1 of
     (C_Trans x2) -> x2
     (Choice_C_Transaction x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_unTrans x1002 x3250 x3500) (d_C_unTrans x1003 x3250 x3500)
     (Choices_C_Transaction x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_unTrans z x3250 x3500) x1002
     (Guard_C_Transaction x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_unTrans x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Transaction x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_runT :: Curry_Prelude.Curry t0 => C_Transaction t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either t0 C_TError)
d_C_runT x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_beginTransaction x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_dollar d_C_catchTrans (d_C_unTrans x1 x3250 x3500) x3250 x3500) d_OP_runT_dot___hash_lambda1 x3250 x3500) x3250 x3500

d_OP_runT_dot___hash_lambda1 :: Curry_Prelude.Curry t0 => C_TransResult t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either t0 C_TError)
d_OP_runT_dot___hash_lambda1 x1 x3250 x3500 = case x1 of
     (C_Error x2) -> Curry_Prelude.d_OP_gt_gt (d_C_rollbackTransaction x3250 x3500) (Curry_Prelude.d_C_return (Curry_Prelude.C_Right x2) x3250 x3500) x3250 x3500
     (C_OK x3) -> Curry_Prelude.d_OP_gt_gt (d_C_commitTransaction x3250 x3500) (Curry_Prelude.d_C_return (Curry_Prelude.C_Left x3) x3250 x3500) x3250 x3500
     (Choice_C_TransResult x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_runT_dot___hash_lambda1 x1002 x3250 x3500) (d_OP_runT_dot___hash_lambda1 x1003 x3250 x3500)
     (Choices_C_TransResult x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_runT_dot___hash_lambda1 z x3250 x3500) x1002
     (Guard_C_TransResult x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_runT_dot___hash_lambda1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_TransResult x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_catchTrans :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IO (C_TransResult t0) -> Cover -> ConstStore -> Curry_Prelude.C_IO (C_TransResult t0)
d_C_catchTrans x1 x3250 x3500 = Curry_Prelude.d_C_catch x1 d_OP_catchTrans_dot___hash_lambda3 x3250 x3500

d_OP_catchTrans_dot___hash_lambda3 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IOError -> Cover -> ConstStore -> Curry_Prelude.C_IO (C_TransResult t0)
d_OP_catchTrans_dot___hash_lambda3 x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Global.d_C_readGlobal (d_C_lastQueryError x3250 x3500) x3250 x3500) (d_OP_catchTrans_dot___hash_lambda3_dot___hash_lambda4 x1) x3250 x3500

d_OP_catchTrans_dot___hash_lambda3_dot___hash_lambda4 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IOError -> Curry_Prelude.C_Maybe C_TError -> Cover -> ConstStore -> Curry_Prelude.C_IO (C_TransResult t0)
d_OP_catchTrans_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Global.d_C_writeGlobal (d_C_lastQueryError x3250 x3500) Curry_Prelude.C_Nothing x3250 x3500) (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (acceptCs id C_Error) x3250 x3500) (Curry_Prelude.d_C_maybe (C_TError C_ExecutionError (Curry_Prelude.d_C_showError x1 x3250 x3500)) Curry_Prelude.d_C_id x2 x3250 x3500) x3250 x3500) x3250 x3500

d_C_runJustT :: Curry_Prelude.Curry t0 => C_Transaction t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_runJustT x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_runT x1 x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (Curry_Prelude.d_C_either Curry_Prelude.d_C_id d_OP_runJustT_dot___hash_lambda5) x3250 x3500) x3250 x3500

d_OP_runJustT_dot___hash_lambda5 :: Curry_Prelude.Curry t0 => C_TError -> Cover -> ConstStore -> t0
d_OP_runJustT_dot___hash_lambda5 x1 x3250 x3500 = Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))) (d_C_showTError x1 x3250 x3500) x3250 x3500) x3250 x3500

d_C_getDB :: Curry_Prelude.Curry t0 => C_Query t0 -> Cover -> ConstStore -> C_Transaction t0
d_C_getDB x1 x3250 x3500 = Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot (acceptCs id C_Trans) d_C_catchTrans x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_runQ x1 x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (acceptCs id C_OK) x3250 x3500) x3250 x3500) x3250 x3500

d_C_transIO :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IO t0 -> Cover -> ConstStore -> C_Transaction t0
d_C_transIO x1 x3250 x3500 = C_Trans (Curry_Prelude.d_OP_gt_gt_eq x1 (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (acceptCs id C_OK) x3250 x3500) x3250 x3500)

d_C_returnT :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Transaction t0
d_C_returnT x3250 x3500 = Curry_Prelude.d_OP_dot d_C_transIO Curry_Prelude.d_C_return x3250 x3500

nd_C_returnT :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func t0 (C_Transaction t0)
nd_C_returnT x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id d_C_transIO) (wrapDX id Curry_Prelude.d_C_return) x2000 x3250 x3500))

d_C_doneT :: Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_C_doneT x3250 x3500 = d_C_transIO (Curry_Prelude.d_C_done x3250 x3500) x3250 x3500

d_C_errorT :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> C_TError -> Cover -> ConstStore -> C_Transaction t0
d_C_errorT x3250 x3500 = Curry_Prelude.d_OP_dot (acceptCs id C_Trans) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (acceptCs id C_Error) x3250 x3500) x3250 x3500

nd_C_errorT :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func C_TError (C_Transaction t0)
nd_C_errorT x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id (acceptCs id C_Trans)) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) (wrapDX id (acceptCs id C_Error)) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_failT :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_Transaction t0
d_C_failT x3250 x3500 = Curry_Prelude.d_OP_dot (d_C_errorT x3250 x3500) (acceptCs id (C_TError C_UserDefinedError)) x3250 x3500

nd_C_failT :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (C_Transaction t0)
nd_C_failT x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (nd_C_errorT x2000 x3250 x3500) (wrapDX id (acceptCs id (C_TError C_UserDefinedError))) x2001 x3250 x3500)))))

d_OP_bar_gt_gt_eq :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => C_Transaction t0 -> (t0 -> Cover -> ConstStore -> C_Transaction t1) -> Cover -> ConstStore -> C_Transaction t1
d_OP_bar_gt_gt_eq x1 x2 x3250 x3500 = case x1 of
     (C_Trans x3) -> Curry_Prelude.d_OP_dollar (acceptCs id C_Trans) (Curry_Prelude.d_OP_gt_gt_eq x3 (d_OP_bar_gt_gt_eq_dot___hash_lambda6 x2) x3250 x3500) x3250 x3500
     (Choice_C_Transaction x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_bar_gt_gt_eq x1002 x2 x3250 x3500) (d_OP_bar_gt_gt_eq x1003 x2 x3250 x3500)
     (Choices_C_Transaction x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_bar_gt_gt_eq z x2 x3250 x3500) x1002
     (Guard_C_Transaction x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_bar_gt_gt_eq x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_Transaction x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_bar_gt_gt_eq :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => C_Transaction t0 -> Func t0 (C_Transaction t1) -> IDSupply -> Cover -> ConstStore -> C_Transaction t1
nd_OP_bar_gt_gt_eq x1 x2 x3000 x3250 x3500 = case x1 of
     (C_Trans x3) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id C_Trans)) (Curry_Prelude.nd_OP_gt_gt_eq x3 (wrapNX id (nd_OP_bar_gt_gt_eq_dot___hash_lambda6 x2)) x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Choice_C_Transaction x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_bar_gt_gt_eq x1002 x2 x3000 x3250 x3500) (nd_OP_bar_gt_gt_eq x1003 x2 x3000 x3250 x3500)
     (Choices_C_Transaction x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_bar_gt_gt_eq z x2 x3000 x3250 x3500) x1002
     (Guard_C_Transaction x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_bar_gt_gt_eq x1002 x2 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Transaction x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_bar_gt_gt_eq_dot___hash_lambda6 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> Cover -> ConstStore -> C_Transaction t1) -> C_TransResult t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (C_TransResult t1)
d_OP_bar_gt_gt_eq_dot___hash_lambda6 x1 x2 x3250 x3500 = case x2 of
     (C_Error x3) -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (C_Error x3) x3250 x3500
     (C_OK x4) -> Curry_Prelude.d_OP_dollar d_C_unTrans (Curry_Prelude.d_C_apply x1 x4 x3250 x3500) x3250 x3500
     (Choice_C_TransResult x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_bar_gt_gt_eq_dot___hash_lambda6 x1 x1002 x3250 x3500) (d_OP_bar_gt_gt_eq_dot___hash_lambda6 x1 x1003 x3250 x3500)
     (Choices_C_TransResult x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_bar_gt_gt_eq_dot___hash_lambda6 x1 z x3250 x3500) x1002
     (Guard_C_TransResult x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_bar_gt_gt_eq_dot___hash_lambda6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_TransResult x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_bar_gt_gt_eq_dot___hash_lambda6 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (C_Transaction t1) -> C_TransResult t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (C_TransResult t1)
nd_OP_bar_gt_gt_eq_dot___hash_lambda6 x1 x2 x3000 x3250 x3500 = case x2 of
     (C_Error x3) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_return) (C_Error x3) x2000 x3250 x3500))
     (C_OK x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_unTrans) (Curry_Prelude.nd_C_apply x1 x4 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Choice_C_TransResult x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_bar_gt_gt_eq_dot___hash_lambda6 x1 x1002 x3000 x3250 x3500) (nd_OP_bar_gt_gt_eq_dot___hash_lambda6 x1 x1003 x3000 x3250 x3500)
     (Choices_C_TransResult x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_bar_gt_gt_eq_dot___hash_lambda6 x1 z x3000 x3250 x3500) x1002
     (Guard_C_TransResult x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_bar_gt_gt_eq_dot___hash_lambda6 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_TransResult x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_bar_gt_gt :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => C_Transaction t0 -> C_Transaction t1 -> Cover -> ConstStore -> C_Transaction t1
d_OP_bar_gt_gt x1 x2 x3250 x3500 = d_OP_bar_gt_gt_eq x1 (Curry_Prelude.d_C_const x2) x3250 x3500

d_C_sequenceT :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> Curry_Prelude.OP_List (C_Transaction t0) -> Cover -> ConstStore -> C_Transaction (Curry_Prelude.OP_List t0)
d_C_sequenceT x3250 x3500 = Curry_Prelude.d_C_foldr (acceptCs id d_OP_sequenceT_dot_seqT_dot_45) (Curry_Prelude.d_C_apply (d_C_returnT x3250 x3500) Curry_Prelude.OP_List x3250 x3500)

nd_C_sequenceT :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List (C_Transaction t0)) (C_Transaction (Curry_Prelude.OP_List t0))
nd_C_sequenceT x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapDX id) (acceptCs id d_OP_sequenceT_dot_seqT_dot_45)) (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_returnT x2000 x3250 x3500) Curry_Prelude.OP_List x2001 x3250 x3500)))))))

d_OP_sequenceT_dot_seqT_dot_45 :: Curry_Prelude.Curry t0 => C_Transaction t0 -> C_Transaction (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> C_Transaction (Curry_Prelude.OP_List t0)
d_OP_sequenceT_dot_seqT_dot_45 x1 x2 x3250 x3500 = d_OP_bar_gt_gt_eq x1 (d_OP_sequenceT_dot_seqT_dot_45_dot___hash_lambda8 x2) x3250 x3500

d_OP_sequenceT_dot_seqT_dot_45_dot___hash_lambda8 :: Curry_Prelude.Curry t0 => C_Transaction (Curry_Prelude.OP_List t0) -> t0 -> Cover -> ConstStore -> C_Transaction (Curry_Prelude.OP_List t0)
d_OP_sequenceT_dot_seqT_dot_45_dot___hash_lambda8 x1 x2 x3250 x3500 = d_OP_bar_gt_gt_eq x1 (d_OP_sequenceT_dot_seqT_dot_45_dot___hash_lambda8_dot___hash_lambda9 x2) x3250 x3500

d_OP_sequenceT_dot_seqT_dot_45_dot___hash_lambda8_dot___hash_lambda9 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> C_Transaction (Curry_Prelude.OP_List t0)
d_OP_sequenceT_dot_seqT_dot_45_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x3250 x3500 = Curry_Prelude.d_C_apply (d_C_returnT x3250 x3500) (Curry_Prelude.OP_Cons x1 x2) x3250 x3500

d_C_sequenceT_ :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> Curry_Prelude.OP_List (C_Transaction t0) -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_C_sequenceT_ x3250 x3500 = Curry_Prelude.d_C_foldr (acceptCs id d_OP_bar_gt_gt) (d_C_doneT x3250 x3500)

nd_C_sequenceT_ :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List (C_Transaction t0)) (C_Transaction Curry_Prelude.OP_Unit)
nd_C_sequenceT_ x3000 x3250 x3500 = wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapDX id) (acceptCs id d_OP_bar_gt_gt)) (d_C_doneT x3250 x3500))

d_C_mapT :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> Cover -> ConstStore -> C_Transaction t1) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> C_Transaction (Curry_Prelude.OP_List t1)
d_C_mapT x1 x3250 x3500 = Curry_Prelude.d_OP_dot (d_C_sequenceT x3250 x3500) (Curry_Prelude.d_C_map x1) x3250 x3500

nd_C_mapT :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (C_Transaction t1) -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List t0) (C_Transaction (Curry_Prelude.OP_List t1))
nd_C_mapT x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (nd_C_sequenceT x2000 x3250 x3500) (wrapNX id (Curry_Prelude.nd_C_map x1)) x2001 x3250 x3500)))))

d_C_mapT_ :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> C_Transaction t1) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_C_mapT_ x1 x3250 x3500 = Curry_Prelude.d_OP_dot (d_C_sequenceT_ x3250 x3500) (Curry_Prelude.d_C_map x1) x3250 x3500

nd_C_mapT_ :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (C_Transaction t1) -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List t0) (C_Transaction Curry_Prelude.OP_Unit)
nd_C_mapT_ x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (nd_C_sequenceT_ x2000 x3250 x3500) (wrapNX id (Curry_Prelude.nd_C_map x1)) x2001 x3250 x3500)))))

d_C_dbInfo :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_dbInfo x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 (d_C_ignored x3250 x3500) x3250 x3500) (d_C_ignored x3250 x3500) x3250 x3500
     x3 = d_OP_dbInfo_dot___hash_selFP2_hash_db x2 x3250 x3500
     x4 = d_OP_dbInfo_dot___hash_selFP3_hash_table x2 x3250 x3500
     x5 = d_OP_dbInfo_dot___hash_selFP4_hash_cols x2 x3250 x3500
      in (Curry_Prelude.OP_Tuple2 x3 (Curry_Prelude.OP_Tuple2 x4 x5))

nd_C_dbInfo :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_C_dbInfo x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2 = let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 (d_C_ignored x3250 x3500) x2000 x3250 x3500) (d_C_ignored x3250 x3500) x2001 x3250 x3500)))
          x3 = d_OP_dbInfo_dot___hash_selFP2_hash_db x2 x3250 x3500
          x4 = d_OP_dbInfo_dot___hash_selFP3_hash_table x2 x3250 x3500
          x5 = d_OP_dbInfo_dot___hash_selFP4_hash_cols x2 x3250 x3500
           in (Curry_Prelude.OP_Tuple2 x3 (Curry_Prelude.OP_Tuple2 x4 x5))))

d_OP_dbInfo_dot___hash_selFP2_hash_db :: C_Dynamic -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_dbInfo_dot___hash_selFP2_hash_db x1 x3250 x3500 = case x1 of
     (C_DBInfo x2 x3 x4) -> x2
     (Choice_C_Dynamic x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_dbInfo_dot___hash_selFP2_hash_db x1002 x3250 x3500) (d_OP_dbInfo_dot___hash_selFP2_hash_db x1003 x3250 x3500)
     (Choices_C_Dynamic x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_dbInfo_dot___hash_selFP2_hash_db z x3250 x3500) x1002
     (Guard_C_Dynamic x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_dbInfo_dot___hash_selFP2_hash_db x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Dynamic x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_dbInfo_dot___hash_selFP3_hash_table :: C_Dynamic -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_dbInfo_dot___hash_selFP3_hash_table x1 x3250 x3500 = case x1 of
     (C_DBInfo x2 x3 x4) -> x3
     (Choice_C_Dynamic x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_dbInfo_dot___hash_selFP3_hash_table x1002 x3250 x3500) (d_OP_dbInfo_dot___hash_selFP3_hash_table x1003 x3250 x3500)
     (Choices_C_Dynamic x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_dbInfo_dot___hash_selFP3_hash_table z x3250 x3500) x1002
     (Guard_C_Dynamic x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_dbInfo_dot___hash_selFP3_hash_table x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Dynamic x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_dbInfo_dot___hash_selFP4_hash_cols :: C_Dynamic -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_dbInfo_dot___hash_selFP4_hash_cols x1 x3250 x3500 = case x1 of
     (C_DBInfo x2 x3 x4) -> x4
     (Choice_C_Dynamic x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_dbInfo_dot___hash_selFP4_hash_cols x1002 x3250 x3500) (d_OP_dbInfo_dot___hash_selFP4_hash_cols x1003 x3250 x3500)
     (Choices_C_Dynamic x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_dbInfo_dot___hash_selFP4_hash_cols z x3250 x3500) x1002
     (Guard_C_Dynamic x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_dbInfo_dot___hash_selFP4_hash_cols x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Dynamic x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_ignored :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> t0
d_C_ignored x3250 x3500 = Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500

d_C_dbFile :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_dbFile x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_fst d_C_dbInfo x3250 x3500

nd_C_dbFile :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (Func Curry_Prelude.C_Int (Func t0 C_Dynamic)) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_dbFile x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_fst) (wrapNX id nd_C_dbInfo) x2000 x3250 x3500))

d_C_tableName :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_tableName x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_fst (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_snd d_C_dbInfo x3250 x3500) x3250 x3500

nd_C_tableName :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (Func Curry_Prelude.C_Int (Func t0 C_Dynamic)) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_tableName x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_fst) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_snd) (wrapNX id nd_C_dbInfo) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_colNames :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_colNames x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_snd (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_snd d_C_dbInfo x3250 x3500) x3250 x3500

nd_C_colNames :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (Func Curry_Prelude.C_Int (Func t0 C_Dynamic)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_colNames x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_snd) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_snd) (wrapNX id nd_C_dbInfo) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_persistentSQLite :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> t0 -> Cover -> ConstStore -> C_Dynamic
d_C_persistentSQLite x1 x2 x3 x4 x5 x3250 x3500 = d_OP__case_61 x3 x2 x1 (Curry_Prelude.d_C_null x3 x3250 x3500) x3250 x3500

d_C_existsDBKey :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.C_Int -> Cover -> ConstStore -> C_Query Curry_Prelude.C_Bool
d_C_existsDBKey x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar (acceptCs id C_Query) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_dollar (d_C_selectInt x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x3250 x3500) d_OP_existsDBKey_dot___hash_lambda10 x3250 x3500) x3250 x3500

nd_C_existsDBKey :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> C_Query Curry_Prelude.C_Bool
nd_C_existsDBKey x1 x2 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id C_Query)) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_C_selectInt x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x2000 x3250 x3500) (wrapDX id d_OP_existsDBKey_dot___hash_lambda10) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

d_OP_existsDBKey_dot___hash_lambda10 :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_existsDBKey_dot___hash_lambda10 x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_bang Curry_Prelude.d_C_return (Curry_Prelude.d_OP_gt x1 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500

d_C_allDBKeys :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_C_allDBKeys x1 x3250 x3500 = Curry_Prelude.d_OP_dollar (acceptCs id C_Query) (Curry_Prelude.d_OP_gt_gt_eq (d_C_selectRows x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List x3250 x3500) d_OP_allDBKeys_dot___hash_lambda11 x3250 x3500) x3250 x3500

nd_C_allDBKeys :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> IDSupply -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List Curry_Prelude.C_Int)
nd_C_allDBKeys x1 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id C_Query)) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_selectRows x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List x2000 x3250 x3500) (wrapDX id d_OP_allDBKeys_dot___hash_lambda11) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

d_OP_allDBKeys_dot___hash_lambda11 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP_allDBKeys_dot___hash_lambda11 x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO d_C_readIntOrExit x3250 x3500) x1 x3250 x3500

d_C_allDBInfos :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List t0)
d_C_allDBInfos x1 x3250 x3500 = Curry_Prelude.d_OP_dollar (acceptCs id C_Query) (Curry_Prelude.d_OP_gt_gt_eq (d_C_selectRows x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List) Curry_Prelude.OP_List x3250 x3500) d_OP_allDBInfos_dot___hash_lambda12 x3250 x3500) x3250 x3500

nd_C_allDBInfos :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> IDSupply -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List t0)
nd_C_allDBInfos x1 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id C_Query)) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_selectRows x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List) Curry_Prelude.OP_List x2000 x3250 x3500) (wrapDX id d_OP_allDBInfos_dot___hash_lambda12) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

d_OP_allDBInfos_dot___hash_lambda12 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
d_OP_allDBInfos_dot___hash_lambda12 x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_bang_bang Curry_Prelude.d_C_return (Curry_Prelude.d_C_map d_C_readInfo x1 x3250 x3500) x3250 x3500

d_C_readInfo :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> t0
d_C_readInfo x1 x3250 x3500 = Curry_Prelude.d_OP_dollar Curry_ReadShowTerm.d_C_readQTerm (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500

d_C_allDBKeyInfos :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0))
d_C_allDBKeyInfos x1 x3250 x3500 = Curry_Prelude.d_OP_dollar (acceptCs id C_Query) (Curry_Prelude.d_OP_gt_gt_eq (d_C_selectRows x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List))))))))) Curry_Prelude.OP_List x3250 x3500) d_OP_allDBKeyInfos_dot___hash_lambda13 x3250 x3500) x3250 x3500

nd_C_allDBKeyInfos :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> IDSupply -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0))
nd_C_allDBKeyInfos x1 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id C_Query)) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_selectRows x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List))))))))) Curry_Prelude.OP_List x2000 x3250 x3500) (wrapDX id d_OP_allDBKeyInfos_dot___hash_lambda13) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

d_OP_allDBKeyInfos_dot___hash_lambda13 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0))
d_OP_allDBKeyInfos_dot___hash_lambda13 x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO d_C_readKeyInfo x3250 x3500) x1 x3250 x3500

d_C_readKeyInfo :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0)
d_C_readKeyInfo x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.C_Char ','#)) x3250 x3500) x1 x3250 x3500
     x3 = d_OP_readKeyInfo_dot___hash_selFP6_hash_keyStr x2 x3250 x3500
     x4 = d_OP_readKeyInfo_dot___hash_selFP7_hash_infoStr x2 x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (d_C_readIntOrExit x3 x3250 x3500) (d_OP_readKeyInfo_dot___hash_lambda14 x4) x3250 x3500)

d_OP_readKeyInfo_dot___hash_selFP6_hash_keyStr :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_readKeyInfo_dot___hash_selFP6_hash_keyStr x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_58 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readKeyInfo_dot___hash_selFP6_hash_keyStr x1002 x3250 x3500) (d_OP_readKeyInfo_dot___hash_selFP6_hash_keyStr x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readKeyInfo_dot___hash_selFP6_hash_keyStr z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readKeyInfo_dot___hash_selFP6_hash_keyStr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_readKeyInfo_dot___hash_selFP7_hash_infoStr :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_readKeyInfo_dot___hash_selFP7_hash_infoStr x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_57 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readKeyInfo_dot___hash_selFP7_hash_infoStr x1002 x3250 x3500) (d_OP_readKeyInfo_dot___hash_selFP7_hash_infoStr x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readKeyInfo_dot___hash_selFP7_hash_infoStr z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readKeyInfo_dot___hash_selFP7_hash_infoStr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_readKeyInfo_dot___hash_lambda14 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0)
d_OP_readKeyInfo_dot___hash_lambda14 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar_bang_bang Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x2 (d_C_readInfo x1 x3250 x3500)) x3250 x3500

d_OP_at_eq :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> t0 -> Cover -> ConstStore -> C_ColVal
d_OP_at_eq x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot (acceptCs id (C_ColVal x1)) d_C_quote x3250 x3500) (Curry_ReadShowTerm.d_C_showQTerm x2 x3250 x3500) x3250 x3500

d_C_someDBKeys :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.OP_List C_ColVal -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_C_someDBKeys x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar (acceptCs id C_Query) (Curry_Prelude.d_OP_gt_gt_eq (d_C_selectSomeRows x1 x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List))))))) x3250 x3500) d_OP_someDBKeys_dot___hash_lambda15 x3250 x3500) x3250 x3500

nd_C_someDBKeys :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.OP_List C_ColVal -> IDSupply -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List Curry_Prelude.C_Int)
nd_C_someDBKeys x1 x2 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id C_Query)) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_selectSomeRows x1 x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List))))))) x2000 x3250 x3500) (wrapDX id d_OP_someDBKeys_dot___hash_lambda15) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

d_OP_someDBKeys_dot___hash_lambda15 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP_someDBKeys_dot___hash_lambda15 x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO d_C_readIntOrExit x3250 x3500) x1 x3250 x3500

d_C_someDBInfos :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.OP_List C_ColVal -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List t0)
d_C_someDBInfos x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar (acceptCs id C_Query) (Curry_Prelude.d_OP_gt_gt_eq (d_C_selectSomeRows x1 x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List) x3250 x3500) d_OP_someDBInfos_dot___hash_lambda16 x3250 x3500) x3250 x3500

nd_C_someDBInfos :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.OP_List C_ColVal -> IDSupply -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List t0)
nd_C_someDBInfos x1 x2 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id C_Query)) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_selectSomeRows x1 x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List) x2000 x3250 x3500) (wrapDX id d_OP_someDBInfos_dot___hash_lambda16) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

d_OP_someDBInfos_dot___hash_lambda16 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
d_OP_someDBInfos_dot___hash_lambda16 x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_bang_bang Curry_Prelude.d_C_return (Curry_Prelude.d_C_map d_C_readInfo x1 x3250 x3500) x3250 x3500

d_C_someDBKeyInfos :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.OP_List C_ColVal -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0))
d_C_someDBKeyInfos x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar (acceptCs id C_Query) (Curry_Prelude.d_OP_gt_gt_eq (d_C_selectSomeRows x1 x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List))))))))) x3250 x3500) d_OP_someDBKeyInfos_dot___hash_lambda17 x3250 x3500) x3250 x3500

nd_C_someDBKeyInfos :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.OP_List C_ColVal -> IDSupply -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0))
nd_C_someDBKeyInfos x1 x2 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id C_Query)) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_selectSomeRows x1 x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List))))))))) x2000 x3250 x3500) (wrapDX id d_OP_someDBKeyInfos_dot___hash_lambda17) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

d_OP_someDBKeyInfos_dot___hash_lambda17 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0))
d_OP_someDBKeyInfos_dot___hash_lambda17 x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO d_C_readKeyInfo x3250 x3500) x1 x3250 x3500

d_C_someDBKeyProjections :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List C_ColVal -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t1))
d_C_someDBKeyProjections x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_dollar (acceptCs id C_Query) (let
     x4 = Curry_Prelude.d_C_apply (d_C_commaSep x3250 x3500) (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_bang_bang (Curry_Prelude.d_C_apply (d_C_colNames x3250 x3500) x1 x3250 x3500)) x2 x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (d_C_selectSomeRows x1 x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List)))))))) x4 x3250 x3500) x3250 x3500) d_OP_someDBKeyProjections_dot___hash_lambda18 x3250 x3500)) x3250 x3500

nd_C_someDBKeyProjections :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List C_ColVal -> IDSupply -> Cover -> ConstStore -> C_Query (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t1))
nd_C_someDBKeyProjections x1 x2 x3 x3000 x3250 x3500 = let
     x2014 = x3000
      in (seq x2014 (let
          x2013 = leftSupply x2014
          x2012 = rightSupply x2014
           in (seq x2013 (seq x2012 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id C_Query)) (let
               x2007 = leftSupply x2012
               x2011 = rightSupply x2012
                in (seq x2007 (seq x2011 (let
                    x4 = let
                         x2006 = leftSupply x2007
                         x2008 = rightSupply x2007
                          in (seq x2006 (seq x2008 (let
                              x2000 = leftSupply x2008
                              x2005 = rightSupply x2008
                               in (seq x2000 (seq x2005 (Curry_Prelude.nd_C_apply (nd_C_commaSep x2000 x3250 x3500) (let
                                   x2004 = leftSupply x2005
                                   x2003 = rightSupply x2005
                                    in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_map (wrapDX id (Curry_Prelude.d_OP_bang_bang (let
                                        x2002 = leftSupply x2003
                                        x2001 = rightSupply x2003
                                         in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_colNames x2001 x3250 x3500) x1 x2002 x3250 x3500)))))) x2 x2004 x3250 x3500)))) x2006 x3250 x3500))))))
                     in (let
                         x2010 = leftSupply x2011
                         x2009 = rightSupply x2011
                          in (seq x2010 (seq x2009 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_selectSomeRows x1 x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List)))))))) x4 x3250 x3500) x2009 x3250 x3500) (wrapDX id d_OP_someDBKeyProjections_dot___hash_lambda18) x2010 x3250 x3500)))))))) x2013 x3250 x3500)))))

d_OP_someDBKeyProjections_dot___hash_lambda18 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0))
d_OP_someDBKeyProjections_dot___hash_lambda18 x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO d_C_readKeyInfo x3250 x3500) x1 x3250 x3500

d_C_getDBInfo :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.C_Int -> Cover -> ConstStore -> C_Query (Curry_Prelude.C_Maybe t0)
d_C_getDBInfo x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar (acceptCs id C_Query) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_dollar (d_C_selectRows x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x3250 x3500) d_OP_getDBInfo_dot___hash_lambda19 x3250 x3500) x3250 x3500

nd_C_getDBInfo :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> C_Query (Curry_Prelude.C_Maybe t0)
nd_C_getDBInfo x1 x2 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id C_Query)) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_C_selectRows x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x2000 x3250 x3500) (wrapDX id d_OP_getDBInfo_dot___hash_lambda19) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

d_OP_getDBInfo_dot_readHeadIfExists_dot_108 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe t0)
d_OP_getDBInfo_dot_readHeadIfExists_dot_108 x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3250 x3500
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_OP_dollar_bang_bang Curry_Prelude.d_C_return (Curry_Prelude.C_Just (d_C_readInfo x2 x3250 x3500)) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getDBInfo_dot_readHeadIfExists_dot_108 x1002 x3250 x3500) (d_OP_getDBInfo_dot_readHeadIfExists_dot_108 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getDBInfo_dot_readHeadIfExists_dot_108 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getDBInfo_dot_readHeadIfExists_dot_108 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getDBInfo_dot___hash_lambda19 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe t0)
d_OP_getDBInfo_dot___hash_lambda19 x1 x3250 x3500 = d_OP_getDBInfo_dot_readHeadIfExists_dot_108 x1 x3250 x3500

d_C_getDBInfos :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> C_Query (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List t0))
d_C_getDBInfos x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar (acceptCs id C_Query) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_dollar (d_C_selectRows x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_commaSep x3250 x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) (d_OP_getDBInfos_dot___hash_lambda22 x2) x3250 x3500) x3250 x3500

nd_C_getDBInfos :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> C_Query (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List t0))
nd_C_getDBInfos x1 x2 x3000 x3250 x3500 = let
     x2010 = x3000
      in (seq x2010 (let
          x2009 = leftSupply x2010
          x2008 = rightSupply x2010
           in (seq x2009 (seq x2008 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id C_Query)) (let
               x2007 = leftSupply x2008
               x2006 = rightSupply x2008
                in (seq x2007 (seq x2006 (Curry_Prelude.nd_OP_gt_gt_eq (let
                    x2005 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2005 (seq x2003 (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_C_selectRows x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (let
                         x2002 = leftSupply x2003
                         x2004 = rightSupply x2003
                          in (seq x2002 (seq x2004 (let
                              x2000 = leftSupply x2004
                              x2001 = rightSupply x2004
                               in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_commaSep x2000 x3250 x3500) (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_show) x2 x2001 x3250 x3500) x2002 x3250 x3500))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x2005 x3250 x3500)))) (wrapDX id (d_OP_getDBInfos_dot___hash_lambda22 x2)) x2007 x3250 x3500)))) x2009 x3250 x3500)))))

d_OP_getDBInfos_dot_sortByIndexInGivenList_dot_116 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List t0))
d_OP_getDBInfos_dot_sortByIndexInGivenList_dot_116 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO d_C_readKeyInfo x3250 x3500) x2 x3250 x3500) (d_OP_getDBInfos_dot_sortByIndexInGivenList_dot_116_dot___hash_lambda20 x1) x3250 x3500

d_OP_getDBInfos_dot_sortByIndexInGivenList_dot_116_dot___hash_lambda20 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List t0))
d_OP_getDBInfos_dot_sortByIndexInGivenList_dot_116_dot___hash_lambda20 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_Prelude.d_C_apply (Curry_Maybe.d_C_mapMMaybe (d_OP_getDBInfos_dot_sortByIndexInGivenList_dot_116_dot___hash_lambda20_dot___hash_lambda21 x2) x3250 x3500) x1 x3250 x3500) x3250 x3500

d_OP_getDBInfos_dot_sortByIndexInGivenList_dot_116_dot___hash_lambda20_dot___hash_lambda21 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int t0) -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t0
d_OP_getDBInfos_dot_sortByIndexInGivenList_dot_116_dot___hash_lambda20_dot___hash_lambda21 x1 x2 x3250 x3500 = Curry_Prelude.d_C_lookup x2 x1 x3250 x3500

d_OP_getDBInfos_dot___hash_lambda22 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List t0))
d_OP_getDBInfos_dot___hash_lambda22 x1 x2 x3250 x3500 = d_OP_getDBInfos_dot_sortByIndexInGivenList_dot_116 x1 x2 x3250 x3500

d_C_commaSep :: Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_commaSep x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) x3250 x3500

nd_C_commaSep :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_commaSep x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_concat) (wrapDX id (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) x2000 x3250 x3500))

d_C_deleteDBEntry :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.C_Int -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_C_deleteDBEntry x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar (d_C_modify x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x3250 x3500

nd_C_deleteDBEntry :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
nd_C_deleteDBEntry x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_C_modify x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x2000 x3250 x3500))

d_C_deleteDBEntries :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_C_deleteDBEntries x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar (d_C_modify x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_commaSep x3250 x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500

nd_C_deleteDBEntries :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
nd_C_deleteDBEntries x1 x2 x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2003 = rightSupply x2006
           in (seq x2005 (seq x2003 (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_C_modify x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_commaSep x2000 x3250 x3500) (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_show) x2 x2001 x3250 x3500) x2002 x3250 x3500))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x2005 x3250 x3500)))))

d_C_updateDBEntry :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.C_Int -> t0 -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_C_updateDBEntry x1 x2 x3 x3250 x3500 = d_OP_bar_gt_gt (d_C_errorUnlessKeyExists x1 x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x3250 x3500) (d_C_modify x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_commaSep x3250 x3500) (d_C_colVals x1 x3 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

nd_C_updateDBEntry :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.C_Int -> t0 -> IDSupply -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
nd_C_updateDBEntry x1 x2 x3 x3000 x3250 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2000 = leftSupply x2008
          x2007 = rightSupply x2008
           in (seq x2000 (seq x2007 (d_OP_bar_gt_gt (nd_C_errorUnlessKeyExists x1 x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x2000 x3250 x3500) (let
               x2006 = leftSupply x2007
               x2004 = rightSupply x2007
                in (seq x2006 (seq x2004 (nd_C_modify x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (let
                    x2003 = leftSupply x2004
                    x2005 = rightSupply x2004
                     in (seq x2003 (seq x2005 (let
                         x2001 = leftSupply x2005
                         x2002 = rightSupply x2005
                          in (seq x2001 (seq x2002 (Curry_Prelude.nd_C_apply (nd_C_commaSep x2001 x3250 x3500) (nd_C_colVals x1 x3 x2002 x3250 x3500) x2003 x3250 x3500))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x2006 x3250 x3500)))) x3250 x3500)))))

d_C_errorUnlessKeyExists :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_C_errorUnlessKeyExists x1 x2 x3 x3250 x3500 = d_OP_bar_gt_gt_eq (d_C_getDB (d_C_existsDBKey x1 x2 x3250 x3500) x3250 x3500) (d_OP_errorUnlessKeyExists_dot___hash_lambda23 x3) x3250 x3500

nd_C_errorUnlessKeyExists :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
nd_C_errorUnlessKeyExists x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_bar_gt_gt_eq (d_C_getDB (nd_C_existsDBKey x1 x2 x2000 x3250 x3500) x3250 x3500) (wrapDX id (d_OP_errorUnlessKeyExists_dot___hash_lambda23 x3)) x2001 x3250 x3500)))))

d_OP_errorUnlessKeyExists_dot___hash_lambda23 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_OP_errorUnlessKeyExists_dot___hash_lambda23 x1 x2 x3250 x3500 = d_OP__case_56 x2 x1 (Curry_Prelude.d_C_not x2 x3250 x3500) x3250 x3500

d_C_colVals :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_colVals x1 x2 x3250 x3500 = Curry_Prelude.d_C_zipWith (acceptCs id d_OP_colVals_dot___hash_lambda24) (Curry_Prelude.d_C_apply (d_C_colNames x3250 x3500) x1 x3250 x3500) (d_C_infoVals x1 x2 x3250 x3500) x3250 x3500

nd_C_colVals :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_colVals x1 x2 x3000 x3250 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2004 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2004 (seq x2006 (let
               x2002 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2002 (seq x2003 (Curry_Prelude.nd_C_zipWith (wrapDX (wrapDX id) (acceptCs id d_OP_colVals_dot___hash_lambda24)) (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_colNames x2000 x3250 x3500) x1 x2001 x3250 x3500)))) (nd_C_infoVals x1 x2 x2003 x3250 x3500) x2004 x3250 x3500))))))))

d_OP_colVals_dot___hash_lambda24 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_colVals_dot___hash_lambda24 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) x2 x3250 x3500) x3250 x3500

d_C_infoVals :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_infoVals x1 x2 x3250 x3500 = d_OP__case_55 x1 x2 (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_null Curry_Prelude.d_C_tail x3250 x3500) (Curry_Prelude.d_C_apply (d_C_colNames x3250 x3500) x1 x3250 x3500) x3250 x3500) x3250 x3500

nd_C_infoVals :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_infoVals x1 x2 x3000 x3250 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2007 = leftSupply x2008
          x2005 = rightSupply x2008
           in (seq x2007 (seq x2005 (nd_OP__case_55 x1 x2 (let
               x2004 = leftSupply x2005
               x2006 = rightSupply x2005
                in (seq x2004 (seq x2006 (let
                    x2000 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2000 (seq x2003 (Curry_Prelude.nd_OP_dollar (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_null) (wrapDX id Curry_Prelude.d_C_tail) x2000 x3250 x3500) (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_colNames x2001 x3250 x3500) x1 x2002 x3250 x3500)))) x2004 x3250 x3500))))))) x2007 x3250 x3500)))))

d_C_quote :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_quote x1 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_quote_dot_quoteChar_dot_139 x3250 x3500) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500

d_OP_quote_dot_quoteChar_dot_139 :: Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_quote_dot_quoteChar_dot_139 x1 x3250 x3500 = d_OP__case_53 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500

d_C_newDBEntry :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> t0 -> Cover -> ConstStore -> C_Transaction Curry_Prelude.C_Int
d_C_newDBEntry x1 x2 x3250 x3500 = d_OP_bar_gt_gt (d_C_modify x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_commaSep x3250 x3500) (d_C_infoVals x1 x2 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) (d_C_getDB (Curry_Prelude.d_OP_dollar (acceptCs id C_Query) (d_C_selectInt x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))))))))))))))))))) Curry_Prelude.OP_List x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

nd_C_newDBEntry :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> t0 -> IDSupply -> Cover -> ConstStore -> C_Transaction Curry_Prelude.C_Int
nd_C_newDBEntry x1 x2 x3000 x3250 x3500 = let
     x2010 = x3000
      in (seq x2010 (let
          x2006 = leftSupply x2010
          x2009 = rightSupply x2010
           in (seq x2006 (seq x2009 (d_OP_bar_gt_gt (let
               x2005 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2005 (seq x2003 (nd_C_modify x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (let
                    x2002 = leftSupply x2003
                    x2004 = rightSupply x2003
                     in (seq x2002 (seq x2004 (let
                         x2000 = leftSupply x2004
                         x2001 = rightSupply x2004
                          in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_commaSep x2000 x3250 x3500) (nd_C_infoVals x1 x2 x2001 x3250 x3500) x2002 x3250 x3500))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x2005 x3250 x3500)))) (d_C_getDB (let
               x2008 = leftSupply x2009
               x2007 = rightSupply x2009
                in (seq x2008 (seq x2007 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id C_Query)) (nd_C_selectInt x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))))))))))))))))))) Curry_Prelude.OP_List x2007 x3250 x3500) x2008 x3250 x3500)))) x3250 x3500) x3250 x3500)))))

d_C_newDBKeyEntry :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.C_Int -> t0 -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_C_newDBKeyEntry x1 x2 x3 x3250 x3500 = d_OP_bar_gt_gt_eq (d_C_getDB (d_C_existsDBKey x1 x2 x3250 x3500) x3250 x3500) (d_OP_newDBKeyEntry_dot___hash_lambda25 x3 x2 x1) x3250 x3500

nd_C_newDBKeyEntry :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.C_Int -> t0 -> IDSupply -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
nd_C_newDBKeyEntry x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_bar_gt_gt_eq (d_C_getDB (nd_C_existsDBKey x1 x2 x2000 x3250 x3500) x3250 x3500) (wrapNX id (nd_OP_newDBKeyEntry_dot___hash_lambda25 x3 x2 x1)) x2001 x3250 x3500)))))

d_OP_newDBKeyEntry_dot___hash_lambda25 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_OP_newDBKeyEntry_dot___hash_lambda25 x1 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot (d_C_errorT x3250 x3500) (acceptCs id (C_TError C_DuplicateKeyError)) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP_bar_gt_gt_eq (d_OP_bar_gt_gt (d_C_modify x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_commaSep x3250 x3500) (d_C_infoVals x3 x1 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) (d_C_getDB (Curry_Prelude.d_OP_dollar (acceptCs id C_Query) (d_C_selectInt x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))))))))))))))))))) Curry_Prelude.OP_List x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (d_OP_newDBKeyEntry_dot___hash_lambda25_dot___hash_lambda26 x2 x3) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_newDBKeyEntry_dot___hash_lambda25 x1 x2 x3 x1002 x3250 x3500) (d_OP_newDBKeyEntry_dot___hash_lambda25 x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_newDBKeyEntry_dot___hash_lambda25 x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_newDBKeyEntry_dot___hash_lambda25 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_newDBKeyEntry_dot___hash_lambda25 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
nd_OP_newDBKeyEntry_dot___hash_lambda25 x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (nd_C_errorT x2000 x3250 x3500) (wrapDX id (acceptCs id (C_TError C_DuplicateKeyError))) x2001 x3250 x3500)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x2003 x3250 x3500)))))
     Curry_Prelude.C_False -> let
          x2012 = x3000
           in (seq x2012 (let
               x2011 = leftSupply x2012
               x2010 = rightSupply x2012
                in (seq x2011 (seq x2010 (nd_OP_bar_gt_gt_eq (let
                    x2006 = leftSupply x2010
                    x2009 = rightSupply x2010
                     in (seq x2006 (seq x2009 (d_OP_bar_gt_gt (let
                         x2005 = leftSupply x2006
                         x2003 = rightSupply x2006
                          in (seq x2005 (seq x2003 (nd_C_modify x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (let
                              x2002 = leftSupply x2003
                              x2004 = rightSupply x2003
                               in (seq x2002 (seq x2004 (let
                                   x2000 = leftSupply x2004
                                   x2001 = rightSupply x2004
                                    in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_commaSep x2000 x3250 x3500) (nd_C_infoVals x3 x1 x2001 x3250 x3500) x2002 x3250 x3500))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x2005 x3250 x3500)))) (d_C_getDB (let
                         x2008 = leftSupply x2009
                         x2007 = rightSupply x2009
                          in (seq x2008 (seq x2007 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id C_Query)) (nd_C_selectInt x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))))))))))))))))))) Curry_Prelude.OP_List x2007 x3250 x3500) x2008 x3250 x3500)))) x3250 x3500) x3250 x3500)))) (wrapNX id (nd_OP_newDBKeyEntry_dot___hash_lambda25_dot___hash_lambda26 x2 x3)) x2011 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_newDBKeyEntry_dot___hash_lambda25 x1 x2 x3 x1002 x3000 x3250 x3500) (nd_OP_newDBKeyEntry_dot___hash_lambda25 x1 x2 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_newDBKeyEntry_dot___hash_lambda25 x1 x2 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_newDBKeyEntry_dot___hash_lambda25 x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_newDBKeyEntry_dot___hash_lambda25_dot___hash_lambda26 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.C_Int -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_OP_newDBKeyEntry_dot___hash_lambda25_dot___hash_lambda26 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_dollar (d_C_modify x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_C_show x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

nd_OP_newDBKeyEntry_dot___hash_lambda25_dot___hash_lambda26 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
nd_OP_newDBKeyEntry_dot___hash_lambda25_dot___hash_lambda26 x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_C_modify x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_C_show x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))

d_C_cleanDB :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_C_cleanDB x1 x3250 x3500 = d_C_modify x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List))))))))))) Curry_Prelude.OP_List x3250 x3500

nd_C_cleanDB :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> IDSupply -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
nd_C_cleanDB x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_modify x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List))))))))))) Curry_Prelude.OP_List x2000 x3250 x3500))

d_C_sqlite3 :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
d_C_sqlite3 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getDBHandle x1 x3250 x3500) (d_OP_sqlite3_dot___hash_lambda27 x2) x3250 x3500

nd_C_sqlite3 :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
nd_C_sqlite3 x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getDBHandle x1 x2000 x3250 x3500) (wrapDX id (d_OP_sqlite3_dot___hash_lambda27 x2)) x2001 x3250 x3500)))))

d_OP_sqlite3_dot___hash_lambda27 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
d_OP_sqlite3_dot___hash_lambda27 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (d_C_hPutAndFlush x2) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return x2 x3250 x3500) x3250 x3500

d_C_hPutAndFlush :: Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hPutAndFlush x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_IO.d_C_hPutStrLn x1 x2 x3250 x3500) (Curry_IO.d_C_hFlush x1 x3250 x3500) x3250 x3500

d_C_modify :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_C_modify x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_dollar d_C_transIO (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (d_C_sqlite3 x1) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_tableName x3250 x3500) x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_done x3250 x3500) x3250 x3500) x3250 x3500

nd_C_modify :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
nd_C_modify x1 x2 x3 x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_transIO) (Curry_Prelude.d_OP_gt_gt (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_C_sqlite3 x1)) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_tableName x2000 x3250 x3500) x1 x2001 x3250 x3500)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x2003 x3250 x3500)))) (Curry_Prelude.d_C_done x3250 x3500) x3250 x3500) x2005 x3250 x3500)))))

d_C_selectInt :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_selectInt x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_dollar (d_C_sqlite3 x1) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_tableName x3250 x3500) x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) d_OP_selectInt_dot___hash_lambda28 x3250 x3500

nd_C_selectInt :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
nd_C_selectInt x1 x2 x3 x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (Curry_Prelude.nd_OP_gt_gt_eq (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_C_sqlite3 x1)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_tableName x2000 x3250 x3500) x1 x2001 x3250 x3500)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x2003 x3250 x3500)))) (wrapDX id d_OP_selectInt_dot___hash_lambda28) x2005 x3250 x3500)))))

d_OP_selectInt_dot___hash_lambda28 :: Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_OP_selectInt_dot___hash_lambda28 x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x1 x3250 x3500) d_C_readIntOrExit x3250 x3500

d_C_readIntOrExit :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_readIntOrExit x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_OP_dollar (d_C_dbError C_ExecutionError) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_maybe x2 (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return Curry_Prelude.d_C_fst x3250 x3500)) (Curry_ReadNumeric.d_C_readInt x1 x3250 x3500) x3250 x3500)

d_C_selectRows :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_selectRows x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_sqlite3 x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '8'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))))))))))))))))))))))))) x3250 x3500) (d_OP_selectRows_dot___hash_lambda29 x2 x3 x1) x3250 x3500

nd_C_selectRows :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_selectRows x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_sqlite3 x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '8'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))))))))))))))))))))))))) x2000 x3250 x3500) (wrapNX id (nd_OP_selectRows_dot___hash_lambda29 x2 x3 x1)) x2001 x3250 x3500)))))

d_OP_selectRows_dot___hash_lambda29 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_selectRows_dot___hash_lambda29 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x4 x3250 x3500) (d_OP_selectRows_dot___hash_lambda29_dot___hash_lambda30 x1 x2 x4 x3) x3250 x3500

nd_OP_selectRows_dot___hash_lambda29 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_IO.C_Handle -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_selectRows_dot___hash_lambda29 x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x4 x3250 x3500) (wrapNX id (nd_OP_selectRows_dot___hash_lambda29_dot___hash_lambda30 x1 x2 x4 x3)) x2000 x3250 x3500))

d_OP_selectRows_dot___hash_lambda29_dot___hash_lambda30 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_IO.C_Handle -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_selectRows_dot___hash_lambda29_dot___hash_lambda30 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (d_C_hPutAndFlush x3) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_tableName x3250 x3500) x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (d_C_quote x5 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (d_C_hGetLinesBefore x3 x5 x3250 x3500) x3250 x3500

nd_OP_selectRows_dot___hash_lambda29_dot___hash_lambda30 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_IO.C_Handle -> Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_selectRows_dot___hash_lambda29_dot___hash_lambda30 x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (Curry_Prelude.d_OP_gt_gt (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id (d_C_hPutAndFlush x3)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_tableName x2000 x3250 x3500) x4 x2001 x3250 x3500)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (d_C_quote x5 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x2003 x3250 x3500)))) (d_C_hGetLinesBefore x3 x5 x3250 x3500) x3250 x3500))

d_C_hGetLinesBefore :: Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_hGetLinesBefore x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x1 x3250 x3500) (d_OP_hGetLinesBefore_dot___hash_lambda31 x1 x2) x3250 x3500

d_OP_hGetLinesBefore_dot___hash_lambda31 :: Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_hGetLinesBefore_dot___hash_lambda31 x1 x2 x3 x3250 x3500 = d_OP__case_52 x2 x3 x1 (Curry_Prelude.d_OP_eq_eq x3 x2 x3250 x3500) x3250 x3500

d_OP_hGetLinesBefore_dot___hash_lambda31_dot___hash_lambda32 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_hGetLinesBefore_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons x1 x2) x3250 x3500

d_C_selectSomeRows :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.OP_List C_ColVal -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_selectSomeRows x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_dollar (d_C_selectRows x1 x3) (d_OP__case_51 x2 x1 (Curry_Prelude.d_C_null x2 x3250 x3500) x3250 x3500) x3250 x3500

nd_C_selectSomeRows :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.OP_List C_ColVal -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_selectSomeRows x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_C_selectRows x1 x3)) (nd_OP__case_51 x2 x1 (Curry_Prelude.d_C_null x2 x3250 x3500) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_showColVals :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.OP_List C_ColVal -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showColVals x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) x3250 x3500) (Curry_Prelude.d_C_map (d_OP_showColVals_dot_showCV_dot_177 x1) (Curry_Prelude.OP_Cons x3 x4) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showColVals x1 x1002 x3250 x3500) (d_C_showColVals x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showColVals x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showColVals x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_showColVals :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.OP_List C_ColVal -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showColVals x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_dollar (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_concat) (wrapDX id (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) x2000 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_showColVals_dot_showCV_dot_177 x1)) (Curry_Prelude.OP_Cons x3 x4) x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showColVals x1 x1002 x3000 x3250 x3500) (nd_C_showColVals x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showColVals x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showColVals x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showColVals_dot_showCV_dot_177 :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> C_ColVal -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showColVals_dot_showCV_dot_177 x1 x2 x3250 x3500 = case x2 of
     (C_ColVal x3 x4) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_OP_bang_bang (Curry_Prelude.d_C_apply (d_C_colNames x3250 x3500) x1 x3250 x3500) x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) x4 x3250 x3500) x3250 x3500
     (Choice_C_ColVal x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showColVals_dot_showCV_dot_177 x1 x1002 x3250 x3500) (d_OP_showColVals_dot_showCV_dot_177 x1 x1003 x3250 x3500)
     (Choices_C_ColVal x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showColVals_dot_showCV_dot_177 x1 z x3250 x3500) x1002
     (Guard_C_ColVal x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showColVals_dot_showCV_dot_177 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ColVal x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_showColVals_dot_showCV_dot_177 :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> C_ColVal -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_showColVals_dot_showCV_dot_177 x1 x2 x3000 x3250 x3500 = case x2 of
     (C_ColVal x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_OP_bang_bang (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_colNames x2000 x3250 x3500) x1 x2001 x3250 x3500)))) x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) x4 x3250 x3500) x3250 x3500))
     (Choice_C_ColVal x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_showColVals_dot_showCV_dot_177 x1 x1002 x3000 x3250 x3500) (nd_OP_showColVals_dot_showCV_dot_177 x1 x1003 x3000 x3250 x3500)
     (Choices_C_ColVal x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_showColVals_dot_showCV_dot_177 x1 z x3000 x3250 x3500) x1002
     (Guard_C_ColVal x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_showColVals_dot_showCV_dot_177 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_ColVal x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_closeDBHandles :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_closeDBHandles x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_withAllDBHandles Curry_IO.d_C_hClose x3250 x3500) (Curry_Global.d_C_writeGlobal (d_C_openDBHandles x3250 x3500) Curry_Prelude.OP_List x3250 x3500) x3250 x3500

d_C_dbError :: Curry_Prelude.Curry t0 => C_TErrorKind -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_dbError x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot (Curry_Global.d_C_writeGlobal (d_C_lastQueryError x3250 x3500)) (acceptCs id Curry_Prelude.C_Just) x3250 x3500) (C_TError x1 x2) x3250 x3500) (Curry_Prelude.d_C_error x2 x3250 x3500) x3250 x3500

d_C_lastQueryError :: Cover -> ConstStore -> Curry_Global.C_Global (Curry_Prelude.C_Maybe C_TError)
d_C_lastQueryError x0 x1 = global_C_lastQueryError

global_C_lastQueryError :: Curry_Global.C_Global (Curry_Prelude.C_Maybe C_TError)
global_C_lastQueryError = Curry_Global.d_C_global (let
     x3500 = emptyCs
     x3250 = initCover
      in Curry_Prelude.C_Nothing) Curry_Global.C_Temporary initCover emptyCs

d_C_getDBHandle :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
d_C_getDBHandle x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_ensureDBFor x1 x3250 x3500) (Curry_Prelude.d_OP_dollar d_C_readDBHandle (Curry_Prelude.d_C_apply (d_C_dbFile x3250 x3500) x1 x3250 x3500) x3250 x3500) x3250 x3500

nd_C_getDBHandle :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
nd_C_getDBHandle x1 x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2000 = leftSupply x2006
          x2005 = rightSupply x2006
           in (seq x2000 (seq x2005 (Curry_Prelude.d_OP_gt_gt (nd_C_ensureDBFor x1 x2000 x3250 x3500) (let
               x2004 = leftSupply x2005
               x2003 = rightSupply x2005
                in (seq x2004 (seq x2003 (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_readDBHandle) (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_dbFile x2001 x3250 x3500) x1 x2002 x3250 x3500)))) x2004 x3250 x3500)))) x3250 x3500)))))

d_C_ensureDBFor :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_ensureDBFor x1 x3250 x3500 = let
     x2 = d_C_dbInfo x1 x3250 x3500
     x3 = d_OP_ensureDBFor_dot___hash_selFP9_hash_db x2 x3250 x3500
     x4 = d_OP_ensureDBFor_dot___hash_selFP10_hash_table x2 x3250 x3500
     x5 = d_OP_ensureDBFor_dot___hash_selFP11_hash_cols x2 x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt (d_C_ensureDBHandle x3 x3250 x3500) (d_C_ensureDBTable x3 x4 x5 x3250 x3500) x3250 x3500)

nd_C_ensureDBFor :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_ensureDBFor x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x2 = nd_C_dbInfo x1 x2000 x3250 x3500
          x3 = d_OP_ensureDBFor_dot___hash_selFP9_hash_db x2 x3250 x3500
          x4 = d_OP_ensureDBFor_dot___hash_selFP10_hash_table x2 x3250 x3500
          x5 = d_OP_ensureDBFor_dot___hash_selFP11_hash_cols x2 x3250 x3500
           in (Curry_Prelude.d_OP_gt_gt (d_C_ensureDBHandle x3 x3250 x3500) (d_C_ensureDBTable x3 x4 x5 x3250 x3500) x3250 x3500)))

d_OP_ensureDBFor_dot___hash_selFP9_hash_db :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_ensureDBFor_dot___hash_selFP9_hash_db x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_50 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_ensureDBFor_dot___hash_selFP9_hash_db x1002 x3250 x3500) (d_OP_ensureDBFor_dot___hash_selFP9_hash_db x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_ensureDBFor_dot___hash_selFP9_hash_db z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_ensureDBFor_dot___hash_selFP9_hash_db x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_ensureDBFor_dot___hash_selFP10_hash_table :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_ensureDBFor_dot___hash_selFP10_hash_table x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_49 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_ensureDBFor_dot___hash_selFP10_hash_table x1002 x3250 x3500) (d_OP_ensureDBFor_dot___hash_selFP10_hash_table x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_ensureDBFor_dot___hash_selFP10_hash_table z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_ensureDBFor_dot___hash_selFP10_hash_table x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_ensureDBFor_dot___hash_selFP11_hash_cols :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_ensureDBFor_dot___hash_selFP11_hash_cols x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_48 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_ensureDBFor_dot___hash_selFP11_hash_cols x1002 x3250 x3500) (d_OP_ensureDBFor_dot___hash_selFP11_hash_cols x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_ensureDBFor_dot___hash_selFP11_hash_cols z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_ensureDBFor_dot___hash_selFP11_hash_cols x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_readDBHandle :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
d_C_readDBHandle x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_OP_dollar (d_C_dbError C_ExecutionError) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Global.d_C_readGlobal (d_C_openDBHandles x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_maybe x2 Curry_Prelude.d_C_return) (Curry_Prelude.d_C_lookup x1) x3250 x3500) x3250 x3500)

d_C_openDBHandles :: Cover -> ConstStore -> Curry_Global.C_Global (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle))
d_C_openDBHandles x0 x1 = global_C_openDBHandles

global_C_openDBHandles :: Curry_Global.C_Global (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle))
global_C_openDBHandles = Curry_Global.d_C_global (let
     x3500 = emptyCs
     x3250 = initCover
      in Curry_Prelude.OP_List) Curry_Global.C_Temporary initCover emptyCs

d_C_withAllDBHandles :: Curry_Prelude.Curry t0 => (Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO t0) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_withAllDBHandles x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Global.d_C_readGlobal (d_C_openDBHandles x3250 x3500) x3250 x3500) (d_OP_withAllDBHandles_dot___hash_lambda33 x1) x3250 x3500

nd_C_withAllDBHandles :: Curry_Prelude.Curry t0 => Func Curry_IO.C_Handle (Curry_Prelude.C_IO t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_withAllDBHandles x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Global.d_C_readGlobal (d_C_openDBHandles x3250 x3500) x3250 x3500) (wrapNX id (nd_OP_withAllDBHandles_dot___hash_lambda33 x1)) x2000 x3250 x3500))

d_OP_withAllDBHandles_dot___hash_lambda33 :: Curry_Prelude.Curry t0 => (Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_withAllDBHandles_dot___hash_lambda33 x1 x2 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO_ (Curry_Prelude.d_OP_dot x1 Curry_Prelude.d_C_snd x3250 x3500) x3250 x3500) x2 x3250 x3500

nd_OP_withAllDBHandles_dot___hash_lambda33 :: Curry_Prelude.Curry t0 => Func Curry_IO.C_Handle (Curry_Prelude.C_IO t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_withAllDBHandles_dot___hash_lambda33 x1 x2 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_mapIO_ (Curry_Prelude.nd_OP_dot x1 (wrapDX id Curry_Prelude.d_C_snd) x2000 x3250 x3500) x2001 x3250 x3500)))) x2 x2003 x3250 x3500)))))

d_C_ensureDBHandle :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_ensureDBHandle x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Global.d_C_readGlobal (d_C_openDBHandles x3250 x3500) x3250 x3500) (d_OP_ensureDBHandle_dot___hash_lambda36 x1) x3250 x3500

d_OP_ensureDBHandle_dot_addNewDBHandle_dot_200 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_ensureDBHandle_dot_addNewDBHandle_dot_200 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_dollar Curry_IOExts.d_C_connectToCommand (Curry_Prelude.d_OP_plus_plus (d_C_path'to'sqlite3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) x3250 x3500) (d_OP_ensureDBHandle_dot_addNewDBHandle_dot_200_dot___hash_lambda34 x1 x2) x3250 x3500

d_OP_ensureDBHandle_dot_addNewDBHandle_dot_200_dot___hash_lambda34 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle) -> Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_ensureDBHandle_dot_addNewDBHandle_dot_200_dot___hash_lambda34 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_hPutAndFlush x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List)))))))))))))) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Global.d_C_writeGlobal (d_C_openDBHandles x3250 x3500)) (Curry_List.d_C_insertBy (acceptCs id (d_C_on (acceptCs id Curry_Prelude.d_OP_lt_eq) Curry_Prelude.d_C_fst)) (Curry_Prelude.OP_Tuple2 x1 x3) x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Global.d_C_readGlobal (d_C_currentlyInTransaction x3250 x3500) x3250 x3500) (d_OP_ensureDBHandle_dot_addNewDBHandle_dot_200_dot___hash_lambda34_dot___hash_lambda35 x3) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_ensureDBHandle_dot_addNewDBHandle_dot_200_dot___hash_lambda34_dot___hash_lambda35 :: Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_ensureDBHandle_dot_addNewDBHandle_dot_200_dot___hash_lambda34_dot___hash_lambda35 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar (d_C_unless (Curry_Prelude.d_C_not x2 x3250 x3500)) (Curry_IO.d_C_hPutStrLn x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List)))))))))))))))) x3250 x3500) x3250 x3500

d_OP_ensureDBHandle_dot___hash_lambda36 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_ensureDBHandle_dot___hash_lambda36 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar (d_C_unless (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x1 x3250 x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst x2 x3250 x3500) x3250 x3500)) (d_OP_ensureDBHandle_dot_addNewDBHandle_dot_200 x1 x2 x3250 x3500) x3250 x3500

d_C_unless :: Curry_Prelude.C_Bool -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_unless x1 x2 x3250 x3500 = case x1 of
     Curry_Prelude.C_False -> x2
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_unless x1002 x2 x3250 x3500) (d_C_unless x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_unless z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_unless x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_on :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t1) => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> t1) -> (t2 -> Cover -> ConstStore -> t0) -> t2 -> t2 -> Cover -> ConstStore -> t1
d_C_on x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 (Curry_Prelude.d_C_apply x2 x3 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply x2 x4 x3250 x3500) x3250 x3500

nd_C_on :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t1) => Func t0 (Func t0 t1) -> Func t2 t0 -> t2 -> t2 -> IDSupply -> Cover -> ConstStore -> t1
nd_C_on x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2004 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2004 (seq x2006 (let
               x2002 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2002 (seq x2003 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply x1 (Curry_Prelude.nd_C_apply x2 x3 x2000 x3250 x3500) x2001 x3250 x3500)))) (Curry_Prelude.nd_C_apply x2 x4 x2003 x3250 x3500) x2004 x3250 x3500))))))))

d_C_ensureDBTable :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_ensureDBTable x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Global.d_C_readGlobal (d_C_knownDBTables x3250 x3500) x3250 x3500) (d_OP_ensureDBTable_dot___hash_lambda37 x3 x1 x2) x3250 x3500

d_OP_ensureDBTable_dot___hash_lambda37 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_ensureDBTable_dot___hash_lambda37 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_dollar (d_C_unless (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem (Curry_Prelude.OP_Tuple2 x2 x3) x3250 x3500) x4 x3250 x3500)) (Curry_Prelude.d_OP_gt_gt_eq (d_C_readDBHandle x2 x3250 x3500) (d_OP_ensureDBTable_dot___hash_lambda37_dot___hash_lambda38 x1 x2 x4 x3) x3250 x3500) x3250 x3500

d_OP_ensureDBTable_dot___hash_lambda37_dot___hash_lambda38 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_ensureDBTable_dot___hash_lambda37_dot___hash_lambda38 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (d_C_hPutAndFlush x5) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (d_C_commaSep x3250 x3500) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_dollar (Curry_Global.d_C_writeGlobal (d_C_knownDBTables x3250 x3500)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x2 x4) x3) x3250 x3500) x3250 x3500

d_C_knownDBTables :: Cover -> ConstStore -> Curry_Global.C_Global (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_knownDBTables x0 x1 = global_C_knownDBTables

global_C_knownDBTables :: Curry_Global.C_Global (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
global_C_knownDBTables = Curry_Global.d_C_global (let
     x3500 = emptyCs
     x3250 = initCover
      in Curry_Prelude.OP_List) Curry_Global.C_Temporary initCover emptyCs

d_C_beginTransaction :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_beginTransaction x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Global.d_C_writeGlobal (d_C_currentlyInTransaction x3250 x3500) Curry_Prelude.C_True x3250 x3500) (d_C_withAllDBHandles (Curry_Prelude.d_C_flip (acceptCs id d_C_hPutAndFlush) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List))))))))))))))))) x3250 x3500) x3250 x3500

d_C_commitTransaction :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_commitTransaction x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_withAllDBHandles (Curry_Prelude.d_C_flip (acceptCs id d_C_hPutAndFlush) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List)))))))) x3250 x3500) (Curry_Global.d_C_writeGlobal (d_C_currentlyInTransaction x3250 x3500) Curry_Prelude.C_False x3250 x3500) x3250 x3500

d_C_rollbackTransaction :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_rollbackTransaction x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_withAllDBHandles (Curry_Prelude.d_C_flip (acceptCs id d_C_hPutAndFlush) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List)))))))))) x3250 x3500) (Curry_Global.d_C_writeGlobal (d_C_currentlyInTransaction x3250 x3500) Curry_Prelude.C_False x3250 x3500) x3250 x3500

d_C_currentlyInTransaction :: Cover -> ConstStore -> Curry_Global.C_Global Curry_Prelude.C_Bool
d_C_currentlyInTransaction x0 x1 = global_C_currentlyInTransaction

global_C_currentlyInTransaction :: Curry_Global.C_Global Curry_Prelude.C_Bool
global_C_currentlyInTransaction = Curry_Global.d_C_global (let
     x3500 = emptyCs
     x3250 = initCover
      in Curry_Prelude.C_False) Curry_Global.C_Temporary initCover emptyCs

d_C_showTupleArgs :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_showTupleArgs x3250 x3500 = Curry_Prelude.d_OP_dot (d_C_splitTLC x3250 x3500) (Curry_Prelude.d_OP_dot d_C_removeOuterParens Curry_ReadShowTerm.d_C_showQTerm x3250 x3500) x3250 x3500

nd_C_showTupleArgs :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_showTupleArgs x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_dot (nd_C_splitTLC x2000 x3250 x3500) (Curry_Prelude.nd_OP_dot (wrapDX id d_C_removeOuterParens) (wrapDX id Curry_ReadShowTerm.d_C_showQTerm) x2001 x3250 x3500) x2002 x3250 x3500))))))))

d_C_removeOuterParens :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_removeOuterParens x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_47 x3 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_removeOuterParens x1002 x3250 x3500) (d_C_removeOuterParens x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_removeOuterParens z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_removeOuterParens x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_init :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_init x3250 x3500 = Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_reverse x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_tail (Curry_Prelude.d_C_reverse x3250 x3500) x3250 x3500) x3250 x3500

nd_C_init :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_C_init x3000 x3250 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2004 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2004 (seq x2006 (let
               x2000 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2000 (seq x2003 (Curry_Prelude.nd_OP_dot (Curry_Prelude.nd_C_reverse x2000 x3250 x3500) (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_tail) (Curry_Prelude.nd_C_reverse x2001 x3250 x3500) x2002 x3250 x3500)))) x2004 x3250 x3500))))))))

d_C_splitTLC :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_splitTLC x3250 x3500 = d_C_parse Curry_Prelude.OP_List

nd_C_splitTLC :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_splitTLC x3000 x3250 x3500 = wrapDX id (d_C_parse Curry_Prelude.OP_List)

d_C_parse :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_parse x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_46 x4 x3 x1 (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.OP_Cons x3 x4)) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_parse x1 x1002 x3250 x3500) (d_C_parse x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_parse x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_parse x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_next :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_next x1 x2 x3 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_OP__case_39 x1 x3 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char ','#) x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP_lt_colon x1 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_next x1 x1002 x3 x3250 x3500) (d_C_next x1 x1003 x3 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_next x1 z x3 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_next x1 x1002 x3 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_lt_colon :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_lt_colon x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x1 x3) x4
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lt_colon x1 x1002 x3250 x3500) (d_OP_lt_colon x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lt_colon x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lt_colon x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_updStack :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_updStack x1 x2 x3250 x3500 = d_OP__case_38 x2 x1 (Curry_Prelude.OP_Tuple2 x1 x2) x3250 x3500

d_C_showTError :: C_TError -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTError x1 x3250 x3500 = case x1 of
     (C_TError x2 x3) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3 x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_TError x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showTError x1002 x3250 x3500) (d_C_showTError x1003 x3250 x3500)
     (Choices_C_TError x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showTError z x3250 x3500) x1002
     (Guard_C_TError x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showTError x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_TError x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_38 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Char (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_38 x2 x1 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_37 x3 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x2 x1 x1002 x3250 x3500) (d_OP__case_38 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_37 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_37 x3 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = x5
           in (d_OP__case_36 x7 x3 x4 x6 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Char '\\'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> let
          x13 = x3
           in (d_OP__case_4 x13 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char '\\'#) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x3 x1002 x3250 x3500) (d_OP__case_37 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_4 x13 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_3 x13 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x13 x1002 x3250 x3500) (d_OP__case_4 x13 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x13 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_3 x13 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_2 x13 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x13 x1002 x3250 x3500) (d_OP__case_3 x13 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x13 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_2 x13 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_1 x13 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char '('#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x13 x1002 x3250 x3500) (d_OP__case_2 x13 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x13 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_1 x13 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_0 x13 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char '['#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x13 x1002 x3250 x3500) (d_OP__case_1 x13 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x13 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x13 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x13 x1002 x3250 x3500) (d_OP__case_0 x13 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x13 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_36 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_36 x7 x3 x4 x6 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> d_OP__case_35 x7 x3 x4 x6 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x7 x3 x4 x6 x1002 x3250 x3500) (d_OP__case_36 x7 x3 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x7 x3 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x7 x3 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_35 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_35 x7 x3 x4 x6 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x8 = x3
           in (d_OP__case_34 x8 x6 x4 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char '\\'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_29 x7 x3 x4 x6 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x7 x3 x4 x6 x1002 x3250 x3500) (d_OP__case_35 x7 x3 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x7 x3 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x7 x3 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_29 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_29 x7 x3 x4 x6 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x9 = x3
           in (d_OP__case_28 x9 x6 x4 (Curry_Prelude.d_OP_eq_eq x9 (Curry_Prelude.C_Char '\\'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_23 x7 x3 x4 x6 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Char '('#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x7 x3 x4 x6 x1002 x3250 x3500) (d_OP__case_29 x7 x3 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x7 x3 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x7 x3 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_23 x7 x3 x4 x6 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x10 = x3
           in (d_OP__case_22 x10 x4 x6 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char '\\'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_16 x7 x3 x4 x6 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Char '['#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x7 x3 x4 x6 x1002 x3250 x3500) (d_OP__case_23 x7 x3 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x7 x3 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x7 x3 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_16 x7 x3 x4 x6 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> let
          x11 = x3
           in (d_OP__case_15 x11 x4 x6 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char '\\'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> let
          x12 = x3
           in (d_OP__case_9 x12 x4 (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.C_Char '\\'#) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x7 x3 x4 x6 x1002 x3250 x3500) (d_OP__case_16 x7 x3 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x7 x3 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x7 x3 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_9 x12 x4 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) x4
     Curry_Prelude.C_False -> d_OP__case_8 x12 x4 (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x12 x4 x1002 x3250 x3500) (d_OP__case_9 x12 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x12 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x12 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_8 x12 x4 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) x4
     Curry_Prelude.C_False -> d_OP__case_7 x12 x4 (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x12 x4 x1002 x3250 x3500) (d_OP__case_8 x12 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x12 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x12 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_7 x12 x4 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) x4
     Curry_Prelude.C_False -> d_OP__case_6 x12 x4 (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.C_Char '('#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x12 x4 x1002 x3250 x3500) (d_OP__case_7 x12 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x12 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x12 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_6 x12 x4 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) x4
     Curry_Prelude.C_False -> d_OP__case_5 x12 x4 (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.C_Char '['#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x12 x4 x1002 x3250 x3500) (d_OP__case_6 x12 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x12 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x12 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_5 x12 x4 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) x4
     Curry_Prelude.C_False -> x4
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x12 x4 x1002 x3250 x3500) (d_OP__case_5 x12 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x12 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x12 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_15 x11 x4 x6 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) x4
     Curry_Prelude.C_False -> d_OP__case_14 x11 x4 x6 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x11 x4 x6 x1002 x3250 x3500) (d_OP__case_15 x11 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x11 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x11 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_14 x11 x4 x6 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) x4
     Curry_Prelude.C_False -> d_OP__case_13 x11 x4 x6 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x11 x4 x6 x1002 x3250 x3500) (d_OP__case_14 x11 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x11 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x11 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_13 x11 x4 x6 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) x4
     Curry_Prelude.C_False -> d_OP__case_12 x11 x4 x6 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char '('#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x11 x4 x6 x1002 x3250 x3500) (d_OP__case_13 x11 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x11 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x11 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_12 x11 x4 x6 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) x4
     Curry_Prelude.C_False -> d_OP__case_11 x11 x4 x6 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char '['#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x11 x4 x6 x1002 x3250 x3500) (d_OP__case_12 x11 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x11 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x11 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_11 x11 x4 x6 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) x4
     Curry_Prelude.C_False -> d_OP__case_10 x11 x4 x6 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char ']'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x11 x4 x6 x1002 x3250 x3500) (d_OP__case_11 x11 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x11 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x11 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_10 x11 x4 x6 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> x4
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x11 x4 x6 x1002 x3250 x3500) (d_OP__case_10 x11 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x11 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x11 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_22 x10 x4 x6 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) x4
     Curry_Prelude.C_False -> d_OP__case_21 x10 x4 x6 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x10 x4 x6 x1002 x3250 x3500) (d_OP__case_22 x10 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x10 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x10 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_21 x10 x4 x6 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) x4
     Curry_Prelude.C_False -> d_OP__case_20 x10 x4 x6 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x10 x4 x6 x1002 x3250 x3500) (d_OP__case_21 x10 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x10 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x10 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_20 x10 x4 x6 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) x4
     Curry_Prelude.C_False -> d_OP__case_19 x10 x4 x6 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char '('#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x10 x4 x6 x1002 x3250 x3500) (d_OP__case_20 x10 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x10 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x10 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_19 x10 x4 x6 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) x4
     Curry_Prelude.C_False -> d_OP__case_18 x10 x4 x6 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char ')'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x10 x4 x6 x1002 x3250 x3500) (d_OP__case_19 x10 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x10 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x10 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_18 x10 x4 x6 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> d_OP__case_17 x10 x4 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char '['#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x10 x4 x6 x1002 x3250 x3500) (d_OP__case_18 x10 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x10 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x10 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_17 x10 x4 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) x4
     Curry_Prelude.C_False -> x4
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x10 x4 x1002 x3250 x3500) (d_OP__case_17 x10 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x10 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x10 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_28 x9 x6 x4 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) x4
     Curry_Prelude.C_False -> d_OP__case_27 x9 x6 (Curry_Prelude.d_OP_eq_eq x9 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x9 x6 x4 x1002 x3250 x3500) (d_OP__case_28 x9 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x9 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x9 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_27 x9 x6 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) x6
     Curry_Prelude.C_False -> d_OP__case_26 x9 x6 (Curry_Prelude.d_OP_eq_eq x9 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x9 x6 x1002 x3250 x3500) (d_OP__case_27 x9 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x9 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x9 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_26 x9 x6 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> d_OP__case_25 x9 x6 (Curry_Prelude.d_OP_eq_eq x9 (Curry_Prelude.C_Char '('#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x9 x6 x1002 x3250 x3500) (d_OP__case_26 x9 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x9 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x9 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_25 x9 x6 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) x6
     Curry_Prelude.C_False -> d_OP__case_24 x9 x6 (Curry_Prelude.d_OP_eq_eq x9 (Curry_Prelude.C_Char '['#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x9 x6 x1002 x3250 x3500) (d_OP__case_25 x9 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x9 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x9 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_24 x9 x6 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) x6
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) x6
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x9 x6 x1002 x3250 x3500) (d_OP__case_24 x9 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x9 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x9 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_34 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_34 x8 x6 x4 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) x4
     Curry_Prelude.C_False -> d_OP__case_33 x8 x6 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x8 x6 x4 x1002 x3250 x3500) (d_OP__case_34 x8 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x8 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x8 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_33 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_33 x8 x6 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> d_OP__case_32 x8 x6 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x8 x6 x1002 x3250 x3500) (d_OP__case_33 x8 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x8 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x8 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_32 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_32 x8 x6 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) x6
     Curry_Prelude.C_False -> d_OP__case_31 x8 x6 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char '('#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x8 x6 x1002 x3250 x3500) (d_OP__case_32 x8 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 x8 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x8 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_31 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_31 x8 x6 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) x6
     Curry_Prelude.C_False -> d_OP__case_30 x8 x6 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char '['#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x8 x6 x1002 x3250 x3500) (d_OP__case_31 x8 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x8 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x8 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_30 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_30 x8 x6 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) x6
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) x6
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x8 x6 x1002 x3250 x3500) (d_OP__case_30 x8 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x8 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x8 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_39 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_39 x1 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons Curry_Prelude.OP_List x3
     Curry_Prelude.C_False -> d_OP_lt_colon x1 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x1 x3 x1002 x3250 x3500) (d_OP__case_39 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_46 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_46 x4 x3 x1 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_45 x4 x1 x3 x6 x5 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_46 x4 x3 x1 x1002 x3250 x3500) (d_OP__case_46 x4 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_46 x4 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_46 x4 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_45 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_45 x4 x1 x3 x6 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x7 x8) -> let
          x9 = x7
           in (d_OP__case_44 x9 x4 x1 x3 x6 x8 (Curry_Prelude.d_OP_eq_eq x9 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_dollar (d_C_next x3 x1) (d_C_parse (d_C_updStack x3 x1 x3250 x3500) x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x4 x1 x3 x6 x1002 x3250 x3500) (d_OP__case_45 x4 x1 x3 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 x4 x1 x3 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x4 x1 x3 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_44 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_44 x9 x4 x1 x3 x6 x8 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> d_OP__case_43 x4 x1 x3 x8 x6 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_dollar (d_C_next x3 x1) (d_C_parse (d_C_updStack x3 x1 x3250 x3500) x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x9 x4 x1 x3 x6 x8 x1002 x3250 x3500) (d_OP__case_44 x9 x4 x1 x3 x6 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 x9 x4 x1 x3 x6 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x9 x4 x1 x3 x6 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_43 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_43 x4 x1 x3 x8 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x12 = x10
           in (d_OP__case_42 x12 x4 x1 x3 x11 x8 (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_dollar (d_C_next x3 x1) (d_C_parse (d_C_updStack x3 x1 x3250 x3500) x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x4 x1 x3 x8 x1002 x3250 x3500) (d_OP__case_43 x4 x1 x3 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x4 x1 x3 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x4 x1 x3 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_42 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_42 x12 x4 x1 x3 x11 x8 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> d_OP__case_41 x4 x1 x3 x8 x11 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_dollar (d_C_next x3 x1) (d_C_parse (d_C_updStack x3 x1 x3250 x3500) x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x12 x4 x1 x3 x11 x8 x1002 x3250 x3500) (d_OP__case_42 x12 x4 x1 x3 x11 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x12 x4 x1 x3 x11 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x12 x4 x1 x3 x11 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_41 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_41 x4 x1 x3 x8 x11 x3250 x3500 = case x11 of
     (Curry_Prelude.OP_Cons x13 x14) -> let
          x15 = x13
           in (d_OP__case_40 x15 x4 x1 x3 x14 x8 (Curry_Prelude.d_OP_eq_eq x15 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_dollar (d_C_next x3 x1) (d_C_parse (d_C_updStack x3 x1 x3250 x3500) x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x4 x1 x3 x8 x1002 x3250 x3500) (d_OP__case_41 x4 x1 x3 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x4 x1 x3 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x4 x1 x3 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_40 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_40 x15 x4 x1 x3 x14 x8 x16 x3250 x3500 = case x16 of
     Curry_Prelude.C_True -> d_OP_lt_colon (Curry_Prelude.C_Char '\''#) (d_OP_lt_colon (Curry_Prelude.C_Char '\''#) (d_C_parse x8 x14 x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_dollar (d_C_next x3 x1) (d_C_parse (d_C_updStack x3 x1 x3250 x3500) x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x15 x4 x1 x3 x14 x8 x1002 x3250 x3500) (d_OP__case_40 x15 x4 x1 x3 x14 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x15 x4 x1 x3 x14 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x15 x4 x1 x3 x14 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_47 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_47 x3 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.C_Char '('#) -> Curry_Prelude.d_C_apply (d_C_init x3250 x3500) x3 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('(',Curry_Prelude.d_C_apply (d_C_init x3250 x3500) x3 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_47 x3 x1002 x3250 x3500) (d_OP__case_47 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_47 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_47 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_48 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_48 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x5
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_48 x1002 x3250 x3500) (d_OP__case_48 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_48 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_48 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_49 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_49 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x4
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_49 x1002 x3250 x3500) (d_OP__case_49 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_49 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_49 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_50 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_50 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_50 x2 x1002 x3250 x3500) (d_OP__case_50 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_50 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_50 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_51 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List C_ColVal -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_51 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (d_C_showColVals x1 x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_51 x2 x1 x1002 x3250 x3500) (d_OP__case_51 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_51 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_51 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_51 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List C_ColVal -> Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_51 x2 x1 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (nd_C_showColVals x1 x2 x2000 x3250 x3500) x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_51 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_51 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_51 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_51 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_52 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_52 x2 x3 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.OP_List x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_hGetLinesBefore x1 x2 x3250 x3500) (d_OP_hGetLinesBefore_dot___hash_lambda31_dot___hash_lambda32 x3) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_52 x2 x3 x1 x1002 x3250 x3500) (d_OP__case_52 x2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_52 x2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_52 x2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_53 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_53 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_53 x1 x1002 x3250 x3500) (d_OP__case_53 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_53 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_53 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_55 :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> C_Dynamic) -> t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_55 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_dollar d_C_quote (Curry_ReadShowTerm.d_C_showQTerm x2 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_54 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_55 x1 x2 x1002 x3250 x3500) (d_OP__case_55 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_55 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_55 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_55 :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int (Func t0 C_Dynamic) -> t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP__case_55 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_Prelude.nd_OP_dollar (wrapDX id d_C_quote) (Curry_ReadShowTerm.d_C_showQTerm x2 x3250 x3500) x2000 x3250 x3500) Curry_Prelude.OP_List))
     Curry_Prelude.C_False -> d_OP__case_54 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_55 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_55 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_55 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_55 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_54 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_54 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_map d_C_quote) (Curry_Prelude.d_C_apply (d_C_showTupleArgs x3250 x3500) x2 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_54 x2 x1002 x3250 x3500) (d_OP__case_54 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_54 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_54 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_56 :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Transaction Curry_Prelude.OP_Unit
d_OP__case_56 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar (d_C_errorT x3250 x3500) (C_TError C_KeyNotExistsError x1) x3250 x3500
     Curry_Prelude.C_False -> d_C_doneT x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_56 x2 x1 x1002 x3250 x3500) (d_OP__case_56 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_56 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_56 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_57 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_57 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_57 x1002 x3250 x3500) (d_OP__case_57 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_57 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_57 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_58 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_58 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_58 x2 x1002 x3250 x3500) (d_OP__case_58 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_58 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_58 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_61 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Dynamic
d_OP__case_61 x3 x2 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> C_DBInfo x1 x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> d_OP__case_60 x3 x2 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List))))))) x3250 x3500) x3 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_61 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_61 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_61 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_61 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_60 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Dynamic
d_OP__case_60 x3 x2 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_59 x3 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_60 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_60 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_60 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_60 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_59 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Dynamic
d_OP__case_59 x3 x2 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> C_DBInfo x1 x2 x3
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_59 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_59 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_59 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_59 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
