{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_BrowserGUI (C_Tree (..), C_ImportTree, C_GuiState, d_C_showExecTime, d_C_title, d_C_version, d_C_patchReadmeVersion, nd_C_main, nd_C_start, nd_C_m1, nd_C_m2, nd_C_m3, nd_C_m4, d_C_trees2strings, d_C_tree2strings, d_C_blanks, d_C_getTreesNodeName, d_C_getTreesValue, d_C_changeTrees, d_C_openNode, d_C_getModuleFileName, d_C_getTrees, d_C_storeTrees, d_C_getAllImportsOfModule, d_C_getFuns, d_C_storeSelectedFunctions, d_C_setMainContentsModule, nd_C_setMainContentsModule, d_C_getContentsModule, nd_C_getContentsModule, d_C_getMainContents, nd_C_getMainContents, d_C_getFunctionListKind, nd_C_getFunctionListKind, d_C_setFunctionListKind, nd_C_setFunctionListKind, d_C_getAllModules, d_C_getIntWithName, d_C_getProgWithName, nd_C_getProgWithName, d_C_getAllTypes, d_C_getAllFunctions, nd_C_getAllFunctions, d_C_getAllFunctionNames, nd_C_getAllFunctionNames, d_C_getCurrentFunctionAnalysis, nd_C_getCurrentFunctionAnalysis, d_C_setCurrentFunctionAnalysis, nd_C_setCurrentFunctionAnalysis, d_C_readProgAndStore, nd_C_readProgAndStore, d_C_readProgAndStoreIfNecessary, nd_C_readProgAndStoreIfNecessary, d_C_findDecl4name, nd_C_browserGUI, d_C_isPublic, d_C_findFunDeclInProgText, d_C_findFirstDeclLine, d_C_browserDir, d_C_leqQName, d_C_showQNameWithMod, d_C_noAnalysisText, nd_C_getAnswer) where

import Basics
import qualified Curry_AnalysisTypes
import qualified Curry_BrowserAnalysis
import qualified Curry_Dependency
import qualified Curry_Directory
import qualified Curry_Distribution
import qualified Curry_FileGoodies
import qualified Curry_FlatCurry
import qualified Curry_FlatCurryGoodies
import qualified Curry_FlatCurryShow
import qualified Curry_GUI
import qualified Curry_IOExts
import qualified Curry_ImportCalls
import qualified Curry_Imports
import qualified Curry_List
import qualified Curry_Maybe
import qualified Curry_Prelude
import qualified Curry_Read
import qualified Curry_ShowFlatCurry
import qualified Curry_ShowGraph
import qualified Curry_Sort
import qualified Curry_System
import qualified Curry_Time
type C_ImportTree = Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))

type C_GuiState = Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))

data C_Tree t0
     = C_Leaf (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0
     | C_Node (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 (Curry_Prelude.OP_List (C_Tree t0))
     | Choice_C_Tree Cover ID (C_Tree t0) (C_Tree t0)
     | Choices_C_Tree Cover ID ([C_Tree t0])
     | Fail_C_Tree Cover FailInfo
     | Guard_C_Tree Cover Constraints (C_Tree t0)

instance Show t0 => Show (C_Tree t0) where
  showsPrec d (Choice_C_Tree cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Tree cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Tree cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Tree cd info) = showChar '!'
  showsPrec _ (C_Leaf x1 x2) = (showString "(Leaf") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_Node x1 x2 x3) = (showString "(Node") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read t0 => Read (C_Tree t0) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Leaf x1 x2,r2) | (_,r0) <- readQualified "BrowserGUI" "Leaf" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_Node x1 x2 x3,r3) | (_,r0) <- readQualified "BrowserGUI" "Node" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s)


instance NonDet (C_Tree t0) where
  choiceCons = Choice_C_Tree
  choicesCons = Choices_C_Tree
  failCons = Fail_C_Tree
  guardCons = Guard_C_Tree
  try (Choice_C_Tree cd i x y) = tryChoice cd i x y
  try (Choices_C_Tree cd i xs) = tryChoices cd i xs
  try (Fail_C_Tree cd info) = Fail cd info
  try (Guard_C_Tree cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Tree cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Tree cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Tree cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Tree cd i _) = error ("BrowserGUI.Tree.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Tree cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Tree cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_Tree t0) where
  generate s = Choices_C_Tree defCover (freeID [2,3] s) [(C_Leaf (generate (leftSupply s)) (generate (rightSupply s))),(C_Node (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s)))]


instance NormalForm t0 => NormalForm (C_Tree t0) where
  ($!!) cont (C_Leaf x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_Leaf y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_Node x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_Node y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_Tree cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Tree cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Tree cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Tree cd info) _ = failCons cd info
  ($##) cont (C_Leaf x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_Leaf y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_Node x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_Node y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_Tree cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Tree cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Tree cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Tree cd info) _ = failCons cd info
  searchNF search cont (C_Leaf x1 x2) = search (\y1 -> search (\y2 -> cont (C_Leaf y1 y2)) x2) x1
  searchNF search cont (C_Node x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_Node y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("BrowserGUI.Tree.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_Tree t0) where
  (=.=) (C_Leaf x1 x2) (C_Leaf y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_Node x1 x2 x3) (C_Node y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Leaf x1 x2) (C_Leaf y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_Node x1 x2 x3) (C_Node y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Leaf x2 x3) = ((i :=: (ChooseN 0 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_Node x2 x3 x4) = ((i :=: (ChooseN 1 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (Choice_C_Tree cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Tree cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Tree cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Tree cd i _) = error ("BrowserGUI.Tree.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Tree cd info) = [(Unsolvable info)]
  bind i (Guard_C_Tree cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Leaf x2 x3) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_Node x2 x3 x4) = [(i :=: (ChooseN 1 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (Choice_C_Tree cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Tree cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Tree cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Tree cd i _) = error ("BrowserGUI.Tree.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Tree cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Tree cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_Tree t0) where
  (=?=) (Choice_C_Tree cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Tree cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Tree cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Tree cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Tree cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Tree cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Tree cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Tree cd info) _ = failCons cd info
  (=?=) (C_Leaf x1 x2) (C_Leaf y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_Node x1 x2 x3) (C_Node y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Tree cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Tree cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Tree cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Tree cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Tree cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Tree cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Tree cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Tree cd info) _ = failCons cd info
  (<?=) (C_Leaf x1 x2) (C_Leaf y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_Leaf _ _) (C_Node _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_Node x1 x2 x3) (C_Node y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable t0 => Coverable (C_Tree t0) where
  cover (C_Leaf x1 x2) = C_Leaf (cover x1) (cover x2)
  cover (C_Node x1 x2 x3) = C_Node (cover x1) (cover x2) (cover x3)
  cover (Choice_C_Tree cd i x y) = Choice_C_Tree (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Tree cd i xs) = Choices_C_Tree (incCover cd) i (map cover xs)
  cover (Fail_C_Tree cd info) = Fail_C_Tree (incCover cd) info
  cover (Guard_C_Tree cd c e) = Guard_C_Tree (incCover cd) c (cover e)


d_C_showExecTime :: ConstStore -> Curry_Prelude.C_Bool
d_C_showExecTime x3500 = Curry_Prelude.C_False

d_C_title :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_title x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))

d_C_version :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_version x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'V'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '3'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) Curry_Prelude.OP_List))))))))))))))))))))

d_C_patchReadmeVersion :: ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_patchReadmeVersion x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readCompleteFile (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) Curry_Prelude.OP_List)))))) x3500) d_OP_patchReadmeVersion_dot___hash_lambda1 x3500

d_OP_patchReadmeVersion_dot_replaceVersion_dot_8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_patchReadmeVersion_dot_replaceVersion_dot_8 x1 x3500 = d_OP__case_47 x1 (Curry_List.d_C_isPrefixOf (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'V'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))) x1 x3500) x3500

d_OP_patchReadmeVersion_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_patchReadmeVersion_dot___hash_lambda1 x1 x3500 = Curry_Prelude.d_C_writeFile (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_unlines (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_map d_OP_patchReadmeVersion_dot_replaceVersion_dot_8) Curry_Prelude.d_C_lines x3500) x3500) x1 x3500) x3500

nd_C_main :: IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_main x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_System.d_C_getArgs x3500) (wrapNX id nd_OP_main_dot___hash_lambda2) x2000 x3500))

nd_OP_main_dot___hash_lambda2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_main_dot___hash_lambda2 x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_46 x1 x2 x3 x2000 x3500))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))) x3500) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_main_dot___hash_lambda2 x1002 x3000 x3500) (nd_OP_main_dot___hash_lambda2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_main_dot___hash_lambda2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_main_dot___hash_lambda2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_start :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_start x1 x3000 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2000 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2000 (seq x2004 (let
               x2001 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2001 (seq x2002 (let
                    x2 = generate x2001
                    x3 = generate x2002
                     in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))) x3500) (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_C_readFile (Curry_Prelude.d_OP_plus_plus (d_C_browserDir x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) Curry_Prelude.OP_List))))))) x3500) x3500) (wrapNX id (nd_OP_start_dot___hash_lambda4 x1 x2 x3)) x2000 x3500) x3500)))))))))

nd_OP_start_dot___hash_lambda4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_start_dot___hash_lambda4 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Imports.d_C_getImportedInterfaces x1 x3500) (wrapNX id (nd_OP_start_dot___hash_lambda4_dot___hash_lambda5 x4 x1 x2 x3)) x2000 x3500))

nd_OP_start_dot___hash_lambda4_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_start_dot___hash_lambda4_dot___hash_lambda5 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2013 = x3000
      in (seq x2013 (let
          x2002 = leftSupply x2013
          x2014 = rightSupply x2013
           in (seq x2002 (seq x2014 (let
               x2009 = leftSupply x2014
               x2012 = rightSupply x2014
                in (seq x2009 (seq x2012 (let
                    x6 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_progName x2000 x3500) (Curry_Imports.d_C_progOfIFFP (Curry_Prelude.d_C_snd (Curry_Prelude.d_C_head x5 x3500) x3500) x3500) x2001 x3500)))
                    x7 = Curry_Prelude.OP_Cons (let
                         x2003 = leftSupply x2009
                         x2008 = rightSupply x2009
                          in (seq x2003 (seq x2008 (C_Leaf (nd_OP__case_45 x2 x6 (Curry_Prelude.d_OP_eq_eq x6 x2 x3500) x2003 x3500) (Curry_Prelude.OP_Tuple2 x6 (let
                              x2007 = leftSupply x2008
                              x2006 = rightSupply x2008
                               in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_map (let
                                   x2005 = leftSupply x2006
                                   x2004 = rightSupply x2006
                                    in (seq x2005 (seq x2004 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Imports.d_C_moduleImports) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Imports.d_C_progOfIFFP) (wrapDX id Curry_Prelude.d_C_snd) x2004 x3500) x2005 x3500)))) x5 x2007 x3500))))))))) Curry_Prelude.OP_List
                     in (let
                         x2011 = leftSupply x2012
                         x2010 = rightSupply x2012
                          in (seq x2011 (seq x2010 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IOExts.d_C_newIORef (Curry_Prelude.OP_Tuple6 x7 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 (Curry_Prelude.OP_Tuple2 x2 (Curry_Prelude.d_C_snd (Curry_Prelude.d_C_head x5 x3500) x3500))) (Curry_Prelude.nd_C_map (wrapDX id d_OP_start_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6) (Curry_Prelude.d_C_tail x5 x3500) x2010 x3500)) Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 Curry_Prelude.OP_List Curry_AnalysisTypes.C_OtherText Curry_Prelude.OP_List) Curry_Prelude.C_False Curry_Prelude.C_Nothing) x3500) (wrapNX id (nd_OP_start_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda7 x1 x3 x4 x7)) x2011 x3500))))))))))))

d_OP_start_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg)
d_OP_start_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.OP_Tuple2 x2 (Curry_Prelude.OP_Tuple2 x2 x3)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_start_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1002 x3500) (d_OP_start_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_start_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_start_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_start_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda7 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_start_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda7 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_GUI.nd_C_runInitGUI (Curry_Prelude.d_OP_plus_plus (d_C_title x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (d_C_version x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500) x3500) (nd_C_browserGUI x5 x2 x3 (d_C_trees2strings x4 x3500) x2000 x3500) (wrapDX id (d_OP_start_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda7_dot___hash_lambda8 x1 x2 x3)) x2001 x3500)))))

d_OP_start_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda7_dot___hash_lambda8 :: Curry_Prelude.Curry t1661 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1661)
d_OP_start_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda7_dot___hash_lambda8 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x3 x1 x4 x3500) (Curry_GUI.d_C_setValue x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) Curry_Prelude.OP_List) x4 x3500) x3500) (Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500) x3500

nd_C_m1 :: IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_m1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_start (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) Curry_Prelude.OP_List)))))))))) x2000 x3500))

nd_C_m2 :: IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_m2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_start (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))) x2000 x3500))

nd_C_m3 :: IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_m3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_start (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List)))) x2000 x3500))

nd_C_m4 :: IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_m4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_start (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) Curry_Prelude.OP_List))) x2000 x3500))

d_C_trees2strings :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_Tree t0) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_trees2strings x1 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_tree2strings (Curry_Prelude.C_Int 0#)) x3500) x1 x3500

d_C_tree2strings :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> C_Tree t0 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_tree2strings x1 x2 x3500 = case x2 of
     (C_Leaf x3 x4) -> Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3 x3500) x3500) Curry_Prelude.OP_List
     (C_Node x5 x6 x7) -> Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x5 x3500) x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_tree2strings (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3500)) x3500) x7 x3500)
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_tree2strings x1 x1002 x3500) (d_C_tree2strings x1 x1003 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_tree2strings x1 z x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_tree2strings x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_blanks :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_blanks x1 x3500 = Curry_Prelude.d_C_take x1 (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ' '#) x3500) x3500

d_C_getTreesNodeName :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List (C_Tree t0) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_getTreesNodeName x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_44 x1 x4 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getTreesNodeName x1 x1002 x3500) (d_C_getTreesNodeName x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getTreesNodeName x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getTreesNodeName x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getTreesValue :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List (C_Tree t0) -> ConstStore -> t0
d_C_getTreesValue x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'V'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))) x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_40 x1 x4 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getTreesValue x1 x1002 x3500) (d_C_getTreesValue x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getTreesValue x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getTreesValue x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_changeTrees :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))))
d_C_changeTrees x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_36 x1 x4 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_changeTrees x1 x1002 x3500) (d_C_changeTrees x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_changeTrees x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_changeTrees x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_changeTrees_dot___hash_lambda9 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))))
d_OP_changeTrees_dot___hash_lambda9 x1 x2 x3 x4 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons (C_Node x1 x3 x4) x2) x3500

d_OP_changeTrees_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))))
d_OP_changeTrees_dot___hash_lambda10 x1 x2 x3 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons (C_Leaf x1 x2) x3) x3500

d_OP_changeTrees_dot___hash_lambda11 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))))
d_OP_changeTrees_dot___hash_lambda11 x1 x2 x3 x4 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons (C_Node x1 x3 x4) x2) x3500

d_OP_changeTrees_dot___hash_lambda12 :: Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))))
d_OP_changeTrees_dot___hash_lambda12 x1 x2 x3 x4 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons (C_Node x2 x3 x1) x4) x3500

d_C_openNode :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))))
d_C_openNode x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> let
          x4 = Curry_Prelude.d_C_lookup x2 x3 x3500
           in (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_Prelude.d_C_maybe Curry_Prelude.OP_List (Curry_Prelude.d_C_map (d_OP_openNode_dot___hash_lambda13 x3)) x4 x3500) x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_openNode x1002 x3500) (d_C_openNode x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_openNode z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_openNode x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_openNode_dot___hash_lambda13 :: Curry_Prelude.Curry t84 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t84 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t84 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))
d_OP_openNode_dot___hash_lambda13 x1 x2 x3500 = C_Leaf x2 (Curry_Prelude.OP_Tuple2 x2 x1)

d_C_getModuleFileName :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t7,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 t2 t3))) t4 t5 t6 t7) -> t1 -> ConstStore -> Curry_Prelude.C_IO t2
d_C_getModuleFileName x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_getModuleFileName_dot___hash_lambda14 x2) x3500

d_OP_getModuleFileName_dot___hash_lambda14 :: (Curry_Prelude.Curry t159,Curry_Prelude.Curry t158,Curry_Prelude.Curry t169,Curry_Prelude.Curry t161,Curry_Prelude.Curry t162,Curry_Prelude.Curry t163,Curry_Prelude.Curry t164,Curry_Prelude.Curry t168) => t158 -> Curry_Prelude.OP_Tuple6 t159 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t158 (Curry_Prelude.OP_Tuple2 t168 t169))) t161 t162 t163 t164 -> ConstStore -> Curry_Prelude.C_IO t168
d_OP_getModuleFileName_dot___hash_lambda14 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple6 x3 x4 x5 x6 x7 x8) -> Curry_Prelude.d_C_return (Curry_Prelude.d_C_fst (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x1 x4 x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getModuleFileName_dot___hash_lambda14 x1 x1002 x3500) (d_OP_getModuleFileName_dot___hash_lambda14 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getModuleFileName_dot___hash_lambda14 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getModuleFileName_dot___hash_lambda14 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getTrees :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t0) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t0 t1 t2 t3 t4 t5) -> ConstStore -> Curry_Prelude.C_IO t0
d_C_getTrees x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) d_OP_getTrees_dot___hash_lambda15 x3500

d_OP_getTrees_dot___hash_lambda15 :: (Curry_Prelude.Curry t179,Curry_Prelude.Curry t180,Curry_Prelude.Curry t181,Curry_Prelude.Curry t182,Curry_Prelude.Curry t183,Curry_Prelude.Curry t178) => Curry_Prelude.OP_Tuple6 t178 t179 t180 t181 t182 t183 -> ConstStore -> Curry_Prelude.C_IO t178
d_OP_getTrees_dot___hash_lambda15 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple6 x2 x3 x4 x5 x6 x7) -> Curry_Prelude.d_C_return x2 x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getTrees_dot___hash_lambda15 x1002 x3500) (d_OP_getTrees_dot___hash_lambda15 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getTrees_dot___hash_lambda15 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getTrees_dot___hash_lambda15 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_storeTrees :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t0) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t0 t1 (Curry_Prelude.OP_List t2) t3 Curry_Prelude.C_Bool t4) -> t0 -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_storeTrees x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_storeTrees_dot___hash_lambda16 x1 x2) x3500

d_OP_storeTrees_dot___hash_lambda16 :: (Curry_Prelude.Curry t187,Curry_Prelude.Curry t189,Curry_Prelude.Curry t197,Curry_Prelude.Curry t191,Curry_Prelude.Curry t193) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t187 t189 (Curry_Prelude.OP_List t197) t191 Curry_Prelude.C_Bool t193) -> t187 -> Curry_Prelude.OP_Tuple6 t187 t189 (Curry_Prelude.OP_List t197) t191 Curry_Prelude.C_Bool t193 -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_storeTrees_dot___hash_lambda16 x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple6 x4 x5 x6 x7 x8 x9) -> Curry_IOExts.d_C_writeIORef x1 (Curry_Prelude.OP_Tuple6 x2 x5 Curry_Prelude.OP_List x7 Curry_Prelude.C_False x9) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_storeTrees_dot___hash_lambda16 x1 x2 x1002 x3500) (d_OP_storeTrees_dot___hash_lambda16 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_storeTrees_dot___hash_lambda16 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_storeTrees_dot___hash_lambda16 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getAllImportsOfModule :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t1) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_List t1)))))) t2 t3 t4 t5 t6) -> t1 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1)
d_C_getAllImportsOfModule x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_getAllImportsOfModule_dot___hash_lambda17 x2) x3500

d_OP_getAllImportsOfModule_dot_importsOfRoot_dot_126 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 t0 t1)) -> ConstStore -> t1
d_OP_getAllImportsOfModule_dot_importsOfRoot_dot_126 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_32 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getAllImportsOfModule_dot_importsOfRoot_dot_126 x1002 x3500) (d_OP_getAllImportsOfModule_dot_importsOfRoot_dot_126 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getAllImportsOfModule_dot_importsOfRoot_dot_126 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getAllImportsOfModule_dot_importsOfRoot_dot_126 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getAllImportsOfModule_dot_collectImports_dot_126 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List t0)) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_getAllImportsOfModule_dot_collectImports_dot_126 x1 x2 x3 x3500 = case x2 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_29 x1 x3 x4 x5 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x4 x3500) x3 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getAllImportsOfModule_dot_collectImports_dot_126 x1 x1002 x3 x3500) (d_OP_getAllImportsOfModule_dot_collectImports_dot_126 x1 x1003 x3 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getAllImportsOfModule_dot_collectImports_dot_126 x1 z x3 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getAllImportsOfModule_dot_collectImports_dot_126 x1 x1002 x3) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getAllImportsOfModule_dot___hash_lambda17 :: (Curry_Prelude.Curry t453,Curry_Prelude.Curry t444,Curry_Prelude.Curry t445,Curry_Prelude.Curry t446,Curry_Prelude.Curry t447,Curry_Prelude.Curry t448,Curry_Prelude.Curry t456) => t456 -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 t453 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t456 (Curry_Prelude.OP_List t456)))))) t444 t445 t446 t447 t448 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t456)
d_OP_getAllImportsOfModule_dot___hash_lambda17 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple6 x3 x4 x5 x6 x7 x8) -> Curry_Prelude.d_C_return (d_OP_getAllImportsOfModule_dot_collectImports_dot_126 (d_OP_getAllImportsOfModule_dot_importsOfRoot_dot_126 x3 x3500) (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) Curry_Prelude.OP_List x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getAllImportsOfModule_dot___hash_lambda17 x1 x1002 x3500) (d_OP_getAllImportsOfModule_dot___hash_lambda17 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getAllImportsOfModule_dot___hash_lambda17 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getAllImportsOfModule_dot___hash_lambda17 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getFuns :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t2) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t0 t1 t2 t3 t4 t5) -> ConstStore -> Curry_Prelude.C_IO t2
d_C_getFuns x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) d_OP_getFuns_dot___hash_lambda18 x3500

d_OP_getFuns_dot___hash_lambda18 :: (Curry_Prelude.Curry t203,Curry_Prelude.Curry t204,Curry_Prelude.Curry t206,Curry_Prelude.Curry t207,Curry_Prelude.Curry t208,Curry_Prelude.Curry t205) => Curry_Prelude.OP_Tuple6 t203 t204 t205 t206 t207 t208 -> ConstStore -> Curry_Prelude.C_IO t205
d_OP_getFuns_dot___hash_lambda18 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple6 x2 x3 x4 x5 x6 x7) -> Curry_Prelude.d_C_return x4 x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getFuns_dot___hash_lambda18 x1002 x3500) (d_OP_getFuns_dot___hash_lambda18 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getFuns_dot___hash_lambda18 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getFuns_dot___hash_lambda18 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_storeSelectedFunctions :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t0 t1 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) t2 t3 t4) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_storeSelectedFunctions x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_storeSelectedFunctions_dot___hash_lambda19 x2 x1) x3500

d_OP_storeSelectedFunctions_dot___hash_lambda19 :: (Curry_Prelude.Curry t213,Curry_Prelude.Curry t214,Curry_Prelude.Curry t216,Curry_Prelude.Curry t217,Curry_Prelude.Curry t218) => Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t213 t214 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) t216 t217 t218) -> Curry_Prelude.OP_Tuple6 t213 t214 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) t216 t217 t218 -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_storeSelectedFunctions_dot___hash_lambda19 x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple6 x4 x5 x6 x7 x8 x9) -> Curry_IOExts.d_C_writeIORef x2 (Curry_Prelude.OP_Tuple6 x4 x5 (Curry_Sort.d_C_mergeSort (acceptCs id Curry_ShowFlatCurry.d_C_leqFunc) x1 x3500) x7 x8 x9) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_storeSelectedFunctions_dot___hash_lambda19 x1 x2 x1002 x3500) (d_OP_storeSelectedFunctions_dot___hash_lambda19 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_storeSelectedFunctions_dot___hash_lambda19 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_storeSelectedFunctions_dot___hash_lambda19 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_setMainContentsModule :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AnalysisTypes.C_ContentsKind -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_setMainContentsModule x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_setMainContentsModule_dot___hash_lambda20 x3 x2 x4 x1) x3500

nd_C_setMainContentsModule :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AnalysisTypes.C_ContentsKind -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_setMainContentsModule x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (wrapNX id (nd_OP_setMainContentsModule_dot___hash_lambda20 x3 x2 x4 x1)) x2000 x3500))

d_OP_setMainContentsModule_dot___hash_lambda20 :: Curry_AnalysisTypes.C_ContentsKind -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_setMainContentsModule_dot___hash_lambda20 x1 x2 x3 x4 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple6 x6 x7 x8 x9 x10 x11) -> Curry_IOExts.d_C_writeIORef x4 (Curry_Prelude.OP_Tuple6 x6 x7 x8 (Curry_Prelude.OP_Tuple3 (d_OP__case_28 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 Curry_AnalysisTypes.C_OtherText x3500) x3500) x1 x3) x10 x11) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_setMainContentsModule_dot___hash_lambda20 x1 x2 x3 x4 x1002 x3500) (d_OP_setMainContentsModule_dot___hash_lambda20 x1 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_setMainContentsModule_dot___hash_lambda20 x1 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_setMainContentsModule_dot___hash_lambda20 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_setMainContentsModule_dot___hash_lambda20 :: Curry_AnalysisTypes.C_ContentsKind -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_setMainContentsModule_dot___hash_lambda20 x1 x2 x3 x4 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple6 x6 x7 x8 x9 x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (Curry_IOExts.d_C_writeIORef x4 (Curry_Prelude.OP_Tuple6 x6 x7 x8 (Curry_Prelude.OP_Tuple3 (nd_OP__case_28 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 Curry_AnalysisTypes.C_OtherText x3500) x2000 x3500) x1 x3) x10 x11) x3500))
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_setMainContentsModule_dot___hash_lambda20 x1 x2 x3 x4 x1002 x3000 x3500) (nd_OP_setMainContentsModule_dot___hash_lambda20 x1 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_setMainContentsModule_dot___hash_lambda20 x1 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_setMainContentsModule_dot___hash_lambda20 x1 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getContentsModule :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getContentsModule x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) d_OP_getContentsModule_dot___hash_lambda21 x3500

nd_C_getContentsModule :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_getContentsModule x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (wrapNX id nd_OP_getContentsModule_dot___hash_lambda21) x2000 x3500))

d_OP_getContentsModule_dot___hash_lambda21 :: Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getContentsModule_dot___hash_lambda21 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple6 x2 x3 x4 x5 x6 x7) -> d_OP__case_27 x5 x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getContentsModule_dot___hash_lambda21 x1002 x3500) (d_OP_getContentsModule_dot___hash_lambda21 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getContentsModule_dot___hash_lambda21 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getContentsModule_dot___hash_lambda21 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_getContentsModule_dot___hash_lambda21 :: Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_getContentsModule_dot___hash_lambda21 x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple6 x2 x3 x4 x5 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_27 x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_getContentsModule_dot___hash_lambda21 x1002 x3000 x3500) (nd_OP_getContentsModule_dot___hash_lambda21 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_getContentsModule_dot___hash_lambda21 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_getContentsModule_dot___hash_lambda21 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getMainContents :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getMainContents x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) d_OP_getMainContents_dot___hash_lambda22 x3500

nd_C_getMainContents :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_getMainContents x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (wrapNX id nd_OP_getMainContents_dot___hash_lambda22) x2000 x3500))

d_OP_getMainContents_dot___hash_lambda22 :: Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getMainContents_dot___hash_lambda22 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple6 x2 x3 x4 x5 x6 x7) -> d_OP__case_26 x5 x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getMainContents_dot___hash_lambda22 x1002 x3500) (d_OP_getMainContents_dot___hash_lambda22 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getMainContents_dot___hash_lambda22 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getMainContents_dot___hash_lambda22 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_getMainContents_dot___hash_lambda22 :: Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_getMainContents_dot___hash_lambda22 x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple6 x2 x3 x4 x5 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_26 x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_getMainContents_dot___hash_lambda22 x1002 x3000 x3500) (nd_OP_getMainContents_dot___hash_lambda22 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_getMainContents_dot___hash_lambda22 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_getMainContents_dot___hash_lambda22 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getFunctionListKind :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_getFunctionListKind x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) d_OP_getFunctionListKind_dot___hash_lambda23 x3500

nd_C_getFunctionListKind :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
nd_C_getFunctionListKind x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (wrapNX id nd_OP_getFunctionListKind_dot___hash_lambda23) x2000 x3500))

d_OP_getFunctionListKind_dot___hash_lambda23 :: Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_getFunctionListKind_dot___hash_lambda23 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple6 x2 x3 x4 x5 x6 x7) -> Curry_Prelude.d_C_return x6 x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getFunctionListKind_dot___hash_lambda23 x1002 x3500) (d_OP_getFunctionListKind_dot___hash_lambda23 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getFunctionListKind_dot___hash_lambda23 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getFunctionListKind_dot___hash_lambda23 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_getFunctionListKind_dot___hash_lambda23 :: Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
nd_OP_getFunctionListKind_dot___hash_lambda23 x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple6 x2 x3 x4 x5 x6 x7) -> Curry_Prelude.d_C_return x6 x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_getFunctionListKind_dot___hash_lambda23 x1002 x3000 x3500) (nd_OP_getFunctionListKind_dot___hash_lambda23 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_getFunctionListKind_dot___hash_lambda23 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_getFunctionListKind_dot___hash_lambda23 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_setFunctionListKind :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_setFunctionListKind x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_setFunctionListKind_dot___hash_lambda24 x2 x1) x3500

nd_C_setFunctionListKind :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Bool -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_setFunctionListKind x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (wrapNX id (nd_OP_setFunctionListKind_dot___hash_lambda24 x2 x1)) x2000 x3500))

d_OP_setFunctionListKind_dot___hash_lambda24 :: Curry_Prelude.C_Bool -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_setFunctionListKind_dot___hash_lambda24 x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple6 x4 x5 x6 x7 x8 x9) -> Curry_IOExts.d_C_writeIORef x2 (Curry_Prelude.OP_Tuple6 x4 x5 x6 x7 x1 x9) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_setFunctionListKind_dot___hash_lambda24 x1 x2 x1002 x3500) (d_OP_setFunctionListKind_dot___hash_lambda24 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_setFunctionListKind_dot___hash_lambda24 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_setFunctionListKind_dot___hash_lambda24 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_setFunctionListKind_dot___hash_lambda24 :: Curry_Prelude.C_Bool -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_setFunctionListKind_dot___hash_lambda24 x1 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple6 x4 x5 x6 x7 x8 x9) -> Curry_IOExts.d_C_writeIORef x2 (Curry_Prelude.OP_Tuple6 x4 x5 x6 x7 x1 x9) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_setFunctionListKind_dot___hash_lambda24 x1 x2 x1002 x3000 x3500) (nd_OP_setFunctionListKind_dot___hash_lambda24 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_setFunctionListKind_dot___hash_lambda24 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_setFunctionListKind_dot___hash_lambda24 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getAllModules :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 t2 Curry_Imports.C_InterfaceOrFlatProg))) t3 t4 t5 t6) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_C_getAllModules x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) d_OP_getAllModules_dot___hash_lambda25 x3500

d_OP_getAllModules_dot___hash_lambda25 :: (Curry_Prelude.Curry t284,Curry_Prelude.Curry t303,Curry_Prelude.Curry t301,Curry_Prelude.Curry t286,Curry_Prelude.Curry t287,Curry_Prelude.Curry t288,Curry_Prelude.Curry t289) => Curry_Prelude.OP_Tuple6 t284 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t303 (Curry_Prelude.OP_Tuple2 t301 Curry_Imports.C_InterfaceOrFlatProg))) t286 t287 t288 t289 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_OP_getAllModules_dot___hash_lambda25 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple6 x2 x3 x4 x5 x6 x7) -> Curry_Prelude.d_C_return (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot Curry_Imports.d_C_progOfIFFP (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_snd Curry_Prelude.d_C_snd x3500) x3500) x3 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getAllModules_dot___hash_lambda25 x1002 x3500) (d_OP_getAllModules_dot___hash_lambda25 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getAllModules_dot___hash_lambda25 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getAllModules_dot___hash_lambda25 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getIntWithName :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t1) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 t2 Curry_Imports.C_InterfaceOrFlatProg))) t3 t4 t5 t6) -> t1 -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_C_getIntWithName x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_getIntWithName_dot___hash_lambda26 x2) x3500

d_OP_getIntWithName_dot___hash_lambda26 :: (Curry_Prelude.Curry t308,Curry_Prelude.Curry t307,Curry_Prelude.Curry t325,Curry_Prelude.Curry t310,Curry_Prelude.Curry t311,Curry_Prelude.Curry t312,Curry_Prelude.Curry t313) => t307 -> Curry_Prelude.OP_Tuple6 t308 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t307 (Curry_Prelude.OP_Tuple2 t325 Curry_Imports.C_InterfaceOrFlatProg))) t310 t311 t312 t313 -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_getIntWithName_dot___hash_lambda26 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple6 x3 x4 x5 x6 x7 x8) -> Curry_Prelude.d_C_return (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot Curry_Imports.d_C_progOfIFFP (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_snd (Curry_Prelude.d_OP_dot Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x1) x3500) x3500) x3500) x4 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getIntWithName_dot___hash_lambda26 x1 x1002 x3500) (d_OP_getIntWithName_dot___hash_lambda26 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getIntWithName_dot___hash_lambda26 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getIntWithName_dot___hash_lambda26 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getProgWithName :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t1) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t2 t3 t4 t5) -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO t6) -> t1 -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_C_getProgWithName x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_getProgWithName_dot___hash_lambda27 x1 x3 x2) x3500

nd_C_getProgWithName :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t1) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t2 t3 t4 t5) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t6) -> t1 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
nd_C_getProgWithName x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (wrapNX id (nd_OP_getProgWithName_dot___hash_lambda27 x1 x3 x2)) x2000 x3500))

d_OP_getProgWithName_dot___hash_lambda27 :: (Curry_Prelude.Curry t398,Curry_Prelude.Curry t382,Curry_Prelude.Curry t381,Curry_Prelude.Curry t384,Curry_Prelude.Curry t385,Curry_Prelude.Curry t386,Curry_Prelude.Curry t387) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t382 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t381 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t384 t385 t386 t387) -> t381 -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO t398) -> Curry_Prelude.OP_Tuple6 t382 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t381 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t384 t385 t386 t387 -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_getProgWithName_dot___hash_lambda27 x1 x2 x3 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple6 x5 x6 x7 x8 x9 x10) -> Curry_Imports.d_C_ifOrProg (d_OP_getProgWithName_dot___hash_lambda27_dot___hash_lambda28 x1 x2 x3) Curry_Prelude.d_C_return (Curry_Prelude.d_C_snd (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x2 x6 x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getProgWithName_dot___hash_lambda27 x1 x2 x3 x1002 x3500) (d_OP_getProgWithName_dot___hash_lambda27 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getProgWithName_dot___hash_lambda27 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getProgWithName_dot___hash_lambda27 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_getProgWithName_dot___hash_lambda27 :: (Curry_Prelude.Curry t398,Curry_Prelude.Curry t382,Curry_Prelude.Curry t381,Curry_Prelude.Curry t384,Curry_Prelude.Curry t385,Curry_Prelude.Curry t386,Curry_Prelude.Curry t387) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t382 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t381 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t384 t385 t386 t387) -> t381 -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t398) -> Curry_Prelude.OP_Tuple6 t382 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t381 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t384 t385 t386 t387 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
nd_OP_getProgWithName_dot___hash_lambda27 x1 x2 x3 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple6 x5 x6 x7 x8 x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Imports.nd_C_ifOrProg (wrapNX id (nd_OP_getProgWithName_dot___hash_lambda27_dot___hash_lambda28 x1 x2 x3)) (wrapDX id Curry_Prelude.d_C_return) (Curry_Prelude.d_C_snd (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x2 x6 x3500) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_getProgWithName_dot___hash_lambda27 x1 x2 x3 x1002 x3000 x3500) (nd_OP_getProgWithName_dot___hash_lambda27 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_getProgWithName_dot___hash_lambda27 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_getProgWithName_dot___hash_lambda27 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getProgWithName_dot___hash_lambda27_dot___hash_lambda28 :: (Curry_Prelude.Curry t382,Curry_Prelude.Curry t384,Curry_Prelude.Curry t385,Curry_Prelude.Curry t386,Curry_Prelude.Curry t387,Curry_Prelude.Curry t381,Curry_Prelude.Curry t398) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t382 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t381 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t384 t385 t386 t387) -> t381 -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO t398) -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_getProgWithName_dot___hash_lambda27_dot___hash_lambda28 x1 x2 x3 x4 x3500 = d_C_readProgAndStore x1 x3 x2 x3500

nd_OP_getProgWithName_dot___hash_lambda27_dot___hash_lambda28 :: (Curry_Prelude.Curry t382,Curry_Prelude.Curry t384,Curry_Prelude.Curry t385,Curry_Prelude.Curry t386,Curry_Prelude.Curry t387,Curry_Prelude.Curry t381,Curry_Prelude.Curry t398) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t382 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t381 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t384 t385 t386 t387) -> t381 -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t398) -> Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
nd_OP_getProgWithName_dot___hash_lambda27_dot___hash_lambda28 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_readProgAndStore x1 x3 x2 x2000 x3500))

d_C_getAllTypes :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t7,Curry_Prelude.Curry t1) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_List t1)))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 t2 Curry_Imports.C_InterfaceOrFlatProg))) t3 t4 t5 t6) -> t7 -> t1 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl)
d_C_getAllTypes x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllImportsOfModule x1 x3 x3500) (d_OP_getAllTypes_dot___hash_lambda29 x1) x3500

d_OP_getAllTypes_dot___hash_lambda29 :: (Curry_Prelude.Curry t463,Curry_Prelude.Curry t492,Curry_Prelude.Curry t476,Curry_Prelude.Curry t477,Curry_Prelude.Curry t478,Curry_Prelude.Curry t479,Curry_Prelude.Curry t501) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 t463 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t501 (Curry_Prelude.OP_List t501)))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t501 (Curry_Prelude.OP_Tuple2 t492 Curry_Imports.C_InterfaceOrFlatProg))) t476 t477 t478 t479) -> Curry_Prelude.OP_List t501 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl)
d_OP_getAllTypes_dot___hash_lambda29 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_getAllTypes_dot___hash_lambda29_dot___hash_lambda30 x2) x3500

d_OP_getAllTypes_dot___hash_lambda29_dot___hash_lambda30 :: (Curry_Prelude.Curry t463,Curry_Prelude.Curry t501,Curry_Prelude.Curry t492,Curry_Prelude.Curry t476,Curry_Prelude.Curry t477,Curry_Prelude.Curry t478,Curry_Prelude.Curry t479) => Curry_Prelude.OP_List t501 -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 t463 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t501 (Curry_Prelude.OP_List t501)))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t501 (Curry_Prelude.OP_Tuple2 t492 Curry_Imports.C_InterfaceOrFlatProg))) t476 t477 t478 t479 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl)
d_OP_getAllTypes_dot___hash_lambda29_dot___hash_lambda30 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple6 x3 x4 x5 x6 x7 x8) -> Curry_Prelude.d_C_return (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_FlatCurryGoodies.d_C_progTypes x3500) (Curry_Prelude.d_OP_dot Curry_Imports.d_C_progOfIFFP (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_snd Curry_Prelude.d_C_snd x3500) x3500) x3500) x3500) (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem x1) Curry_Prelude.d_C_fst x3500) x4 x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getAllTypes_dot___hash_lambda29_dot___hash_lambda30 x1 x1002 x3500) (d_OP_getAllTypes_dot___hash_lambda29_dot___hash_lambda30 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getAllTypes_dot___hash_lambda29_dot___hash_lambda30 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getAllTypes_dot___hash_lambda29_dot___hash_lambda30 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getAllFunctions :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl)
d_C_getAllFunctions x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllImportsOfModule x1 x3 x3500) (d_OP_getAllFunctions_dot___hash_lambda31 x1 x2) x3500

nd_C_getAllFunctions :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl)
nd_C_getAllFunctions x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getAllImportsOfModule x1 x3 x3500) (wrapNX id (nd_OP_getAllFunctions_dot___hash_lambda31 x1 x2)) x2000 x3500))

d_OP_getAllFunctions_dot___hash_lambda31 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl)
d_OP_getAllFunctions_dot___hash_lambda31 x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 x1 x3 x2) x3500

nd_OP_getAllFunctions_dot___hash_lambda31 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl)
nd_OP_getAllFunctions_dot___hash_lambda31 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (wrapNX id (nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 x1 x3 x2)) x2000 x3500))

d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl)
d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple6 x5 x6 x7 x8 x9 x10) -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO_ (d_C_readProgAndStoreIfNecessary x1 x3) x3500) (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem x2) Curry_Prelude.d_C_fst x3500) x6 x3500) x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 x2) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 x1002 x3500) (d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl)
nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple6 x5 x6 x7 x8 x9 x10) -> let
          x2008 = x3000
           in (seq x2008 (let
               x2005 = leftSupply x2008
               x2007 = rightSupply x2008
                in (seq x2005 (seq x2007 (Curry_Prelude.d_OP_gt_gt (let
                    x2004 = leftSupply x2005
                    x2006 = rightSupply x2005
                     in (seq x2004 (seq x2006 (let
                         x2000 = leftSupply x2006
                         x2003 = rightSupply x2006
                          in (seq x2000 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_mapIO_ (wrapNX id (nd_C_readProgAndStoreIfNecessary x1 x3)) x2000 x3500) (let
                              x2002 = leftSupply x2003
                              x2001 = rightSupply x2003
                               in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_filter (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_flip (wrapNX id Curry_Prelude.nd_C_elem) x2)) (wrapDX id Curry_Prelude.d_C_fst) x2001 x3500) x6 x2002 x3500)))) x2004 x3500))))))) (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (wrapNX id (nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 x2)) x2007 x3500) x3500)))))
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 x1002 x3000 x3500) (nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl)
d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple6 x3 x4 x5 x6 x7 x8) -> Curry_Prelude.d_C_return (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_FlatCurryGoodies.d_C_progFuncs x3500) (Curry_Prelude.d_OP_dot Curry_Imports.d_C_progOfIFFP (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_snd Curry_Prelude.d_C_snd x3500) x3500) x3500) x3500) (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem x1) Curry_Prelude.d_C_fst x3500) x4 x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 x1 x1002 x3500) (d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl)
nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple6 x3 x4 x5 x6 x7 x8) -> let
          x2013 = x3000
           in (seq x2013 (Curry_Prelude.d_C_return (let
               x2012 = leftSupply x2013
               x2014 = rightSupply x2013
                in (seq x2012 (seq x2014 (let
                    x2008 = leftSupply x2014
                    x2011 = rightSupply x2014
                     in (seq x2008 (seq x2011 (Curry_Prelude.nd_C_apply (let
                         x2007 = leftSupply x2008
                         x2005 = rightSupply x2008
                          in (seq x2007 (seq x2005 (Curry_Prelude.nd_C_concatMap (let
                              x2004 = leftSupply x2005
                              x2006 = rightSupply x2005
                               in (seq x2004 (seq x2006 (let
                                   x2000 = leftSupply x2006
                                   x2003 = rightSupply x2006
                                    in (seq x2000 (seq x2003 (Curry_Prelude.nd_OP_dot (Curry_FlatCurryGoodies.nd_C_progFuncs x2000 x3500) (let
                                        x2002 = leftSupply x2003
                                        x2001 = rightSupply x2003
                                         in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Imports.d_C_progOfIFFP) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_snd) (wrapDX id Curry_Prelude.d_C_snd) x2001 x3500) x2002 x3500)))) x2004 x3500))))))) x2007 x3500)))) (let
                         x2010 = leftSupply x2011
                         x2009 = rightSupply x2011
                          in (seq x2010 (seq x2009 (Curry_Prelude.nd_C_filter (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_flip (wrapNX id Curry_Prelude.nd_C_elem) x1)) (wrapDX id Curry_Prelude.d_C_fst) x2009 x3500) x4 x2010 x3500)))) x2012 x3500))))))) x3500))
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 x1 x1002 x3000 x3500) (nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_getAllFunctions_dot___hash_lambda31_dot___hash_lambda32_dot___hash_lambda33 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getAllFunctionNames :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_getAllFunctionNames x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllImportsOfModule x1 x2 x3500) (d_OP_getAllFunctionNames_dot___hash_lambda34 x1) x3500

nd_C_getAllFunctionNames :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_C_getAllFunctionNames x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getAllImportsOfModule x1 x2 x3500) (wrapNX id (nd_OP_getAllFunctionNames_dot___hash_lambda34 x1)) x2000 x3500))

d_OP_getAllFunctionNames_dot___hash_lambda34 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_getAllFunctionNames_dot___hash_lambda34 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 x2) x3500

nd_OP_getAllFunctionNames_dot___hash_lambda34 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_OP_getAllFunctionNames_dot___hash_lambda34 x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (wrapNX id (nd_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 x2)) x2000 x3500))

d_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple6 x3 x4 x5 x6 x7 x8) -> Curry_Prelude.d_C_return (Curry_Prelude.d_C_map (Curry_FlatCurryGoodies.d_C_funcName x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_FlatCurryGoodies.d_C_progFuncs x3500) (Curry_Prelude.d_OP_dot Curry_Imports.d_C_progOfIFFP (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_snd Curry_Prelude.d_C_snd x3500) x3500) x3500) x3500) (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem x1) Curry_Prelude.d_C_fst x3500) x4 x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 x1 x1002 x3500) (d_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple6 x3 x4 x5 x6 x7 x8) -> let
          x2017 = x3000
           in (seq x2017 (Curry_Prelude.d_C_return (let
               x2016 = leftSupply x2017
               x2018 = rightSupply x2017
                in (seq x2016 (seq x2018 (let
                    x2000 = leftSupply x2018
                    x2014 = rightSupply x2018
                     in (seq x2000 (seq x2014 (Curry_Prelude.nd_C_map (Curry_FlatCurryGoodies.nd_C_funcName x2000 x3500) (let
                         x2013 = leftSupply x2014
                         x2015 = rightSupply x2014
                          in (seq x2013 (seq x2015 (let
                              x2009 = leftSupply x2015
                              x2012 = rightSupply x2015
                               in (seq x2009 (seq x2012 (Curry_Prelude.nd_C_apply (let
                                   x2008 = leftSupply x2009
                                   x2006 = rightSupply x2009
                                    in (seq x2008 (seq x2006 (Curry_Prelude.nd_C_concatMap (let
                                        x2005 = leftSupply x2006
                                        x2007 = rightSupply x2006
                                         in (seq x2005 (seq x2007 (let
                                             x2001 = leftSupply x2007
                                             x2004 = rightSupply x2007
                                              in (seq x2001 (seq x2004 (Curry_Prelude.nd_OP_dot (Curry_FlatCurryGoodies.nd_C_progFuncs x2001 x3500) (let
                                                  x2003 = leftSupply x2004
                                                  x2002 = rightSupply x2004
                                                   in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Imports.d_C_progOfIFFP) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_snd) (wrapDX id Curry_Prelude.d_C_snd) x2002 x3500) x2003 x3500)))) x2005 x3500))))))) x2008 x3500)))) (let
                                   x2011 = leftSupply x2012
                                   x2010 = rightSupply x2012
                                    in (seq x2011 (seq x2010 (Curry_Prelude.nd_C_filter (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_flip (wrapNX id Curry_Prelude.nd_C_elem) x1)) (wrapDX id Curry_Prelude.d_C_fst) x2010 x3500) x4 x2011 x3500)))) x2013 x3500))))))) x2016 x3500))))))) x3500))
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 x1 x1002 x3000 x3500) (nd_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_getAllFunctionNames_dot___hash_lambda34_dot___hash_lambda35 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getCurrentFunctionAnalysis :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))
d_C_getCurrentFunctionAnalysis x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) d_OP_getCurrentFunctionAnalysis_dot___hash_lambda36 x3500

nd_C_getCurrentFunctionAnalysis :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))
nd_C_getCurrentFunctionAnalysis x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (wrapNX id nd_OP_getCurrentFunctionAnalysis_dot___hash_lambda36) x2000 x3500))

d_OP_getCurrentFunctionAnalysis_dot___hash_lambda36 :: Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))
d_OP_getCurrentFunctionAnalysis_dot___hash_lambda36 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple6 x2 x3 x4 x5 x6 x7) -> Curry_Prelude.d_C_return x7 x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getCurrentFunctionAnalysis_dot___hash_lambda36 x1002 x3500) (d_OP_getCurrentFunctionAnalysis_dot___hash_lambda36 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getCurrentFunctionAnalysis_dot___hash_lambda36 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getCurrentFunctionAnalysis_dot___hash_lambda36 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_getCurrentFunctionAnalysis_dot___hash_lambda36 :: Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))
nd_OP_getCurrentFunctionAnalysis_dot___hash_lambda36 x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple6 x2 x3 x4 x5 x6 x7) -> Curry_Prelude.d_C_return x7 x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_getCurrentFunctionAnalysis_dot___hash_lambda36 x1002 x3000 x3500) (nd_OP_getCurrentFunctionAnalysis_dot___hash_lambda36 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_getCurrentFunctionAnalysis_dot___hash_lambda36 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_getCurrentFunctionAnalysis_dot___hash_lambda36 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_setCurrentFunctionAnalysis :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_setCurrentFunctionAnalysis x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 x2 x1) x3500

nd_C_setCurrentFunctionAnalysis :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_setCurrentFunctionAnalysis x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (wrapNX id (nd_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 x2 x1)) x2000 x3500))

d_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 :: Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult) -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple6 x4 x5 x6 x7 x8 x9) -> Curry_IOExts.d_C_writeIORef x2 (Curry_Prelude.OP_Tuple6 x4 x5 x6 x7 x8 x1) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 x1 x2 x1002 x3500) (d_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 :: Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult) -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 x1 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple6 x4 x5 x6 x7 x8 x9) -> Curry_IOExts.d_C_writeIORef x2 (Curry_Prelude.OP_Tuple6 x4 x5 x6 x7 x8 x1) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 x1 x2 x1002 x3000 x3500) (nd_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_setCurrentFunctionAnalysis_dot___hash_lambda37 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_readProgAndStore :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t1) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t2 t3 t4 t5) -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO t6) -> t1 -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_C_readProgAndStore x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (d_OP_readProgAndStore_dot___hash_lambda38 x1 x3 x2) x3500

nd_C_readProgAndStore :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t1) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t2 t3 t4 t5) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t6) -> t1 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
nd_C_readProgAndStore x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IOExts.d_C_readIORef x1 x3500) (wrapNX id (nd_OP_readProgAndStore_dot___hash_lambda38 x1 x3 x2)) x2000 x3500))

d_OP_readProgAndStore_dot_update_dot_269 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_Tuple2 t1 Curry_Imports.C_InterfaceOrFlatProg)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_Tuple2 t1 Curry_Imports.C_InterfaceOrFlatProg))
d_OP_readProgAndStore_dot_update_dot_269 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_25 x1 x2 x5 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readProgAndStore_dot_update_dot_269 x1 x2 x1002 x3500) (d_OP_readProgAndStore_dot_update_dot_269 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readProgAndStore_dot_update_dot_269 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readProgAndStore_dot_update_dot_269 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_readProgAndStore_dot___hash_lambda38 :: (Curry_Prelude.Curry t366,Curry_Prelude.Curry t358,Curry_Prelude.Curry t336,Curry_Prelude.Curry t360,Curry_Prelude.Curry t361,Curry_Prelude.Curry t362,Curry_Prelude.Curry t363) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t358 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t336 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t360 t361 t362 t363) -> t336 -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO t366) -> Curry_Prelude.OP_Tuple6 t358 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t336 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t360 t361 t362 t363 -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_readProgAndStore_dot___hash_lambda38 x1 x2 x3 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple6 x5 x6 x7 x8 x9 x10) -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Imports.d_C_readFlatCurryFileInLoadPath x3 (Curry_Prelude.d_C_fst (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x2 x6 x3500) x3500) x3500) x3500) (d_OP_readProgAndStore_dot___hash_lambda38_dot___hash_lambda39 x8 x10 x9 x7 x1 x2 x6 x5) x3500
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readProgAndStore_dot___hash_lambda38 x1 x2 x3 x1002 x3500) (d_OP_readProgAndStore_dot___hash_lambda38 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readProgAndStore_dot___hash_lambda38 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readProgAndStore_dot___hash_lambda38 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_readProgAndStore_dot___hash_lambda38 :: (Curry_Prelude.Curry t366,Curry_Prelude.Curry t358,Curry_Prelude.Curry t336,Curry_Prelude.Curry t360,Curry_Prelude.Curry t361,Curry_Prelude.Curry t362,Curry_Prelude.Curry t363) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t358 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t336 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t360 t361 t362 t363) -> t336 -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t366) -> Curry_Prelude.OP_Tuple6 t358 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t336 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t360 t361 t362 t363 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
nd_OP_readProgAndStore_dot___hash_lambda38 x1 x2 x3 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple6 x5 x6 x7 x8 x9 x10) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Imports.nd_C_readFlatCurryFileInLoadPath x3 (Curry_Prelude.d_C_fst (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x2 x6 x3500) x3500) x3500) x2000 x3500) (wrapDX id (d_OP_readProgAndStore_dot___hash_lambda38_dot___hash_lambda39 x8 x10 x9 x7 x1 x2 x6 x5)) x2001 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple6 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_readProgAndStore_dot___hash_lambda38 x1 x2 x3 x1002 x3000 x3500) (nd_OP_readProgAndStore_dot___hash_lambda38 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple6 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_readProgAndStore_dot___hash_lambda38 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple6 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_readProgAndStore_dot___hash_lambda38 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple6 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_readProgAndStore_dot___hash_lambda38_dot___hash_lambda39 :: (Curry_Prelude.Curry t360,Curry_Prelude.Curry t361,Curry_Prelude.Curry t362,Curry_Prelude.Curry t363,Curry_Prelude.Curry t336,Curry_Prelude.Curry t358) => t361 -> t363 -> t362 -> t360 -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t358 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t336 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t360 t361 t362 t363) -> t336 -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t336 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg)) -> t358 -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_readProgAndStore_dot___hash_lambda38_dot___hash_lambda39 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_IOExts.d_C_writeIORef x5 (Curry_Prelude.OP_Tuple6 x8 (d_OP_readProgAndStore_dot_update_dot_269 x6 x9 x7 x3500) x4 x1 x3 x2) x3500) (Curry_Prelude.d_C_return x9 x3500) x3500

d_C_readProgAndStoreIfNecessary :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t1,Curry_Prelude.Curry t7) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t2 t3 t4 t5) -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO t6) -> Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 t7 Curry_Imports.C_InterfaceOrFlatProg) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_readProgAndStoreIfNecessary x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_22 x1 x2 x4 x5 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_readProgAndStoreIfNecessary x1 x2 x1002 x3500) (d_C_readProgAndStoreIfNecessary x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_readProgAndStoreIfNecessary x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_readProgAndStoreIfNecessary x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_readProgAndStoreIfNecessary :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t1,Curry_Prelude.Curry t7) => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) t2 t3 t4 t5) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t6) -> Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 t7 Curry_Imports.C_InterfaceOrFlatProg) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_readProgAndStoreIfNecessary x1 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_22 x1 x2 x4 x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_readProgAndStoreIfNecessary x1 x2 x1002 x3000 x3500) (nd_C_readProgAndStoreIfNecessary x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_readProgAndStoreIfNecessary x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_readProgAndStoreIfNecessary x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_findDecl4name :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_FlatCurry.C_FuncDecl
d_C_findDecl4name x1 x2 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '4'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))) x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_20 x2 x3 x4 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3500) x3 x3500) x2 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_findDecl4name x1002 x2 x3500) (d_C_findDecl4name x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_findDecl4name z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_findDecl4name x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_browserGUI :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_GUI.C_Widget
nd_C_browserGUI x1 x2 x3 x4 x3000 x3500 = let
     x2048 = x3000
      in (seq x2048 (let
          x2049 = leftSupply x2048
          x2051 = rightSupply x2048
           in (seq x2049 (seq x2051 (let
               x2041 = leftSupply x2049
               x2050 = rightSupply x2049
                in (seq x2041 (seq x2050 (let
                    x2043 = leftSupply x2050
                    x2044 = rightSupply x2050
                     in (seq x2043 (seq x2044 (let
                         x2045 = leftSupply x2051
                         x2052 = rightSupply x2051
                          in (seq x2045 (seq x2052 (let
                              x2046 = leftSupply x2052
                              x2047 = rightSupply x2052
                               in (seq x2046 (seq x2047 (let
                                   x5 = generate x2043
                                   x6 = generate x2044
                                   x7 = generate x2045
                                   x8 = generate x2046
                                   x9 = generate x2047
                                    in (let
                                        x2040 = leftSupply x2041
                                        x2042 = rightSupply x2041
                                         in (seq x2040 (seq x2042 (let
                                             x2000 = leftSupply x2042
                                             x2038 = rightSupply x2042
                                              in (seq x2000 (seq x2038 (Curry_Prelude.nd_C_apply (Curry_GUI.nd_C_col x2000 x3500) (Curry_Prelude.OP_Cons (let
                                                  x2037 = leftSupply x2038
                                                  x2039 = rightSupply x2038
                                                   in (seq x2037 (seq x2039 (let
                                                       x2001 = leftSupply x2039
                                                       x2036 = rightSupply x2039
                                                        in (seq x2001 (seq x2036 (Curry_Prelude.nd_C_apply (Curry_GUI.nd_C_row x2001 x3500) (let
                                                            x2018 = leftSupply x2036
                                                            x2035 = rightSupply x2036
                                                             in (seq x2018 (seq x2035 (Curry_Prelude.OP_Cons (Curry_GUI.C_Col (Curry_Prelude.OP_Cons Curry_GUI.C_LeftAlign Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_GUI.C_Label (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List))))))))))))))))))))))))))) Curry_Prelude.OP_List)) (let
                                                                 x2004 = leftSupply x2018
                                                                 x2017 = rightSupply x2018
                                                                  in (seq x2004 (seq x2017 (Curry_Prelude.OP_Cons (let
                                                                      x2003 = leftSupply x2004
                                                                      x2002 = rightSupply x2004
                                                                       in (seq x2003 (seq x2002 (Curry_GUI.nd_C_ListBoxScroll (Curry_Prelude.OP_Cons (Curry_GUI.C_WRef x2) (Curry_Prelude.OP_Cons (Curry_GUI.C_List x4) (Curry_Prelude.OP_Cons (Curry_GUI.C_Width (Curry_Prelude.C_Int 20#)) (Curry_Prelude.OP_Cons (Curry_GUI.C_Height (Curry_Prelude.C_Int 14#)) (Curry_Prelude.OP_Cons (Curry_GUI.nd_C_Cmd (wrapNX id (nd_OP_browserGUI_dot_showBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_selmod_dot_295 x1 x5 x6 x2 x3)))) x2002 x3500) (Curry_Prelude.OP_Cons (Curry_GUI.C_Background (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons Curry_GUI.C_Fill Curry_Prelude.OP_List))))))) x2003 x3500)))) (let
                                                                      x2007 = leftSupply x2017
                                                                      x2016 = rightSupply x2017
                                                                       in (seq x2007 (seq x2016 (Curry_Prelude.OP_Cons (Curry_GUI.C_MenuButton (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Menu (let
                                                                           x2006 = leftSupply x2007
                                                                           x2005 = rightSupply x2007
                                                                            in (seq x2006 (seq x2005 (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_browserGUI_dot___hash_lambda112 x1 x5 x6 x2 x8 x3)) (Curry_BrowserAnalysis.nd_C_allFunctionAnalyses x2005 x3500) x2006 x3500))))) Curry_Prelude.OP_List))) (let
                                                                           x2011 = leftSupply x2016
                                                                           x2015 = rightSupply x2016
                                                                            in (seq x2011 (seq x2015 (Curry_Prelude.OP_Cons (let
                                                                                x2010 = leftSupply x2011
                                                                                x2012 = rightSupply x2011
                                                                                 in (seq x2010 (seq x2012 (let
                                                                                     x2008 = leftSupply x2012
                                                                                     x2009 = rightSupply x2012
                                                                                      in (seq x2008 (seq x2009 (Curry_Prelude.nd_C_apply (Curry_GUI.nd_C_row x2008 x3500) (Curry_Prelude.OP_Cons (Curry_GUI.C_MenuButton (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Menu (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_executeForModule_dot_295 x1 x2 x3 (wrapDX (wrapNX id) (acceptCs id (nd_OP_browserGUI_dot_showExportedFuns_dot_295 x1 x6 x8))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_executeForModule_dot_295 x1 x2 x3 (wrapDX (wrapNX id) (acceptCs id (nd_OP_browserGUI_dot_showAllModuleFuns_dot_295 x1 x6 x8))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_executeForModule_dot_295 x1 x2 x3 (wrapDX (wrapNX id) (acceptCs id (nd_OP_browserGUI_dot_showAllExportedFuns_dot_295 x1 x6 x8))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_selectDirectCalls_dot_295 x1 x6 x2 x8)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_selectInDirectCalls_dot_295 x1 x6 x2 x8)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_GUI.C_CheckButton (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_WRef x7) (Curry_Prelude.OP_Cons (Curry_GUI.nd_C_Cmd (wrapNX id (nd_OP_browserGUI_dot_focusFunctionIfSelected_dot_295 x7 x1 x6 x3)) x2009 x3500) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List)) x2010 x3500))))))) (Curry_Prelude.OP_Cons (let
                                                                                x2014 = leftSupply x2015
                                                                                x2013 = rightSupply x2015
                                                                                 in (seq x2014 (seq x2013 (Curry_GUI.nd_C_ListBoxScroll (Curry_Prelude.OP_Cons (Curry_GUI.C_WRef x6) (Curry_Prelude.OP_Cons (Curry_GUI.C_Width (Curry_Prelude.C_Int 20#)) (Curry_Prelude.OP_Cons (Curry_GUI.C_Height (Curry_Prelude.C_Int 16#)) (Curry_Prelude.OP_Cons (Curry_GUI.nd_C_Cmd (wrapNX id (nd_OP_browserGUI_dot_showBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_selectFunction_dot_295 x7 x1 x5 x6 x2 x8 x3)))) x2013 x3500) (Curry_Prelude.OP_Cons (Curry_GUI.C_Background (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons Curry_GUI.C_Fill Curry_Prelude.OP_List)))))) x2014 x3500)))) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Col (Curry_Prelude.OP_Cons Curry_GUI.C_LeftAlign Curry_Prelude.OP_List) (let
                                                                 x2026 = leftSupply x2035
                                                                 x2034 = rightSupply x2035
                                                                  in (seq x2026 (seq x2034 (Curry_Prelude.OP_Cons (let
                                                                      x2025 = leftSupply x2026
                                                                      x2027 = rightSupply x2026
                                                                       in (seq x2025 (seq x2027 (let
                                                                           x2019 = leftSupply x2027
                                                                           x2024 = rightSupply x2027
                                                                            in (seq x2019 (seq x2024 (Curry_Prelude.nd_C_apply (Curry_GUI.nd_C_row x2019 x3500) (let
                                                                                x2020 = leftSupply x2024
                                                                                x2023 = rightSupply x2024
                                                                                 in (seq x2020 (seq x2023 (Curry_Prelude.OP_Cons (Curry_GUI.nd_C_Button (wrapNX id (nd_OP_browserGUI_dot_showBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_executeForModule_dot_295 x1 x2 x3 (wrapDX (wrapNX id) (acceptCs id (nd_OP_browserGUI_dot_showSource_dot_295 x1 x3))))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))) Curry_Prelude.OP_List) x2020 x3500) (Curry_Prelude.OP_Cons (Curry_GUI.C_MenuButton (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Menu (let
                                                                                     x2022 = leftSupply x2023
                                                                                     x2021 = rightSupply x2023
                                                                                      in (seq x2022 (seq x2021 (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_browserGUI_dot___hash_lambda113 x1 x2 x8 x3)) (Curry_BrowserAnalysis.nd_C_moduleAnalyses x2021 x3500) x2022 x3500))))) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_GUI.C_MenuButton (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Menu (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_executeForModule_dot_295 x1 x2 x3 (wrapDX (wrapNX id) (acceptCs id (nd_OP_browserGUI_dot_showImpCalls_dot_295 x1 x8 x3))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_showImportGraph_dot_295 x1 x3)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))))) Curry_Prelude.OP_List))) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_GUI.C_MenuButton (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Menu (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_executeForModule_dot_295 x1 x2 x3 (wrapDX (wrapNX id) (acceptCs id (nd_OP_browserGUI_dot_showModuleInfo_dot_295 x1 x3))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x8 (wrapDX id (d_OP_browserGUI_dot_saveMainText_dot_295 x3)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))))))))))))))) (Curry_Prelude.OP_Cons Curry_GUI.C_MSeparator (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapDX id d_OP_browserGUI_dot___hash_lambda114) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_GUI.C_MenuButton (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Menu (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id nd_OP_browserGUI_dot_setViewDot_dot_295) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) Curry_Prelude.OP_List)) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Label (Curry_Prelude.OP_Cons Curry_GUI.C_FillX Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_GUI.C_MenuButton (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Menu (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_help_dot_295 x1 x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.OP_Cons Curry_GUI.C_MSeparator (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_help_dot_295 x1 x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_help_dot_295 x1 x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List))) Curry_Prelude.OP_List)))))))))) x2025 x3500))))))) (let
                                                                      x2028 = leftSupply x2034
                                                                      x2033 = rightSupply x2034
                                                                       in (seq x2028 (seq x2033 (Curry_Prelude.OP_Cons (Curry_GUI.nd_C_TextEditScroll (Curry_Prelude.OP_Cons (Curry_GUI.C_WRef x3) (Curry_Prelude.OP_Cons (Curry_GUI.C_Height (Curry_Prelude.C_Int 25#)) (Curry_Prelude.OP_Cons (Curry_GUI.C_Width (Curry_Prelude.C_Int 80#)) (Curry_Prelude.OP_Cons (Curry_GUI.C_Background (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)))) x2028 x3500) (let
                                                                           x2031 = leftSupply x2033
                                                                           x2032 = rightSupply x2033
                                                                            in (seq x2031 (seq x2032 (Curry_Prelude.OP_Cons (Curry_GUI.C_Row (Curry_Prelude.OP_Cons Curry_GUI.C_LeftAlign Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_GUI.C_Label (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List))))))))))))))))))))))))))) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_GUI.C_Entry (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (d_C_noAnalysisText x3500)) (Curry_Prelude.OP_Cons (Curry_GUI.C_WRef x9) (Curry_Prelude.OP_Cons (Curry_GUI.C_Background (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons Curry_GUI.C_FillX Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_MenuButton (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Menu (Curry_Prelude.OP_Cons (Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x8 (wrapNX id (nd_OP_browserGUI_dot_deselectFunAna_dot_295 x9 x1 x5)))) (d_C_noAnalysisText x3500)) (let
                                                                                x2030 = leftSupply x2031
                                                                                x2029 = rightSupply x2031
                                                                                 in (seq x2030 (seq x2029 (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_browserGUI_dot___hash_lambda115 x9 x1 x5 x6 x2 x8 x3)) (Curry_BrowserAnalysis.nd_C_functionAnalyses x2029 x3500) x2030 x3500)))))) Curry_Prelude.OP_List))) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_GUI.nd_C_TextEditScroll (Curry_Prelude.OP_Cons (Curry_GUI.C_WRef x5) (Curry_Prelude.OP_Cons (Curry_GUI.C_Height (Curry_Prelude.C_Int 5#)) (Curry_Prelude.OP_Cons (Curry_GUI.C_Width (Curry_Prelude.C_Int 72#)) (Curry_Prelude.OP_Cons (Curry_GUI.C_Background (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)))) x2032 x3500) Curry_Prelude.OP_List)))))))))))))) Curry_Prelude.OP_List))))) x2037 x3500))))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Label (Curry_Prelude.OP_Cons (Curry_GUI.C_WRef x8) (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))))))))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Background (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons Curry_GUI.C_FillX Curry_Prelude.OP_List))))) Curry_Prelude.OP_List)) x2040 x3500))))))))))))))))))))))))

d_OP_browserGUI_dot_saveMainText_dot_295 :: Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_saveMainText_dot_295 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_GUI.d_C_getSaveFile x3500) (d_OP_browserGUI_dot_saveMainText_dot_295_dot___hash_lambda40 x2 x1) x3500

d_OP_browserGUI_dot_saveMainText_dot_295_dot___hash_lambda40 :: Curry_GUI.C_GuiPort -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_saveMainText_dot_295_dot___hash_lambda40 x1 x2 x3 x3500 = d_OP__case_18 x1 x2 x3 (Curry_Prelude.d_C_null x3 x3500) x3500

d_OP_browserGUI_dot_putMainMessage_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_putMainMessage_dot_295 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x2 x4 x3 x3500) (d_C_setMainContentsModule x1 Curry_Prelude.OP_List Curry_AnalysisTypes.C_OtherText x4 x3500) x3500

nd_OP_browserGUI_dot_putMainMessage_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_putMainMessage_dot_295 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x2 x4 x3 x3500) (nd_C_setMainContentsModule x1 Curry_Prelude.OP_List Curry_AnalysisTypes.C_OtherText x4 x2000 x3500) x3500))

nd_OP_browserGUI_dot_setViewDot_dot_295 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1)
nd_OP_browserGUI_dot_setViewDot_dot_295 x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_ShowGraph.d_C_getDotViewCmd x3500) (wrapNX id nd_OP_browserGUI_dot_setViewDot_dot_295_dot___hash_lambda41) x2000 x3500))

nd_OP_browserGUI_dot_setViewDot_dot_295_dot___hash_lambda41 :: Curry_Prelude.Curry t1662 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1662)
nd_OP_browserGUI_dot_setViewDot_dot_295_dot___hash_lambda41 x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_OP_gt_gt (nd_C_getAnswer (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)))))))))))))))))))))))))) x1 (wrapDX id (d_OP_browserGUI_dot_setViewDot_dot_295_dot___hash_lambda41_dot___hash_lambda42 x1)) x2000 x3500) (Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500) x3500))

d_OP_browserGUI_dot_setViewDot_dot_295_dot___hash_lambda41_dot___hash_lambda42 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_setViewDot_dot_295_dot___hash_lambda41_dot___hash_lambda42 x1 x2 x3500 = d_OP__case_17 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 x2 x3500) x3500

d_OP_browserGUI_dot_help_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_help_dot_295 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile (Curry_Prelude.d_OP_plus_plus (d_C_browserDir x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) x3 x3500) x3500) x3500) (d_OP_browserGUI_dot_putMainMessage_dot_295 x1 x2 x4) x3500

nd_OP_browserGUI_dot_help_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_help_dot_295 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_C_readFile (Curry_Prelude.d_OP_plus_plus (d_C_browserDir x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) x3 x3500) x3500) x3500) (wrapNX id (nd_OP_browserGUI_dot_putMainMessage_dot_295 x1 x2 x4)) x2000 x3500))

d_OP_browserGUI_dot_showBusy_dot_295 :: Curry_Prelude.Curry t0 => Curry_GUI.C_WidgetRef -> (Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO t0) -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showBusy_dot_295 x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setConfig x1 (Curry_GUI.C_Background (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))) x3 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setConfig x1 (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List)))))))))))))))) x3 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_System.d_C_getCPUTime x3500) (d_OP_browserGUI_dot_showBusy_dot_295_dot___hash_lambda43 x3 x2 x1) x3500) x3500) x3500

nd_OP_browserGUI_dot_showBusy_dot_295 :: Curry_Prelude.Curry t0 => Curry_GUI.C_WidgetRef -> Func Curry_GUI.C_GuiPort (Curry_Prelude.C_IO t0) -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showBusy_dot_295 x1 x2 x3 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_gt_gt (Curry_GUI.nd_C_setConfig x1 (Curry_GUI.C_Background (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))) x3 x2000 x3500) (let
               x2001 = leftSupply x2003
               x2002 = rightSupply x2003
                in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_gt_gt (Curry_GUI.nd_C_setConfig x1 (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List)))))))))))))))) x3 x2001 x3500) (Curry_Prelude.nd_OP_gt_gt_eq (Curry_System.d_C_getCPUTime x3500) (wrapNX id (nd_OP_browserGUI_dot_showBusy_dot_295_dot___hash_lambda43 x3 x2 x1)) x2002 x3500) x3500)))) x3500)))))

d_OP_browserGUI_dot_showBusy_dot_295_dot___hash_lambda43 :: Curry_Prelude.Curry t750 => Curry_GUI.C_GuiPort -> (Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO t750) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showBusy_dot_295_dot___hash_lambda43 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply x2 x1 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_System.d_C_getCPUTime x3500) (d_OP_browserGUI_dot_showBusy_dot_295_dot___hash_lambda43_dot___hash_lambda44 x1 x3 x4) x3500) x3500

nd_OP_browserGUI_dot_showBusy_dot_295_dot___hash_lambda43 :: Curry_Prelude.Curry t750 => Curry_GUI.C_GuiPort -> Func Curry_GUI.C_GuiPort (Curry_Prelude.C_IO t750) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Int -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showBusy_dot_295_dot___hash_lambda43 x1 x2 x3 x4 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_C_apply x2 x1 x2000 x3500) (Curry_Prelude.nd_OP_gt_gt_eq (Curry_System.d_C_getCPUTime x3500) (wrapDX id (d_OP_browserGUI_dot_showBusy_dot_295_dot___hash_lambda43_dot___hash_lambda44 x1 x3 x4)) x2001 x3500) x3500)))))

d_OP_browserGUI_dot_showBusy_dot_295_dot___hash_lambda43_dot___hash_lambda44 :: Curry_GUI.C_GuiPort -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showBusy_dot_295_dot___hash_lambda43_dot___hash_lambda44 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setConfig x2 (Curry_Prelude.d_OP_dollar (acceptCs id Curry_GUI.C_Text) (d_OP__case_16 x3 x4 (d_C_showExecTime x3500) x3500) x3500) x1 x3500) (Curry_GUI.d_C_setConfig x2 (Curry_GUI.C_Background (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)))))) x1 x3500) x3500

d_OP_browserGUI_dot_showMBusy_dot_295 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_GUI.C_WidgetRef -> (Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO t0) -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1)
d_OP_browserGUI_dot_showMBusy_dot_295 x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt (d_OP_browserGUI_dot_showBusy_dot_295 x1 x2 x3 x3500) (Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500) x3500

nd_OP_browserGUI_dot_showMBusy_dot_295 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_GUI.C_WidgetRef -> Func Curry_GUI.C_GuiPort (Curry_Prelude.C_IO t0) -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1)
nd_OP_browserGUI_dot_showMBusy_dot_295 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_OP_gt_gt (nd_OP_browserGUI_dot_showBusy_dot_295 x1 x2 x3 x2000 x3500) (Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500) x3500))

d_OP_browserGUI_dot_showDoing_dot_295 :: Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showDoing_dot_295 x1 x2 x3 x3500 = Curry_GUI.d_C_setConfig x1 (Curry_GUI.C_Text (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) x3 x3500)) x2 x3500

d_OP_browserGUI_dot_safeIO_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_safeIO_dot_295 x1 x2 x3 x4 x3500 = Curry_Prelude.d_C_catch x4 (d_OP_browserGUI_dot_safeIO_dot_295_dot___hash_lambda45 x3 x1 x2) x3500

nd_OP_browserGUI_dot_safeIO_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_safeIO_dot_295 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_catch x4 (wrapNX id (nd_OP_browserGUI_dot_safeIO_dot_295_dot___hash_lambda45 x3 x1 x2)) x2000 x3500))

d_OP_browserGUI_dot_safeIO_dot_295_dot___hash_lambda45 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_IOError -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_safeIO_dot_295_dot___hash_lambda45 x1 x2 x3 x4 x3500 = d_OP_browserGUI_dot_putMainMessage_dot_295 x2 x3 x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Prelude.d_C_showError x4 x3500) x3500) x3500

nd_OP_browserGUI_dot_safeIO_dot_295_dot___hash_lambda45 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_IOError -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_safeIO_dot_295_dot___hash_lambda45 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_browserGUI_dot_putMainMessage_dot_295 x2 x3 x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Prelude.d_C_showError x4 x3500) x3500) x2000 x3500))

d_OP_browserGUI_dot_selmod_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selmod_dot_295 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_GUI.d_C_getValue x4 x6 x3500) (d_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46 x6 x1 x2 x3 x4 x5) x3500

nd_OP_browserGUI_dot_selmod_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selmod_dot_295 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_GUI.d_C_getValue x4 x6 x3500) (wrapNX id (nd_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46 x6 x1 x2 x3 x4 x5)) x2000 x3500))

d_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46 x1 x2 x3 x4 x5 x6 x7 x3500 = d_OP__case_15 x1 x2 x3 x4 x5 x6 x7 (Curry_Prelude.d_OP_eq_eq x7 Curry_Prelude.OP_List x3500) x3500

nd_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_15 x1 x2 x3 x4 x5 x6 x7 (Curry_Prelude.d_OP_eq_eq x7 Curry_Prelude.OP_List x3500) x2000 x3500))

d_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46_dot___hash_lambda47 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46_dot___hash_lambda47 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_changeTrees (Curry_Read.d_C_readNat x5 x3500) x6 x3500) (d_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46_dot___hash_lambda47_dot___hash_lambda48 x1 x2 x3 x4 x5) x3500

nd_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46_dot___hash_lambda47 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46_dot___hash_lambda47 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_changeTrees (Curry_Read.d_C_readNat x5 x3500) x6 x3500) (wrapNX id (nd_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46_dot___hash_lambda47_dot___hash_lambda48 x1 x2 x3 x4 x5)) x2000 x3500))

d_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46_dot___hash_lambda47_dot___hash_lambda48 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46_dot___hash_lambda47_dot___hash_lambda48 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_gt_gt (d_C_storeTrees x2 x6 x3500) (Curry_GUI.d_C_setConfig x4 (Curry_GUI.C_List (d_C_trees2strings x6 x3500)) x1 x3500) x3500) (Curry_GUI.d_C_setValue x3 Curry_Prelude.OP_List x1 x3500) x3500) (Curry_GUI.d_C_setValue x4 x5 x1 x3500) x3500

nd_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46_dot___hash_lambda47_dot___hash_lambda48 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46_dot___hash_lambda47_dot___hash_lambda48 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_gt_gt (d_C_storeTrees x2 x6 x3500) (Curry_GUI.nd_C_setConfig x4 (Curry_GUI.C_List (d_C_trees2strings x6 x3500)) x1 x2000 x3500) x3500) (Curry_GUI.d_C_setValue x3 Curry_Prelude.OP_List x1 x3500) x3500) (Curry_GUI.d_C_setValue x4 x5 x1 x3500) x3500))

d_OP_browserGUI_dot_getSelectedModName_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_browserGUI_dot_getSelectedModName_dot_295 x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_GUI.d_C_getValue x2 x3 x3500) (d_OP_browserGUI_dot_getSelectedModName_dot_295_dot___hash_lambda49 x1) x3500

nd_OP_browserGUI_dot_getSelectedModName_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_browserGUI_dot_getSelectedModName_dot_295 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_GUI.d_C_getValue x2 x3 x3500) (wrapNX id (nd_OP_browserGUI_dot_getSelectedModName_dot_295_dot___hash_lambda49 x1)) x2000 x3500))

d_OP_browserGUI_dot_getSelectedModName_dot_295_dot___hash_lambda49 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_browserGUI_dot_getSelectedModName_dot_295_dot___hash_lambda49 x1 x2 x3500 = d_OP__case_14 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 Curry_Prelude.OP_List x3500) x3500

nd_OP_browserGUI_dot_getSelectedModName_dot_295_dot___hash_lambda49 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_browserGUI_dot_getSelectedModName_dot_295_dot___hash_lambda49 x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_14 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 Curry_Prelude.OP_List x3500) x2000 x3500))

d_OP_browserGUI_dot_getSelectedModName_dot_295_dot___hash_lambda49_dot___hash_lambda50 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_browserGUI_dot_getSelectedModName_dot_295_dot___hash_lambda49_dot___hash_lambda50 x1 x2 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.C_Just (Curry_Prelude.d_C_fst (d_C_getTreesValue (Curry_Read.d_C_readNat x1 x3500) x2 x3500) x3500)) x3500

d_OP_browserGUI_dot_executeForModule_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_executeForModule_dot_295 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_OP_browserGUI_dot_getSelectedModName_dot_295 x1 x2 x5 x3500) (d_OP_browserGUI_dot_executeForModule_dot_295_dot___hash_lambda51 x5 x1 x4 x3) x3500

nd_OP_browserGUI_dot_executeForModule_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func Curry_GUI.C_GuiPort (Curry_Prelude.C_IO Curry_Prelude.OP_Unit)) -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_executeForModule_dot_295 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_OP_browserGUI_dot_getSelectedModName_dot_295 x1 x2 x5 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_executeForModule_dot_295_dot___hash_lambda51 x5 x1 x4 x3)) x2001 x3500)))))

d_OP_browserGUI_dot_executeForModule_dot_295_dot___hash_lambda51 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_executeForModule_dot_295_dot___hash_lambda51 x1 x2 x3 x4 x5 x3500 = d_OP__case_13 x1 x2 x3 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 Curry_Prelude.C_Nothing x3500) x3500

nd_OP_browserGUI_dot_executeForModule_dot_295_dot___hash_lambda51 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func Curry_GUI.C_GuiPort (Curry_Prelude.C_IO Curry_Prelude.OP_Unit)) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_executeForModule_dot_295_dot___hash_lambda51 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_13 x1 x2 x3 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 Curry_Prelude.C_Nothing x3500) x2000 x3500))

d_OP_browserGUI_dot_showModAnalysisResult_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AnalysisTypes.C_ModuleAnalysisResult -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showModAnalysisResult_dot_295 x1 x2 x3 x4 x5 x3500 = case x4 of
     (Curry_AnalysisTypes.C_ContentsResult x6 x7) -> Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x2 x7 x5 x3500) (d_C_setMainContentsModule x1 x3 x6 x7 x3500) x3500
     (Curry_AnalysisTypes.C_ModuleAction x8) -> x8
     (Curry_AnalysisTypes.Choice_C_ModuleAnalysisResult x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot_showModAnalysisResult_dot_295 x1 x2 x3 x1002 x5 x3500) (d_OP_browserGUI_dot_showModAnalysisResult_dot_295 x1 x2 x3 x1003 x5 x3500)
     (Curry_AnalysisTypes.Choices_C_ModuleAnalysisResult x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot_showModAnalysisResult_dot_295 x1 x2 x3 z x5 x3500) x1002
     (Curry_AnalysisTypes.Guard_C_ModuleAnalysisResult x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot_showModAnalysisResult_dot_295 x1 x2 x3 x1002 x5) $! (addCs x1001 x3500))
     (Curry_AnalysisTypes.Fail_C_ModuleAnalysisResult x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_browserGUI_dot_showModAnalysisResult_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AnalysisTypes.C_ModuleAnalysisResult -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showModAnalysisResult_dot_295 x1 x2 x3 x4 x5 x3000 x3500 = case x4 of
     (Curry_AnalysisTypes.C_ContentsResult x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x2 x7 x5 x3500) (nd_C_setMainContentsModule x1 x3 x6 x7 x2000 x3500) x3500))
     (Curry_AnalysisTypes.C_ModuleAction x8) -> x8
     (Curry_AnalysisTypes.Choice_C_ModuleAnalysisResult x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_browserGUI_dot_showModAnalysisResult_dot_295 x1 x2 x3 x1002 x5 x3000 x3500) (nd_OP_browserGUI_dot_showModAnalysisResult_dot_295 x1 x2 x3 x1003 x5 x3000 x3500)
     (Curry_AnalysisTypes.Choices_C_ModuleAnalysisResult x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_browserGUI_dot_showModAnalysisResult_dot_295 x1 x2 x3 z x5 x3000 x3500) x1002
     (Curry_AnalysisTypes.Guard_C_ModuleAnalysisResult x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_browserGUI_dot_showModAnalysisResult_dot_295 x1 x2 x3 x1002 x5 x3000) $! (addCs x1001 x3500))
     (Curry_AnalysisTypes.Fail_C_ModuleAnalysisResult x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_browserGUI_dot_performModuleAnalysis_dot_295 :: Curry_Prelude.Curry t0 => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_AnalysisTypes.C_ModuleAnalysis Curry_AnalysisTypes.C_ModuleAnalysisResult -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult
d_OP_browserGUI_dot_performModuleAnalysis_dot_295 x1 x2 x3 x4 x3500 = case x2 of
     (Curry_AnalysisTypes.C_InterfaceAnalysis x5) -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getIntWithName x1 x4 x3500) (d_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda98 x5) x3500
     (Curry_AnalysisTypes.C_FlatCurryAnalysis x6) -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getProgWithName x1 x3 x4 x3500) (d_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda99 x6) x3500
     (Curry_AnalysisTypes.C_SourceCodeAnalysis x7) -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Imports.d_C_findFileInLoadPath x4 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)) x3500) (d_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda100 x7 x4) x3500
     (Curry_AnalysisTypes.Choice_C_ModuleAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot_performModuleAnalysis_dot_295 x1 x1002 x3 x4 x3500) (d_OP_browserGUI_dot_performModuleAnalysis_dot_295 x1 x1003 x3 x4 x3500)
     (Curry_AnalysisTypes.Choices_C_ModuleAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot_performModuleAnalysis_dot_295 x1 z x3 x4 x3500) x1002
     (Curry_AnalysisTypes.Guard_C_ModuleAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot_performModuleAnalysis_dot_295 x1 x1002 x3 x4) $! (addCs x1001 x3500))
     (Curry_AnalysisTypes.Fail_C_ModuleAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_browserGUI_dot_performModuleAnalysis_dot_295 :: Curry_Prelude.Curry t0 => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_AnalysisTypes.C_ModuleAnalysis Curry_AnalysisTypes.C_ModuleAnalysisResult -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult
nd_OP_browserGUI_dot_performModuleAnalysis_dot_295 x1 x2 x3 x4 x3000 x3500 = case x2 of
     (Curry_AnalysisTypes.HO_C_InterfaceAnalysis x5) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getIntWithName x1 x4 x3500) (wrapNX id (nd_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda98 x5)) x2000 x3500))
     (Curry_AnalysisTypes.HO_C_FlatCurryAnalysis x6) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getProgWithName x1 x3 x4 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda99 x6)) x2001 x3500)))))
     (Curry_AnalysisTypes.HO_C_SourceCodeAnalysis x7) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Imports.d_C_findFileInLoadPath x4 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)) x3500) (wrapNX id (nd_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda100 x7 x4)) x2000 x3500))
     (Curry_AnalysisTypes.Choice_C_ModuleAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_browserGUI_dot_performModuleAnalysis_dot_295 x1 x1002 x3 x4 x3000 x3500) (nd_OP_browserGUI_dot_performModuleAnalysis_dot_295 x1 x1003 x3 x4 x3000 x3500)
     (Curry_AnalysisTypes.Choices_C_ModuleAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_browserGUI_dot_performModuleAnalysis_dot_295 x1 z x3 x4 x3000 x3500) x1002
     (Curry_AnalysisTypes.Guard_C_ModuleAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_browserGUI_dot_performModuleAnalysis_dot_295 x1 x1002 x3 x4 x3000) $! (addCs x1001 x3500))
     (Curry_AnalysisTypes.Fail_C_ModuleAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda98 :: (Curry_FlatCurry.C_Prog -> ConstStore -> Curry_AnalysisTypes.C_ModuleAnalysisResult) -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult
d_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda98 x1 x2 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_C_apply x1 x2 x3500) x3500

nd_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda98 :: Func Curry_FlatCurry.C_Prog Curry_AnalysisTypes.C_ModuleAnalysisResult -> Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult
nd_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda98 x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_return (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x3500))

d_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda99 :: (Curry_FlatCurry.C_Prog -> ConstStore -> Curry_AnalysisTypes.C_ModuleAnalysisResult) -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult
d_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda99 x1 x2 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_C_apply x1 x2 x3500) x3500

nd_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda99 :: Func Curry_FlatCurry.C_Prog Curry_AnalysisTypes.C_ModuleAnalysisResult -> Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult
nd_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda99 x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_return (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x3500))

d_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda100 :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult
d_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda100 x1 x2 x3 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_return (Curry_AnalysisTypes.C_ContentsResult Curry_AnalysisTypes.C_OtherText (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))) x3500) x3500)) x3500) (d_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda100_dot___hash_lambda101 x1) x3 x3500

nd_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda100 :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult
nd_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda100 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_maybe (Curry_Prelude.d_C_return (Curry_AnalysisTypes.C_ContentsResult Curry_AnalysisTypes.C_OtherText (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))) x3500) x3500)) x3500) (wrapNX id (nd_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda100_dot___hash_lambda101 x1)) x3 x2000 x3500))

d_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda100_dot___hash_lambda101 :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult
d_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda100_dot___hash_lambda101 x1 x2 x3500 = Curry_Prelude.d_C_apply x1 x2 x3500

nd_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda100_dot___hash_lambda101 :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_AnalysisTypes.C_ModuleAnalysisResult
nd_OP_browserGUI_dot_performModuleAnalysis_dot_295_dot___hash_lambda100_dot___hash_lambda101 x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500))

d_OP_browserGUI_dot_analyzeModuleWith_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_AnalysisTypes.C_ModuleAnalysis Curry_AnalysisTypes.C_ModuleAnalysisResult -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_analyzeModuleWith_dot_295 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_OP_dollar (d_OP_browserGUI_dot_safeIO_dot_295 x1 x3 x6) (Curry_Prelude.d_OP_gt_gt_eq (d_OP_browserGUI_dot_performModuleAnalysis_dot_295 x1 x4 (d_OP_browserGUI_dot_showDoing_dot_295 x2 x6) x5 x3500) (d_OP_browserGUI_dot_analyzeModuleWith_dot_295_dot___hash_lambda52 x6 x1 x5 x3) x3500) x3500

nd_OP_browserGUI_dot_analyzeModuleWith_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_AnalysisTypes.C_ModuleAnalysis Curry_AnalysisTypes.C_ModuleAnalysisResult -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_analyzeModuleWith_dot_295 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_OP_browserGUI_dot_safeIO_dot_295 x1 x3 x6)) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_OP_browserGUI_dot_performModuleAnalysis_dot_295 x1 x4 (wrapDX id (d_OP_browserGUI_dot_showDoing_dot_295 x2 x6)) x5 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_analyzeModuleWith_dot_295_dot___hash_lambda52 x6 x1 x5 x3)) x2001 x3500)))) x2003 x3500)))))

d_OP_browserGUI_dot_analyzeModuleWith_dot_295_dot___hash_lambda52 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_AnalysisTypes.C_ModuleAnalysisResult -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_analyzeModuleWith_dot_295_dot___hash_lambda52 x1 x2 x3 x4 x5 x3500 = d_OP_browserGUI_dot_showModAnalysisResult_dot_295 x2 x4 x3 x5 x1 x3500

nd_OP_browserGUI_dot_analyzeModuleWith_dot_295_dot___hash_lambda52 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_AnalysisTypes.C_ModuleAnalysisResult -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_analyzeModuleWith_dot_295_dot___hash_lambda52 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_browserGUI_dot_showModAnalysisResult_dot_295 x2 x4 x3 x5 x1 x2000 x3500))

d_OP_browserGUI_dot_showSource_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showSource_dot_295 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getModuleFileName x1 x3 x3500) (d_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53 x4 x1 x3 x2) x3500

nd_OP_browserGUI_dot_showSource_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showSource_dot_295 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getModuleFileName x1 x3 x3500) (wrapNX id (nd_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53 x4 x1 x3 x2)) x2000 x3500))

d_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Imports.d_C_findFileInLoadPath x5 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)) x3500) (d_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54 x1 x2 x3 x4) x3500

nd_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Imports.d_C_findFileInLoadPath x5 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)) x3500) (wrapNX id (nd_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54 x1 x2 x3 x4)) x2000 x3500))

d_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_C_maybe (d_OP_browserGUI_dot_putMainMessage_dot_295 x2 x4 x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))))))))))))))))))))))))))) x3500) (d_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54_dot___hash_lambda55 x1 x2 x3 x4) x5 x3500

nd_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_maybe (nd_OP_browserGUI_dot_putMainMessage_dot_295 x2 x4 x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))))))))))))))))))))))))))) x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54_dot___hash_lambda55 x1 x2 x3 x4)) x5 x2001 x3500)))))

d_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54_dot___hash_lambda55 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54_dot___hash_lambda55 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x5 x3500) (d_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54_dot___hash_lambda55_dot___hash_lambda56 x5 x1 x2 x3 x4) x3500

nd_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54_dot___hash_lambda55 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54_dot___hash_lambda55 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x5 x3500) (wrapNX id (nd_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54_dot___hash_lambda55_dot___hash_lambda56 x5 x1 x2 x3 x4)) x2000 x3500))

d_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54_dot___hash_lambda55_dot___hash_lambda56 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54_dot___hash_lambda55_dot___hash_lambda56 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x5 x6 x2 x3500) (d_C_setMainContentsModule x3 x4 (d_OP__case_12 x1 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 7#) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) x1 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List))))))) x3500) x3500) x6 x3500) x3500

nd_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54_dot___hash_lambda55_dot___hash_lambda56 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showSource_dot_295_dot___hash_lambda53_dot___hash_lambda54_dot___hash_lambda55_dot___hash_lambda56 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x5 x6 x2 x3500) (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (nd_C_setMainContentsModule x3 x4 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_12 x1 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 7#) (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2000 x3500) x1 x2001 x3500)))) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List))))))) x3500) x2003 x3500)))) x6 x2005 x3500)))) x3500))

d_OP_browserGUI_dot_getFileInfo_dot_295 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_browserGUI_dot_getFileInfo_dot_295 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))))))))) x3500
     (Curry_Prelude.C_Just x3) -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_fileSize x3 x3500) (d_OP_browserGUI_dot_getFileInfo_dot_295_dot___hash_lambda62 x1 x3) x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot_getFileInfo_dot_295 x1 x1002 x3500) (d_OP_browserGUI_dot_getFileInfo_dot_295 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot_getFileInfo_dot_295 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot_getFileInfo_dot_295 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_browserGUI_dot_getFileInfo_dot_295_dot___hash_lambda62 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_browserGUI_dot_getFileInfo_dot_295_dot___hash_lambda62 x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x2 x3500) (d_OP_browserGUI_dot_getFileInfo_dot_295_dot___hash_lambda62_dot___hash_lambda63 x1 x2 x3) x3500

d_OP_browserGUI_dot_getFileInfo_dot_295_dot___hash_lambda62_dot___hash_lambda63 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Time.C_ClockTime -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_browserGUI_dot_getFileInfo_dot_295_dot___hash_lambda62_dot___hash_lambda63 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Time.d_C_toCalendarTime x4 x3500) (d_OP_browserGUI_dot_getFileInfo_dot_295_dot___hash_lambda62_dot___hash_lambda63_dot___hash_lambda64 x1 x2 x3) x3500

d_OP_browserGUI_dot_getFileInfo_dot_295_dot___hash_lambda62_dot___hash_lambda63_dot___hash_lambda64 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Time.C_CalendarTime -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_browserGUI_dot_getFileInfo_dot_295_dot___hash_lambda62_dot___hash_lambda63_dot___hash_lambda64 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_take x1 (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ' '#) x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Time.d_C_calendarTimeToString x4 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x3 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))))))) x3500) x3500) x3500) x3500) x3500) x3500) x3500

d_OP_browserGUI_dot_showModuleInfo_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showModuleInfo_dot_295 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getModuleFileName x1 x3 x3500) (d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57 x4 x1 x2) x3500

nd_OP_browserGUI_dot_showModuleInfo_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showModuleInfo_dot_295 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getModuleFileName x1 x3 x3500) (wrapNX id (nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57 x4 x1 x2)) x2000 x3500))

d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Imports.d_C_findFileInLoadPath x4 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)) x3500) (d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58 x4 x1 x2 x3) x3500

nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Imports.d_C_findFileInLoadPath x4 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)) x3500) (wrapNX id (nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58 x4 x1 x2 x3)) x2000 x3500))

d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Imports.d_C_findFileInLoadPath x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List) x3500) (d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59 x2 x3 x5 x4) x3500

nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Imports.d_C_findFileInLoadPath x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List) x3500) (wrapNX id (nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59 x2 x3 x5 x4)) x2000 x3500))

d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_OP_browserGUI_dot_getFileInfo_dot_295 (Curry_Prelude.C_Int 2#) x3 x3500) (d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59_dot___hash_lambda60 x1 x2 x5 x4) x3500

nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_OP_browserGUI_dot_getFileInfo_dot_295 (Curry_Prelude.C_Int 2#) x3 x3500) (wrapNX id (nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59_dot___hash_lambda60 x1 x2 x5 x4)) x2000 x3500))

d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59_dot___hash_lambda60 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59_dot___hash_lambda60 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_OP_browserGUI_dot_getFileInfo_dot_295 (Curry_Prelude.C_Int 4#) x3 x3500) (d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59_dot___hash_lambda60_dot___hash_lambda61 x1 x2 x4 x5) x3500

nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59_dot___hash_lambda60 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59_dot___hash_lambda60 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_OP_browserGUI_dot_getFileInfo_dot_295 (Curry_Prelude.C_Int 4#) x3 x3500) (wrapNX id (nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59_dot___hash_lambda60_dot___hash_lambda61 x1 x2 x4 x5)) x2000 x3500))

d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59_dot___hash_lambda60_dot___hash_lambda61 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59_dot___hash_lambda60_dot___hash_lambda61 x1 x2 x3 x4 x5 x3500 = let
     x6 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))) (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) x5 x3500) x3500) x3500
      in (d_OP_browserGUI_dot_putMainMessage_dot_295 x2 x3 x1 x6 x3500)

nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59_dot___hash_lambda60_dot___hash_lambda61 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showModuleInfo_dot_295_dot___hash_lambda57_dot___hash_lambda58_dot___hash_lambda59_dot___hash_lambda60_dot___hash_lambda61 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x6 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))) (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) x5 x3500) x3500) x3500
           in (nd_OP_browserGUI_dot_putMainMessage_dot_295 x2 x3 x1 x6 x2000 x3500)))

d_OP_browserGUI_dot_showImportGraph_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showImportGraph_dot_295 x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllModules x1 x3500) (d_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65 x3 x1 x2) x3500

nd_OP_browserGUI_dot_showImportGraph_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showImportGraph_dot_295 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getAllModules x1 x3500) (wrapNX id (nd_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65 x3 x1 x2)) x2000 x3500))

d_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_dollar (d_OP_browserGUI_dot_safeIO_dot_295 x2 x3 x1) (Curry_ShowGraph.d_C_viewDependencyGraph (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65_dot___hash_lambda66 x3500) x4 x3500) x3500) x3500

nd_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65 x1 x2 x3 x4 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_OP_browserGUI_dot_safeIO_dot_295 x2 x3 x1)) (Curry_ShowGraph.d_C_viewDependencyGraph (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id d_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65_dot___hash_lambda66) x2000 x3500) x4 x2001 x3500)))) x3500) x2003 x3500)))))

d_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65_dot___hash_lambda66 :: Curry_Prelude.Curry t1663 => Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List t1663) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65_dot___hash_lambda66 x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> d_OP__case_11 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x3500) x3500
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65_dot___hash_lambda66 x1002 x3500) (d_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65_dot___hash_lambda66 x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65_dot___hash_lambda66 z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot_showImportGraph_dot_295_dot___hash_lambda65_dot___hash_lambda66 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_browserGUI_dot_showImpCalls_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showImpCalls_dot_295 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getProgWithName x1 (d_OP_browserGUI_dot_showDoing_dot_295 x2 x5) x4 x3500) (d_OP_browserGUI_dot_showImpCalls_dot_295_dot___hash_lambda67 x5 x1 x3) x3500

nd_OP_browserGUI_dot_showImpCalls_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showImpCalls_dot_295 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getProgWithName x1 (wrapDX id (d_OP_browserGUI_dot_showDoing_dot_295 x2 x5)) x4 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_showImpCalls_dot_295_dot___hash_lambda67 x5 x1 x3)) x2001 x3500)))))

d_OP_browserGUI_dot_showImpCalls_dot_295_dot___hash_lambda67 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showImpCalls_dot_295_dot___hash_lambda67 x1 x2 x3 x4 x3500 = d_OP_browserGUI_dot_putMainMessage_dot_295 x2 x3 x1 (Curry_Prelude.d_C_apply (Curry_ImportCalls.d_C_showImportCalls x3500) x4 x3500) x3500

nd_OP_browserGUI_dot_showImpCalls_dot_295_dot___hash_lambda67 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showImpCalls_dot_295_dot___hash_lambda67 x1 x2 x3 x4 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (nd_OP_browserGUI_dot_putMainMessage_dot_295 x2 x3 x1 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_ImportCalls.nd_C_showImportCalls x2000 x3500) x4 x2001 x3500)))) x2003 x3500)))))

d_OP_browserGUI_dot_showAllModuleFuns_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showAllModuleFuns_dot_295 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getProgWithName x1 (d_OP_browserGUI_dot_showDoing_dot_295 x3 x5) x4 x3500) (d_OP_browserGUI_dot_showAllModuleFuns_dot_295_dot___hash_lambda68 x5 x1 x2) x3500

nd_OP_browserGUI_dot_showAllModuleFuns_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showAllModuleFuns_dot_295 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getProgWithName x1 (wrapDX id (d_OP_browserGUI_dot_showDoing_dot_295 x3 x5)) x4 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_showAllModuleFuns_dot_295_dot___hash_lambda68 x5 x1 x2)) x2001 x3500)))))

d_OP_browserGUI_dot_showAllModuleFuns_dot_295_dot___hash_lambda68 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showAllModuleFuns_dot_295_dot___hash_lambda68 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_storeSelectedFunctions x2 (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_progFuncs x3500) x4 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (d_C_setFunctionListKind x2 Curry_Prelude.C_True x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_getFuns x2 x3500) (d_OP_browserGUI_dot_showAllModuleFuns_dot_295_dot___hash_lambda68_dot___hash_lambda69 x1 x3) x3500) x3500) x3500

nd_OP_browserGUI_dot_showAllModuleFuns_dot_295_dot___hash_lambda68 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showAllModuleFuns_dot_295_dot___hash_lambda68 x1 x2 x3 x4 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2002 = leftSupply x2006
          x2005 = rightSupply x2006
           in (seq x2002 (seq x2005 (Curry_Prelude.d_OP_gt_gt (d_C_storeSelectedFunctions x2 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_progFuncs x2000 x3500) x4 x2001 x3500)))) x3500) (let
               x2003 = leftSupply x2005
               x2004 = rightSupply x2005
                in (seq x2003 (seq x2004 (Curry_Prelude.d_OP_gt_gt (nd_C_setFunctionListKind x2 Curry_Prelude.C_True x2003 x3500) (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getFuns x2 x3500) (wrapDX id (d_OP_browserGUI_dot_showAllModuleFuns_dot_295_dot___hash_lambda68_dot___hash_lambda69 x1 x3)) x2004 x3500) x3500)))) x3500)))))

d_OP_browserGUI_dot_showAllModuleFuns_dot_295_dot___hash_lambda68_dot___hash_lambda69 :: Curry_GUI.C_GuiPort -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showAllModuleFuns_dot_295_dot___hash_lambda68_dot___hash_lambda69 x1 x2 x3 x3500 = Curry_GUI.d_C_setConfig x2 (Curry_GUI.C_List (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_snd (Curry_FlatCurryGoodies.d_C_funcName x3500) x3500) x3 x3500)) x1 x3500

d_OP_browserGUI_dot_showExportedFuns_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showExportedFuns_dot_295 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getProgWithName x1 (d_OP_browserGUI_dot_showDoing_dot_295 x3 x5) x4 x3500) (d_OP_browserGUI_dot_showExportedFuns_dot_295_dot___hash_lambda70 x5 x1 x2) x3500

nd_OP_browserGUI_dot_showExportedFuns_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showExportedFuns_dot_295 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getProgWithName x1 (wrapDX id (d_OP_browserGUI_dot_showDoing_dot_295 x3 x5)) x4 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_showExportedFuns_dot_295_dot___hash_lambda70 x5 x1 x2)) x2001 x3500)))))

d_OP_browserGUI_dot_showExportedFuns_dot_295_dot___hash_lambda70 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showExportedFuns_dot_295_dot___hash_lambda70 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_storeSelectedFunctions x2 (Curry_Prelude.d_C_filter d_C_isPublic (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_progFuncs x3500) x4 x3500) x3500) x3500) (Curry_Prelude.d_OP_gt_gt (d_C_setFunctionListKind x2 Curry_Prelude.C_True x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_getFuns x2 x3500) (d_OP_browserGUI_dot_showExportedFuns_dot_295_dot___hash_lambda70_dot___hash_lambda71 x1 x3) x3500) x3500) x3500

nd_OP_browserGUI_dot_showExportedFuns_dot_295_dot___hash_lambda70 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showExportedFuns_dot_295_dot___hash_lambda70 x1 x2 x3 x4 x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2004 = leftSupply x2008
          x2007 = rightSupply x2008
           in (seq x2004 (seq x2007 (Curry_Prelude.d_OP_gt_gt (d_C_storeSelectedFunctions x2 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_filter (wrapDX id d_C_isPublic) (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_progFuncs x2000 x3500) x4 x2001 x3500)))) x2003 x3500)))) x3500) (let
               x2005 = leftSupply x2007
               x2006 = rightSupply x2007
                in (seq x2005 (seq x2006 (Curry_Prelude.d_OP_gt_gt (nd_C_setFunctionListKind x2 Curry_Prelude.C_True x2005 x3500) (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getFuns x2 x3500) (wrapDX id (d_OP_browserGUI_dot_showExportedFuns_dot_295_dot___hash_lambda70_dot___hash_lambda71 x1 x3)) x2006 x3500) x3500)))) x3500)))))

d_OP_browserGUI_dot_showExportedFuns_dot_295_dot___hash_lambda70_dot___hash_lambda71 :: Curry_GUI.C_GuiPort -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showExportedFuns_dot_295_dot___hash_lambda70_dot___hash_lambda71 x1 x2 x3 x3500 = Curry_GUI.d_C_setConfig x2 (Curry_GUI.C_List (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_snd (Curry_FlatCurryGoodies.d_C_funcName x3500) x3500) x3 x3500)) x1 x3500

d_OP_browserGUI_dot_showAllExportedFuns_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showAllExportedFuns_dot_295 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllFunctions x1 (d_OP_browserGUI_dot_showDoing_dot_295 x3 x5) x4 x3500) (d_OP_browserGUI_dot_showAllExportedFuns_dot_295_dot___hash_lambda72 x5 x1 x2) x3500

nd_OP_browserGUI_dot_showAllExportedFuns_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showAllExportedFuns_dot_295 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getAllFunctions x1 (wrapDX id (d_OP_browserGUI_dot_showDoing_dot_295 x3 x5)) x4 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_showAllExportedFuns_dot_295_dot___hash_lambda72 x5 x1 x2)) x2001 x3500)))))

d_OP_browserGUI_dot_showAllExportedFuns_dot_295_dot___hash_lambda72 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showAllExportedFuns_dot_295_dot___hash_lambda72 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_storeSelectedFunctions x2 (Curry_Prelude.d_C_filter d_C_isPublic x4 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (d_C_setFunctionListKind x2 Curry_Prelude.C_False x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_getFuns x2 x3500) (d_OP_browserGUI_dot_showAllExportedFuns_dot_295_dot___hash_lambda72_dot___hash_lambda73 x1 x3) x3500) x3500) x3500

nd_OP_browserGUI_dot_showAllExportedFuns_dot_295_dot___hash_lambda72 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showAllExportedFuns_dot_295_dot___hash_lambda72 x1 x2 x3 x4 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_gt_gt (d_C_storeSelectedFunctions x2 (Curry_Prelude.nd_C_filter (wrapDX id d_C_isPublic) x4 x2000 x3500) x3500) (let
               x2001 = leftSupply x2003
               x2002 = rightSupply x2003
                in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_gt_gt (nd_C_setFunctionListKind x2 Curry_Prelude.C_False x2001 x3500) (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getFuns x2 x3500) (wrapDX id (d_OP_browserGUI_dot_showAllExportedFuns_dot_295_dot___hash_lambda72_dot___hash_lambda73 x1 x3)) x2002 x3500) x3500)))) x3500)))))

d_OP_browserGUI_dot_showAllExportedFuns_dot_295_dot___hash_lambda72_dot___hash_lambda73 :: Curry_GUI.C_GuiPort -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showAllExportedFuns_dot_295_dot___hash_lambda72_dot___hash_lambda73 x1 x2 x3 x3500 = Curry_GUI.d_C_setConfig x2 (Curry_GUI.C_List (Curry_Prelude.d_C_map d_C_showQNameWithMod (Curry_Prelude.d_C_map (Curry_FlatCurryGoodies.d_C_funcName x3500) x3 x3500) x3500)) x1 x3500

d_OP_browserGUI_dot_selectDirectCalls_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selectDirectCalls_dot_295 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_OP_browserGUI_dot_getSelectedModName_dot_295 x1 x3 x5 x3500) (d_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74 x5 x1 x2 x4) x3500

nd_OP_browserGUI_dot_selectDirectCalls_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selectDirectCalls_dot_295 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_OP_browserGUI_dot_getSelectedModName_dot_295 x1 x3 x5 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74 x5 x1 x2 x4)) x2001 x3500)))))

d_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_GUI.d_C_getValue x3 x1 x3500) (d_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75 x1 x2 x5 x3 x4) x3500

nd_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_GUI.d_C_getValue x3 x1 x3500) (wrapNX id (nd_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75 x1 x2 x5 x3 x4)) x2000 x3500))

d_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75 x1 x2 x3 x4 x5 x6 x3500 = d_OP__case_10 x1 x2 x3 x4 x5 x6 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 Curry_Prelude.C_Nothing x3500) (Curry_Prelude.d_C_null x6 x3500) x3500) x3500

nd_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_10 x1 x2 x3 x4 x5 x6 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 Curry_Prelude.C_Nothing x3500) (Curry_Prelude.d_C_null x6 x3500) x3500) x2000 x3500))

d_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75_dot___hash_lambda76 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75_dot___hash_lambda76 x1 x2 x3 x4 x5 x6 x7 x3500 = let
     x8 = Curry_Prelude.d_OP_bang_bang x7 (Curry_Read.d_C_readNat x6 x3500) x3500
     x9 = Curry_Sort.d_C_mergeSort (acceptCs id d_C_leqQName) (Curry_List.d_C_union (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3500) x8 x3500) Curry_Prelude.OP_List) (Curry_Dependency.d_C_callsDirectly x8 x3500) x3500) x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllFunctions x2 (d_OP_browserGUI_dot_showDoing_dot_295 x5 x1) (Curry_Maybe.d_C_fromJust x3 x3500) x3500) (d_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75_dot___hash_lambda76_dot___hash_lambda77 x1 x2 x9 x4) x3500)

nd_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75_dot___hash_lambda76 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75_dot___hash_lambda76 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2004 = leftSupply x2008
          x2007 = rightSupply x2008
           in (seq x2004 (seq x2007 (let
               x8 = Curry_Prelude.d_OP_bang_bang x7 (Curry_Read.d_C_readNat x6 x3500) x3500
               x9 = let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Sort.nd_C_mergeSort (wrapDX (wrapDX id) (acceptCs id d_C_leqQName)) (Curry_List.d_C_union (Curry_Prelude.OP_Cons (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_funcName x2000 x3500) x8 x2001 x3500)))) Curry_Prelude.OP_List) (Curry_Dependency.d_C_callsDirectly x8 x3500) x3500) x2003 x3500)))
                in (let
                    x2006 = leftSupply x2007
                    x2005 = rightSupply x2007
                     in (seq x2006 (seq x2005 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getAllFunctions x2 (wrapDX id (d_OP_browserGUI_dot_showDoing_dot_295 x5 x1)) (Curry_Maybe.d_C_fromJust x3 x3500) x2005 x3500) (wrapNX id (nd_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75_dot___hash_lambda76_dot___hash_lambda77 x1 x2 x9 x4)) x2006 x3500)))))))))

d_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75_dot___hash_lambda76_dot___hash_lambda77 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75_dot___hash_lambda76_dot___hash_lambda77 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_gt_gt (d_C_storeSelectedFunctions x2 (Curry_Prelude.d_C_map (d_C_findDecl4name x5) x3 x3500) x3500) (d_C_setFunctionListKind x2 Curry_Prelude.C_False x3500) x3500) (Curry_GUI.d_C_setConfig x4 (Curry_GUI.C_List (Curry_Prelude.d_C_map d_C_showQNameWithMod x3 x3500)) x1 x3500) x3500

nd_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75_dot___hash_lambda76_dot___hash_lambda77 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75_dot___hash_lambda76_dot___hash_lambda77 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2002 = leftSupply x2006
          x2005 = rightSupply x2006
           in (seq x2002 (seq x2005 (Curry_Prelude.d_OP_gt_gt (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_gt_gt (d_C_storeSelectedFunctions x2 (Curry_Prelude.nd_C_map (wrapDX id (d_C_findDecl4name x5)) x3 x2000 x3500) x3500) (nd_C_setFunctionListKind x2 Curry_Prelude.C_False x2001 x3500) x3500)))) (let
               x2004 = leftSupply x2005
               x2003 = rightSupply x2005
                in (seq x2004 (seq x2003 (Curry_GUI.nd_C_setConfig x4 (Curry_GUI.C_List (Curry_Prelude.nd_C_map (wrapDX id d_C_showQNameWithMod) x3 x2003 x3500)) x1 x2004 x3500)))) x3500)))))

d_OP_browserGUI_dot_selectInDirectCalls_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selectInDirectCalls_dot_295 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_OP_browserGUI_dot_getSelectedModName_dot_295 x1 x3 x5 x3500) (d_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78 x5 x1 x2 x4) x3500

nd_OP_browserGUI_dot_selectInDirectCalls_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selectInDirectCalls_dot_295 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_OP_browserGUI_dot_getSelectedModName_dot_295 x1 x3 x5 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78 x5 x1 x2 x4)) x2001 x3500)))))

d_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_GUI.d_C_getValue x3 x1 x3500) (d_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79 x1 x2 x5 x3 x4) x3500

nd_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_GUI.d_C_getValue x3 x1 x3500) (wrapNX id (nd_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79 x1 x2 x5 x3 x4)) x2000 x3500))

d_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79 x1 x2 x3 x4 x5 x6 x3500 = d_OP__case_9 x1 x2 x3 x4 x5 x6 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 Curry_Prelude.C_Nothing x3500) (Curry_Prelude.d_C_null x6 x3500) x3500) x3500

nd_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_9 x1 x2 x3 x4 x5 x6 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 Curry_Prelude.C_Nothing x3500) (Curry_Prelude.d_C_null x6 x3500) x3500) x2000 x3500))

d_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79_dot___hash_lambda80 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79_dot___hash_lambda80 x1 x2 x3 x4 x5 x6 x7 x3500 = let
     x8 = Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3500) (Curry_Prelude.d_OP_bang_bang x7 (Curry_Read.d_C_readNat x6 x3500) x3500) x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllFunctions x2 (d_OP_browserGUI_dot_showDoing_dot_295 x5 x1) (Curry_Maybe.d_C_fromJust x3 x3500) x3500) (d_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79_dot___hash_lambda80_dot___hash_lambda81 x1 x2 x8 x4) x3500)

nd_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79_dot___hash_lambda80 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79_dot___hash_lambda80 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2002 = leftSupply x2006
          x2005 = rightSupply x2006
           in (seq x2002 (seq x2005 (let
               x8 = let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_funcName x2000 x3500) (Curry_Prelude.d_OP_bang_bang x7 (Curry_Read.d_C_readNat x6 x3500) x3500) x2001 x3500)))
                in (let
                    x2004 = leftSupply x2005
                    x2003 = rightSupply x2005
                     in (seq x2004 (seq x2003 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getAllFunctions x2 (wrapDX id (d_OP_browserGUI_dot_showDoing_dot_295 x5 x1)) (Curry_Maybe.d_C_fromJust x3 x3500) x2003 x3500) (wrapNX id (nd_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79_dot___hash_lambda80_dot___hash_lambda81 x1 x2 x8 x4)) x2004 x3500)))))))))

d_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79_dot___hash_lambda80_dot___hash_lambda81 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79_dot___hash_lambda80_dot___hash_lambda81 x1 x2 x3 x4 x5 x3500 = let
     x6 = Curry_Sort.d_C_mergeSort (acceptCs id d_C_leqQName) (Curry_List.d_C_union (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x3 (Curry_Dependency.d_C_indirectlyDependent x5 x3500) x3500) x3500) x3500) x3500
      in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_gt_gt (d_C_storeSelectedFunctions x2 (Curry_Prelude.d_C_map (d_C_findDecl4name x5) x6 x3500) x3500) (d_C_setFunctionListKind x2 Curry_Prelude.C_False x3500) x3500) (Curry_GUI.d_C_setConfig x4 (Curry_GUI.C_List (Curry_Prelude.d_C_map d_C_showQNameWithMod x6 x3500)) x1 x3500) x3500)

nd_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79_dot___hash_lambda80_dot___hash_lambda81 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79_dot___hash_lambda80_dot___hash_lambda81 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2000 = leftSupply x2008
          x2007 = rightSupply x2008
           in (seq x2000 (seq x2007 (let
               x6 = Curry_Sort.nd_C_mergeSort (wrapDX (wrapDX id) (acceptCs id d_C_leqQName)) (Curry_List.d_C_union (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x3 (Curry_Dependency.d_C_indirectlyDependent x5 x3500) x3500) x3500) x3500) x2000 x3500
                in (let
                    x2003 = leftSupply x2007
                    x2006 = rightSupply x2007
                     in (seq x2003 (seq x2006 (Curry_Prelude.d_OP_gt_gt (let
                         x2001 = leftSupply x2003
                         x2002 = rightSupply x2003
                          in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_gt_gt (d_C_storeSelectedFunctions x2 (Curry_Prelude.nd_C_map (wrapDX id (d_C_findDecl4name x5)) x6 x2001 x3500) x3500) (nd_C_setFunctionListKind x2 Curry_Prelude.C_False x2002 x3500) x3500)))) (let
                         x2005 = leftSupply x2006
                         x2004 = rightSupply x2006
                          in (seq x2005 (seq x2004 (Curry_GUI.nd_C_setConfig x4 (Curry_GUI.C_List (Curry_Prelude.nd_C_map (wrapDX id d_C_showQNameWithMod) x6 x2004 x3500)) x1 x2005 x3500)))) x3500)))))))))

d_OP_browserGUI_dot_showAnalysisResult_dot_295 :: Curry_GUI.C_WidgetRef -> Curry_AnalysisTypes.C_AnalysisResult -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showAnalysisResult_dot_295 x1 x2 x3 x3500 = case x2 of
     (Curry_AnalysisTypes.C_MsgResult x4) -> Curry_GUI.d_C_setValue x1 x4 x3 x3500
     (Curry_AnalysisTypes.C_ActionResult x5) -> x5
     (Curry_AnalysisTypes.Choice_C_AnalysisResult x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot_showAnalysisResult_dot_295 x1 x1002 x3 x3500) (d_OP_browserGUI_dot_showAnalysisResult_dot_295 x1 x1003 x3 x3500)
     (Curry_AnalysisTypes.Choices_C_AnalysisResult x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot_showAnalysisResult_dot_295 x1 z x3 x3500) x1002
     (Curry_AnalysisTypes.Guard_C_AnalysisResult x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot_showAnalysisResult_dot_295 x1 x1002 x3) $! (addCs x1001 x3500))
     (Curry_AnalysisTypes.Fail_C_AnalysisResult x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_browserGUI_dot_performAnalysis_dot_295 :: Curry_Prelude.Curry t0 => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_AnalysisTypes.C_FunctionAnalysis t0 -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_browserGUI_dot_performAnalysis_dot_295 x1 x2 x3 x4 x3500 = case x2 of
     (Curry_AnalysisTypes.C_LocalAnalysis x5) -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x3500) (Curry_Prelude.d_C_return (Curry_Prelude.d_C_apply x5 x4 x3500) x3500) x3500
     (Curry_AnalysisTypes.C_LocalDataAnalysis x6) -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllTypes x1 x3 (Curry_ShowFlatCurry.d_C_funcModule x4 x3500) x3500) (d_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda102 x6 x4 x3) x3500
     (Curry_AnalysisTypes.C_GlobalAnalysis x7) -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllFunctions x1 x3 (Curry_ShowFlatCurry.d_C_funcModule x4 x3500) x3500) (d_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda103 x7 x4 x3) x3500
     (Curry_AnalysisTypes.C_GlobalDataAnalysis x8) -> let
          x9 = Curry_ShowFlatCurry.d_C_funcModule x4 x3500
           in (Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllTypes x1 x3 x9 x3500) (d_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda104 x8 x4 x1 x9 x3) x3500)
     (Curry_AnalysisTypes.Choice_C_FunctionAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot_performAnalysis_dot_295 x1 x1002 x3 x4 x3500) (d_OP_browserGUI_dot_performAnalysis_dot_295 x1 x1003 x3 x4 x3500)
     (Curry_AnalysisTypes.Choices_C_FunctionAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot_performAnalysis_dot_295 x1 z x3 x4 x3500) x1002
     (Curry_AnalysisTypes.Guard_C_FunctionAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot_performAnalysis_dot_295 x1 x1002 x3 x4) $! (addCs x1001 x3500))
     (Curry_AnalysisTypes.Fail_C_FunctionAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_browserGUI_dot_performAnalysis_dot_295 :: Curry_Prelude.Curry t0 => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_AnalysisTypes.C_FunctionAnalysis t0 -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO t0
nd_OP_browserGUI_dot_performAnalysis_dot_295 x1 x2 x3 x4 x3000 x3500 = case x2 of
     (Curry_AnalysisTypes.HO_C_LocalAnalysis x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x2000 x3500) (Curry_Prelude.d_C_return (Curry_Prelude.nd_C_apply x5 x4 x2001 x3500) x3500) x3500)))))
     (Curry_AnalysisTypes.HO_C_LocalDataAnalysis x6) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getAllTypes x1 x3 (Curry_ShowFlatCurry.d_C_funcModule x4 x3500) x3500) (wrapNX id (nd_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda102 x6 x4 x3)) x2000 x3500))
     (Curry_AnalysisTypes.HO_C_GlobalAnalysis x7) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getAllFunctions x1 x3 (Curry_ShowFlatCurry.d_C_funcModule x4 x3500) x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda103 x7 x4 x3)) x2001 x3500)))))
     (Curry_AnalysisTypes.HO_C_GlobalDataAnalysis x8) -> let
          x2000 = x3000
           in (seq x2000 (let
               x9 = Curry_ShowFlatCurry.d_C_funcModule x4 x3500
                in (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getAllTypes x1 x3 x9 x3500) (wrapNX id (nd_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda104 x8 x4 x1 x9 x3)) x2000 x3500)))
     (Curry_AnalysisTypes.Choice_C_FunctionAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_browserGUI_dot_performAnalysis_dot_295 x1 x1002 x3 x4 x3000 x3500) (nd_OP_browserGUI_dot_performAnalysis_dot_295 x1 x1003 x3 x4 x3000 x3500)
     (Curry_AnalysisTypes.Choices_C_FunctionAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_browserGUI_dot_performAnalysis_dot_295 x1 z x3 x4 x3000 x3500) x1002
     (Curry_AnalysisTypes.Guard_C_FunctionAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_browserGUI_dot_performAnalysis_dot_295 x1 x1002 x3 x4 x3000) $! (addCs x1001 x3500))
     (Curry_AnalysisTypes.Fail_C_FunctionAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda102 :: Curry_Prelude.Curry t1242 => (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_FlatCurry.C_FuncDecl -> ConstStore -> t1242) -> Curry_FlatCurry.C_FuncDecl -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.C_IO t1242
d_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda102 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x3500) (Curry_Prelude.d_C_return (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x4 x3500) x2 x3500) x3500) x3500

nd_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda102 :: Curry_Prelude.Curry t1242 => Func (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Func Curry_FlatCurry.C_FuncDecl t1242) -> Curry_FlatCurry.C_FuncDecl -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO t1242
nd_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda102 x1 x2 x3 x4 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x2000 x3500) (Curry_Prelude.d_C_return (let
               x2002 = leftSupply x2003
               x2001 = rightSupply x2003
                in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x4 x2001 x3500) x2 x2002 x3500)))) x3500) x3500)))))

d_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda103 :: Curry_Prelude.Curry t1242 => (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1242)) -> Curry_FlatCurry.C_FuncDecl -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO t1242
d_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda103 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x3500) (Curry_Prelude.d_C_return (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3500) x2 x3500) (Curry_Prelude.d_C_apply x1 x4 x3500) x3500) x3500) x3500) x3500

nd_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda103 :: Curry_Prelude.Curry t1242 => Func (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1242)) -> Curry_FlatCurry.C_FuncDecl -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO t1242
nd_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda103 x1 x2 x3 x4 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2000 = leftSupply x2006
          x2005 = rightSupply x2006
           in (seq x2000 (seq x2005 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x2000 x3500) (Curry_Prelude.d_C_return (Curry_Maybe.d_C_fromJust (let
               x2003 = leftSupply x2005
               x2004 = rightSupply x2005
                in (seq x2003 (seq x2004 (Curry_Prelude.d_C_lookup (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_funcName x2001 x3500) x2 x2002 x3500)))) (Curry_Prelude.nd_C_apply x1 x4 x2004 x3500) x3500)))) x3500) x3500) x3500)))))

d_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda104 :: Curry_Prelude.Curry t1242 => (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1242)) -> Curry_FlatCurry.C_FuncDecl -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.C_IO t1242
d_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda104 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllFunctions x3 x5 x4 x3500) (d_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda104_dot___hash_lambda105 x1 x2 x5 x6) x3500

nd_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda104 :: Curry_Prelude.Curry t1242 => Func (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Func (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1242))) -> Curry_FlatCurry.C_FuncDecl -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO t1242
nd_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda104 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getAllFunctions x3 x5 x4 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda104_dot___hash_lambda105 x1 x2 x5 x6)) x2001 x3500)))))

d_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda104_dot___hash_lambda105 :: Curry_Prelude.Curry t1242 => (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1242)) -> Curry_FlatCurry.C_FuncDecl -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO t1242
d_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda104_dot___hash_lambda105 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x3500) (Curry_Prelude.d_C_return (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3500) x2 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x4 x3500) x5 x3500) x3500) x3500) x3500) x3500

nd_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda104_dot___hash_lambda105 :: Curry_Prelude.Curry t1242 => Func (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Func (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1242))) -> Curry_FlatCurry.C_FuncDecl -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO t1242
nd_OP_browserGUI_dot_performAnalysis_dot_295_dot___hash_lambda104_dot___hash_lambda105 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2000 = leftSupply x2008
          x2007 = rightSupply x2008
           in (seq x2000 (seq x2007 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x2000 x3500) (Curry_Prelude.d_C_return (Curry_Maybe.d_C_fromJust (let
               x2003 = leftSupply x2007
               x2006 = rightSupply x2007
                in (seq x2003 (seq x2006 (Curry_Prelude.d_C_lookup (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_funcName x2001 x3500) x2 x2002 x3500)))) (let
                    x2005 = leftSupply x2006
                    x2004 = rightSupply x2006
                     in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x4 x2004 x3500) x5 x2005 x3500)))) x3500)))) x3500) x3500) x3500)))))

d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_OP_browserGUI_dot_getSelectedModName_dot_295 x1 x4 x6 x3500) (d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82 x6 x1 x2 x3 x5) x3500

nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_OP_browserGUI_dot_getSelectedModName_dot_295 x1 x4 x6 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82 x6 x1 x2 x3 x5)) x2001 x3500)))))

d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_GUI.d_C_getValue x4 x1 x3500) (d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83 x1 x2 x6 x3 x5) x3500

nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_GUI.d_C_getValue x4 x1 x3500) (wrapNX id (nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83 x1 x2 x6 x3 x5)) x2000 x3500))

d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getCurrentFunctionAnalysis x2 x3500) (d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84 x1 x2 x3 x4 x5 x6) x3500

nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getCurrentFunctionAnalysis x2 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84 x1 x2 x3 x4 x5 x6)) x2001 x3500)))))

d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84 x1 x2 x3 x4 x5 x6 x7 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getFuns x2 x3500) (d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84_dot___hash_lambda85 x7 x1 x2 x3 x4 x5 x6) x3500

nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getFuns x2 x3500) (wrapNX id (nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84_dot___hash_lambda85 x7 x1 x2 x3 x4 x5 x6)) x2000 x3500))

d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84_dot___hash_lambda85 :: Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult) -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84_dot___hash_lambda85 x1 x2 x3 x4 x5 x6 x7 x8 x3500 = d_OP__case_8 x1 x2 x3 x4 x5 x6 x7 x8 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x4 Curry_Prelude.C_Nothing x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_null x7 x3500) (Curry_Prelude.d_OP_eq_eq x1 Curry_Prelude.C_Nothing x3500) x3500) x3500) x3500

nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84_dot___hash_lambda85 :: Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult) -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84_dot___hash_lambda85 x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_8 x1 x2 x3 x4 x5 x6 x7 x8 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x4 Curry_Prelude.C_Nothing x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_null x7 x3500) (Curry_Prelude.d_OP_eq_eq x1 Curry_Prelude.C_Nothing x3500) x3500) x3500) x2000 x3500))

d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84_dot___hash_lambda85_dot___hash_lambda86 :: Curry_GUI.C_GuiPort -> Curry_GUI.C_WidgetRef -> Curry_AnalysisTypes.C_AnalysisResult -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84_dot___hash_lambda85_dot___hash_lambda86 x1 x2 x3 x3500 = d_OP_browserGUI_dot_showAnalysisResult_dot_295 x2 x3 x1 x3500

d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 x1 x2 x3 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getContentsModule x1 x3500) (d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90 x5 x6 x3 x1 x2) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 x1 x2 x3 x1002 x3500) (d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 x1 x2 x3 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getContentsModule x1 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90 x5 x6 x3 x1 x2)) x2001 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 x1 x2 x3 x1002 x3000 x3500) (nd_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90 x1 x2 x3 x4 x5 x6 x3500 = d_OP__case_7 x1 x2 x3 x4 x5 x6 (Curry_Prelude.d_OP_eq_eq x1 x6 x3500) x3500

nd_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_7 x1 x2 x3 x4 x5 x6 (Curry_Prelude.d_OP_eq_eq x1 x6 x3500) x2000 x3500))

d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda91 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_Tuple2 Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda91 x1 x2 x3 x4 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x8 = d_C_findFunDeclInProgText x6 x7 (Curry_Prelude.OP_Tuple2 x1 x2) x3500
           in (d_OP__case_6 x3 x4 x8 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Int 0#) x3500) x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda91 x1 x2 x3 x4 x1002 x3500) (d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda91 x1 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda91 x1 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda91 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda92 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_Tuple2 Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda92 x1 x2 x3 x4 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x8 = d_C_findFunDeclInProgText x6 x7 (Curry_Prelude.OP_Tuple2 x1 x2) x3500
           in (d_OP__case_5 x3 x4 x8 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Int 0#) x3500) x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda92 x1 x2 x3 x4 x1002 x3500) (d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda92 x1 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda92 x1 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda92 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_browserGUI_dot_focusFunctionIfSelected_dot_295 :: Curry_GUI.C_WidgetRef -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_focusFunctionIfSelected_dot_295 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_GUI.d_C_getValue x3 x5 x3500) (d_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87 x1 x5 x2 x4) x3500

nd_OP_browserGUI_dot_focusFunctionIfSelected_dot_295 :: Curry_GUI.C_WidgetRef -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_focusFunctionIfSelected_dot_295 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_GUI.d_C_getValue x3 x5 x3500) (wrapNX id (nd_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87 x1 x5 x2 x4)) x2000 x3500))

d_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87 :: Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_GUI.d_C_getValue x1 x2 x3500) (d_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87_dot___hash_lambda88 x2 x3 x4 x5) x3500

nd_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87 :: Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_GUI.d_C_getValue x1 x2 x3500) (wrapNX id (nd_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87_dot___hash_lambda88 x2 x3 x4 x5)) x2000 x3500))

d_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87_dot___hash_lambda88 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87_dot___hash_lambda88 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getFuns x2 x3500) (d_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87_dot___hash_lambda88_dot___hash_lambda89 x5 x1 x2 x3 x4) x3500

nd_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87_dot___hash_lambda88 :: Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87_dot___hash_lambda88 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getFuns x2 x3500) (wrapNX id (nd_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87_dot___hash_lambda88_dot___hash_lambda89 x5 x1 x2 x3 x4)) x2000 x3500))

d_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87_dot___hash_lambda88_dot___hash_lambda89 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87_dot___hash_lambda88_dot___hash_lambda89 x1 x2 x3 x4 x5 x6 x3500 = d_OP__case_4 x1 x2 x3 x4 x5 x6 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_null x5 x3500) (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) Curry_Prelude.OP_List) x3500) x3500) x3500

nd_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87_dot___hash_lambda88_dot___hash_lambda89 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_focusFunctionIfSelected_dot_295_dot___hash_lambda87_dot___hash_lambda88_dot___hash_lambda89 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_4 x1 x2 x3 x4 x5 x6 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_null x5 x3500) (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) Curry_Prelude.OP_List) x3500) x3500) x2000 x3500))

d_OP_browserGUI_dot_selectFunction_dot_295 :: Curry_GUI.C_WidgetRef -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selectFunction_dot_295 x1 x2 x3 x4 x5 x6 x7 x8 x3500 = Curry_Prelude.d_OP_dollar (d_OP_browserGUI_dot_safeIO_dot_295 x2 x7 x8) (Curry_Prelude.d_OP_gt_gt (d_OP_browserGUI_dot_focusFunctionIfSelected_dot_295 x1 x2 x4 x7 x8 x3500) (d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295 x2 x3 x4 x5 x6 x8 x3500) x3500) x3500

nd_OP_browserGUI_dot_selectFunction_dot_295 :: Curry_GUI.C_WidgetRef -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selectFunction_dot_295 x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_OP_browserGUI_dot_safeIO_dot_295 x2 x7 x8)) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_gt_gt (nd_OP_browserGUI_dot_focusFunctionIfSelected_dot_295 x1 x2 x4 x7 x8 x2000 x3500) (nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295 x2 x3 x4 x5 x6 x8 x2001 x3500) x3500)))) x2003 x3500)))))

d_OP_browserGUI_dot_selectAna_dot_295 :: Curry_GUI.C_WidgetRef -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_selectAna_dot_295 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x3500 = Curry_Prelude.d_OP_dollar (d_OP_browserGUI_dot_safeIO_dot_295 x2 x7 x10) (Curry_Prelude.d_OP_gt_gt (d_C_setCurrentFunctionAnalysis x2 (Curry_Prelude.C_Just x9) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x1 x8 x10 x3500) (d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295 x2 x3 x4 x5 x6 x10 x3500) x3500) x3500) x3500

nd_OP_browserGUI_dot_selectAna_dot_295 :: Curry_GUI.C_WidgetRef -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_selectAna_dot_295 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_OP_browserGUI_dot_safeIO_dot_295 x2 x7 x10)) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_gt_gt (nd_C_setCurrentFunctionAnalysis x2 (Curry_Prelude.C_Just x9) x2000 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x1 x8 x10 x3500) (nd_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295 x2 x3 x4 x5 x6 x10 x2001 x3500) x3500) x3500)))) x2003 x3500)))))

d_OP_browserGUI_dot_deselectFunAna_dot_295 :: Curry_GUI.C_WidgetRef -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_deselectFunAna_dot_295 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_setCurrentFunctionAnalysis x2 Curry_Prelude.C_Nothing x3500) (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x1 (d_C_noAnalysisText x3500) x4 x3500) (Curry_GUI.d_C_setValue x3 Curry_Prelude.OP_List x4 x3500) x3500) x3500

nd_OP_browserGUI_dot_deselectFunAna_dot_295 :: Curry_GUI.C_WidgetRef -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_deselectFunAna_dot_295 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_OP_gt_gt (nd_C_setCurrentFunctionAnalysis x2 Curry_Prelude.C_Nothing x2000 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x1 (d_C_noAnalysisText x3500) x4 x3500) (Curry_GUI.d_C_setValue x3 Curry_Prelude.OP_List x4 x3500) x3500) x3500))

d_OP_browserGUI_dot_performAllAnalysis_dot_295 :: Curry_Prelude.Curry t0 => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_AnalysisTypes.C_FunctionAnalysis t0 -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
d_OP_browserGUI_dot_performAllAnalysis_dot_295 x1 x2 x3 x4 x5 x3500 = case x2 of
     (Curry_AnalysisTypes.C_LocalAnalysis x6) -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x3500) (Curry_Prelude.d_C_return (Curry_Prelude.d_C_map x6 x5 x3500) x3500) x3500
     (Curry_AnalysisTypes.C_LocalDataAnalysis x7) -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllTypes x1 x3 x4 x3500) (d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda106 x7 x5 x3) x3500
     (Curry_AnalysisTypes.C_GlobalAnalysis x8) -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllFunctions x1 x3 x4 x3500) (d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda107 x8 x5 x3) x3500
     (Curry_AnalysisTypes.C_GlobalDataAnalysis x9) -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllTypes x1 x3 x4 x3500) (d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109 x9 x5 x1 x4 x3) x3500
     (Curry_AnalysisTypes.Choice_C_FunctionAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot_performAllAnalysis_dot_295 x1 x1002 x3 x4 x5 x3500) (d_OP_browserGUI_dot_performAllAnalysis_dot_295 x1 x1003 x3 x4 x5 x3500)
     (Curry_AnalysisTypes.Choices_C_FunctionAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot_performAllAnalysis_dot_295 x1 z x3 x4 x5 x3500) x1002
     (Curry_AnalysisTypes.Guard_C_FunctionAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot_performAllAnalysis_dot_295 x1 x1002 x3 x4 x5) $! (addCs x1001 x3500))
     (Curry_AnalysisTypes.Fail_C_FunctionAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_browserGUI_dot_performAllAnalysis_dot_295 :: Curry_Prelude.Curry t0 => Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_AnalysisTypes.C_FunctionAnalysis t0 -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
nd_OP_browserGUI_dot_performAllAnalysis_dot_295 x1 x2 x3 x4 x5 x3000 x3500 = case x2 of
     (Curry_AnalysisTypes.HO_C_LocalAnalysis x6) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x2000 x3500) (Curry_Prelude.d_C_return (Curry_Prelude.nd_C_map x6 x5 x2001 x3500) x3500) x3500)))))
     (Curry_AnalysisTypes.HO_C_LocalDataAnalysis x7) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getAllTypes x1 x3 x4 x3500) (wrapNX id (nd_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda106 x7 x5 x3)) x2000 x3500))
     (Curry_AnalysisTypes.HO_C_GlobalAnalysis x8) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getAllFunctions x1 x3 x4 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda107 x8 x5 x3)) x2001 x3500)))))
     (Curry_AnalysisTypes.HO_C_GlobalDataAnalysis x9) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getAllTypes x1 x3 x4 x3500) (wrapNX id (nd_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109 x9 x5 x1 x4 x3)) x2000 x3500))
     (Curry_AnalysisTypes.Choice_C_FunctionAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_browserGUI_dot_performAllAnalysis_dot_295 x1 x1002 x3 x4 x5 x3000 x3500) (nd_OP_browserGUI_dot_performAllAnalysis_dot_295 x1 x1003 x3 x4 x5 x3000 x3500)
     (Curry_AnalysisTypes.Choices_C_FunctionAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_browserGUI_dot_performAllAnalysis_dot_295 x1 z x3 x4 x5 x3000 x3500) x1002
     (Curry_AnalysisTypes.Guard_C_FunctionAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_browserGUI_dot_performAllAnalysis_dot_295 x1 x1002 x3 x4 x5 x3000) $! (addCs x1001 x3500))
     (Curry_AnalysisTypes.Fail_C_FunctionAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda106 :: Curry_Prelude.Curry t1401 => (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_FlatCurry.C_FuncDecl -> ConstStore -> t1401) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1401)
d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda106 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x3500) (Curry_Prelude.d_C_return (Curry_Prelude.d_C_map (Curry_Prelude.d_C_apply x1 x4 x3500) x2 x3500) x3500) x3500

nd_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda106 :: Curry_Prelude.Curry t1401 => Func (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Func Curry_FlatCurry.C_FuncDecl t1401) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1401)
nd_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda106 x1 x2 x3 x4 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x2000 x3500) (Curry_Prelude.d_C_return (let
               x2002 = leftSupply x2003
               x2001 = rightSupply x2003
                in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_map (Curry_Prelude.nd_C_apply x1 x4 x2001 x3500) x2 x2002 x3500)))) x3500) x3500)))))

d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda107 :: Curry_Prelude.Curry t1401 => (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1401)) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1401)
d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda107 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x3500) (let
     x5 = Curry_Prelude.d_C_apply x1 x4 x3500
      in (Curry_Prelude.d_C_return (Curry_Prelude.d_C_map (d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda107_dot___hash_lambda108 x5) x2 x3500) x3500)) x3500

nd_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda107 :: Curry_Prelude.Curry t1401 => Func (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1401)) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1401)
nd_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda107 x1 x2 x3 x4 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x2000 x3500) (let
               x2001 = leftSupply x2003
               x2002 = rightSupply x2003
                in (seq x2001 (seq x2002 (let
                    x5 = Curry_Prelude.nd_C_apply x1 x4 x2001 x3500
                     in (Curry_Prelude.d_C_return (Curry_Prelude.nd_C_map (wrapDX id (d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda107_dot___hash_lambda108 x5)) x2 x2002 x3500) x3500))))) x3500)))))

d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda107_dot___hash_lambda108 :: Curry_Prelude.Curry t1401 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1401) -> Curry_FlatCurry.C_FuncDecl -> ConstStore -> t1401
d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda107_dot___hash_lambda108 x1 x2 x3500 = Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3500) x2 x3500) x1 x3500) x3500

d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109 :: Curry_Prelude.Curry t1401 => (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1401)) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1401)
d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAllFunctions x3 x5 x4 x3500) (d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109_dot___hash_lambda110 x1 x2 x5 x6) x3500

nd_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109 :: Curry_Prelude.Curry t1401 => Func (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Func (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1401))) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1401)
nd_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getAllFunctions x3 x5 x4 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109_dot___hash_lambda110 x1 x2 x5 x6)) x2001 x3500)))))

d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109_dot___hash_lambda110 :: Curry_Prelude.Curry t1401 => (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1401)) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1401)
d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109_dot___hash_lambda110 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x3500) (let
     x6 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x4 x3500) x5 x3500
      in (Curry_Prelude.d_C_return (Curry_Prelude.d_C_map (d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109_dot___hash_lambda110_dot___hash_lambda111 x6) x2 x3500) x3500)) x3500

nd_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109_dot___hash_lambda110 :: Curry_Prelude.Curry t1401 => Func (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Func (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1401))) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1401)
nd_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109_dot___hash_lambda110 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2000 = leftSupply x2006
          x2005 = rightSupply x2006
           in (seq x2000 (seq x2005 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_C_apply x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))) x2000 x3500) (let
               x2003 = leftSupply x2005
               x2004 = rightSupply x2005
                in (seq x2003 (seq x2004 (let
                    x6 = let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x4 x2001 x3500) x5 x2002 x3500)))
                     in (Curry_Prelude.d_C_return (Curry_Prelude.nd_C_map (wrapDX id (d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109_dot___hash_lambda110_dot___hash_lambda111 x6)) x2 x2004 x3500) x3500))))) x3500)))))

d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109_dot___hash_lambda110_dot___hash_lambda111 :: Curry_Prelude.Curry t1401 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1401) -> Curry_FlatCurry.C_FuncDecl -> ConstStore -> t1401
d_OP_browserGUI_dot_performAllAnalysis_dot_295_dot___hash_lambda109_dot___hash_lambda110_dot___hash_lambda111 x1 x2 x3500 = Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3500) x2 x3500) x1 x3500) x3500

d_OP_browserGUI_dot_analyzeAllFuns_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AnalysisTypes.C_FunctionAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_analyzeAllFuns_dot_295 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3500 = Curry_Prelude.d_OP_dollar (d_OP_browserGUI_dot_safeIO_dot_295 x1 x6 x9) (Curry_Prelude.d_OP_gt_gt_eq (d_OP_browserGUI_dot_getSelectedModName_dot_295 x1 x4 x9 x3500) (d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93 x8 x7 x9 x1 x2 x3 x5) x3500) x3500

nd_OP_browserGUI_dot_analyzeAllFuns_dot_295 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AnalysisTypes.C_FunctionAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_analyzeAllFuns_dot_295 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapNX id (nd_OP_browserGUI_dot_safeIO_dot_295 x1 x6 x9)) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_OP_browserGUI_dot_getSelectedModName_dot_295 x1 x4 x9 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93 x8 x7 x9 x1 x2 x3 x5)) x2001 x3500)))) x2003 x3500)))))

d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93 :: Curry_AnalysisTypes.C_FunctionAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93 x1 x2 x3 x4 x5 x6 x7 x8 x3500 = d_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 (Curry_Prelude.d_OP_eq_eq x8 Curry_Prelude.C_Nothing x3500) x3500

nd_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93 :: Curry_AnalysisTypes.C_FunctionAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93 x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 (Curry_Prelude.d_OP_eq_eq x8 Curry_Prelude.C_Nothing x3500) x2000 x3500))

d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94 :: Curry_AnalysisTypes.C_FunctionAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt (d_OP__case_2 x3 x4 x5 x7 x8 x9 x3500) (d_C_getFuns x4 x3500) x3500) (d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95 x1 x2 x3 x4 x5 x6 x7 x8) x3500

nd_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94 :: Curry_AnalysisTypes.C_FunctionAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.C_Bool -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt (nd_OP__case_2 x3 x4 x5 x7 x8 x9 x2000 x3500) (d_C_getFuns x4 x3500) x3500) (wrapNX id (nd_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95 x1 x2 x3 x4 x5 x6 x7 x8)) x2001 x3500)))))

d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95 :: Curry_AnalysisTypes.C_FunctionAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x6 x2 x3 x3500) (d_OP_browserGUI_dot_performAllAnalysis_dot_295 x4 x1 (d_OP_browserGUI_dot_showDoing_dot_295 x8 x3) (Curry_Maybe.d_C_fromJust x5 x3500) x9 x3500) x3500) (d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95_dot___hash_lambda96 x9 x3 x7) x3500

nd_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95 :: Curry_AnalysisTypes.C_FunctionAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GUI.C_GuiPort -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x6 x2 x3 x3500) (nd_OP_browserGUI_dot_performAllAnalysis_dot_295 x4 x1 (wrapDX id (d_OP_browserGUI_dot_showDoing_dot_295 x8 x3)) (Curry_Maybe.d_C_fromJust x5 x3500) x9 x2000 x3500) x3500) (wrapDX id (d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95_dot___hash_lambda96 x9 x3 x7)) x2001 x3500)))))

d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95_dot___hash_lambda96 :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Curry_GUI.C_GuiPort -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95_dot___hash_lambda96 x1 x2 x3 x4 x3500 = Curry_GUI.d_C_setConfig x3 (Curry_GUI.C_List (Curry_Prelude.d_C_map d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95_dot___hash_lambda96_dot___hash_lambda97 (Curry_Prelude.d_C_zip x4 x1 x3500) x3500)) x2 x3500

d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95_dot___hash_lambda96_dot___hash_lambda97 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95_dot___hash_lambda96_dot___hash_lambda97 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_snd (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3500) x3 x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95_dot___hash_lambda96_dot___hash_lambda97 x1002 x3500) (d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95_dot___hash_lambda96_dot___hash_lambda97 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95_dot___hash_lambda96_dot___hash_lambda97 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94_dot___hash_lambda95_dot___hash_lambda96_dot___hash_lambda97 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_browserGUI_dot___hash_lambda112 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_AnalysisTypes.C_FunctionAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_GUI.C_MenuItem
d_OP_browserGUI_dot___hash_lambda112 x1 x2 x3 x4 x5 x6 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple3 x8 x9 x10) -> Curry_GUI.C_MButton (d_OP_browserGUI_dot_showMBusy_dot_295 x5 (d_OP_browserGUI_dot_analyzeAllFuns_dot_295 x1 x2 x3 x4 x5 x6 x9 x10)) x8
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot___hash_lambda112 x1 x2 x3 x4 x5 x6 x1002 x3500) (d_OP_browserGUI_dot___hash_lambda112 x1 x2 x3 x4 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot___hash_lambda112 x1 x2 x3 x4 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot___hash_lambda112 x1 x2 x3 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_browserGUI_dot___hash_lambda112 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_AnalysisTypes.C_FunctionAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> ConstStore -> Curry_GUI.C_MenuItem
nd_OP_browserGUI_dot___hash_lambda112 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple3 x8 x9 x10) -> Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x5 (wrapNX id (nd_OP_browserGUI_dot_analyzeAllFuns_dot_295 x1 x2 x3 x4 x5 x6 x9 x10)))) x8
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_browserGUI_dot___hash_lambda112 x1 x2 x3 x4 x5 x6 x1002 x3000 x3500) (nd_OP_browserGUI_dot___hash_lambda112 x1 x2 x3 x4 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_browserGUI_dot___hash_lambda112 x1 x2 x3 x4 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_browserGUI_dot___hash_lambda112 x1 x2 x3 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_browserGUI_dot___hash_lambda113 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_AnalysisTypes.C_ModuleAnalysis Curry_AnalysisTypes.C_ModuleAnalysisResult) -> ConstStore -> Curry_GUI.C_MenuItem
d_OP_browserGUI_dot___hash_lambda113 x1 x2 x3 x4 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> Curry_GUI.C_MButton (d_OP_browserGUI_dot_showMBusy_dot_295 x3 (d_OP_browserGUI_dot_executeForModule_dot_295 x1 x2 x4 (acceptCs id (d_OP_browserGUI_dot_analyzeModuleWith_dot_295 x1 x3 x4 x7)))) x6
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot___hash_lambda113 x1 x2 x3 x4 x1002 x3500) (d_OP_browserGUI_dot___hash_lambda113 x1 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot___hash_lambda113 x1 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot___hash_lambda113 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_browserGUI_dot___hash_lambda113 :: Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_AnalysisTypes.C_ModuleAnalysis Curry_AnalysisTypes.C_ModuleAnalysisResult) -> IDSupply -> ConstStore -> Curry_GUI.C_MenuItem
nd_OP_browserGUI_dot___hash_lambda113 x1 x2 x3 x4 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x3 (wrapNX id (nd_OP_browserGUI_dot_executeForModule_dot_295 x1 x2 x4 (wrapDX (wrapNX id) (acceptCs id (nd_OP_browserGUI_dot_analyzeModuleWith_dot_295 x1 x3 x4 x7))))))) x6
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_browserGUI_dot___hash_lambda113 x1 x2 x3 x4 x1002 x3000 x3500) (nd_OP_browserGUI_dot___hash_lambda113 x1 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_browserGUI_dot___hash_lambda113 x1 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_browserGUI_dot___hash_lambda113 x1 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_browserGUI_dot___hash_lambda114 :: Curry_Prelude.Curry t1664 => Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1664)
d_OP_browserGUI_dot___hash_lambda114 x1 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_exitGUI x1 x3500) (Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500) x3500

d_OP_browserGUI_dot___hash_lambda115 :: Curry_GUI.C_WidgetRef -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult) -> ConstStore -> Curry_GUI.C_MenuItem
d_OP_browserGUI_dot___hash_lambda115 x1 x2 x3 x4 x5 x6 x7 x8 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x9 x10) -> Curry_GUI.C_MButton (d_OP_browserGUI_dot_showMBusy_dot_295 x6 (d_OP_browserGUI_dot_selectAna_dot_295 x1 x2 x3 x4 x5 x6 x7 x9 x10)) x9
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_browserGUI_dot___hash_lambda115 x1 x2 x3 x4 x5 x6 x7 x1002 x3500) (d_OP_browserGUI_dot___hash_lambda115 x1 x2 x3 x4 x5 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_browserGUI_dot___hash_lambda115 x1 x2 x3 x4 x5 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_browserGUI_dot___hash_lambda115 x1 x2 x3 x4 x5 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_browserGUI_dot___hash_lambda115 :: Curry_GUI.C_WidgetRef -> Curry_IOExts.C_IORef (Curry_Prelude.OP_Tuple6 (Curry_Prelude.OP_List (C_Tree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Imports.C_InterfaceOrFlatProg))) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AnalysisTypes.C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult))) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_AnalysisTypes.C_FunctionAnalysis Curry_AnalysisTypes.C_AnalysisResult) -> IDSupply -> ConstStore -> Curry_GUI.C_MenuItem
nd_OP_browserGUI_dot___hash_lambda115 x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x9 x10) -> Curry_GUI.HO_C_MButton (wrapNX id (nd_OP_browserGUI_dot_showMBusy_dot_295 x6 (wrapNX id (nd_OP_browserGUI_dot_selectAna_dot_295 x1 x2 x3 x4 x5 x6 x7 x9 x10)))) x9
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_browserGUI_dot___hash_lambda115 x1 x2 x3 x4 x5 x6 x7 x1002 x3000 x3500) (nd_OP_browserGUI_dot___hash_lambda115 x1 x2 x3 x4 x5 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_browserGUI_dot___hash_lambda115 x1 x2 x3 x4 x5 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_browserGUI_dot___hash_lambda115 x1 x2 x3 x4 x5 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isPublic :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_Bool
d_C_isPublic x1 x3500 = Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcVisibility x3500) x1 x3500) Curry_FlatCurry.C_Public x3500

d_C_findFunDeclInProgText :: Curry_AnalysisTypes.C_ContentsKind -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_Int
d_C_findFunDeclInProgText x1 x2 x3 x3500 = case x1 of
     Curry_AnalysisTypes.C_CurryProg -> d_C_findFirstDeclLine (Curry_FlatCurryShow.d_C_showCurryId (Curry_Prelude.d_C_snd x3 x3500) x3500) (Curry_Prelude.d_C_lines x2 x3500) (Curry_Prelude.C_Int 1#) x3500
     Curry_AnalysisTypes.C_LCurryProg -> d_C_findFirstDeclLine (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_FlatCurryShow.d_C_showCurryId (Curry_Prelude.d_C_snd x3 x3500) x3500) x3500) (Curry_Prelude.d_C_lines x2 x3500) (Curry_Prelude.C_Int 1#) x3500
     Curry_AnalysisTypes.C_FlatCurryExp -> d_C_findFirstDeclLine (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_fst x3 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_snd x3 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3500) x3500) x3500) x3500) (Curry_Prelude.d_C_lines x2 x3500) (Curry_Prelude.C_Int 1#) x3500
     Curry_AnalysisTypes.C_OtherText -> Curry_Prelude.C_Int 0#
     (Curry_AnalysisTypes.Choice_C_ContentsKind x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_findFunDeclInProgText x1002 x2 x3 x3500) (d_C_findFunDeclInProgText x1003 x2 x3 x3500)
     (Curry_AnalysisTypes.Choices_C_ContentsKind x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_findFunDeclInProgText z x2 x3 x3500) x1002
     (Curry_AnalysisTypes.Guard_C_ContentsKind x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_findFunDeclInProgText x1002 x2 x3) $! (addCs x1001 x3500))
     (Curry_AnalysisTypes.Fail_C_ContentsKind x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_findFirstDeclLine :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_findFirstDeclLine x1 x2 x3 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Int 0#
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_1 x1 x3 x4 x5 (Curry_List.d_C_isPrefixOf x1 x4 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_findFirstDeclLine x1 x1002 x3 x3500) (d_C_findFirstDeclLine x1 x1003 x3 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_findFirstDeclLine x1 z x3 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_findFirstDeclLine x1 x1002 x3) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_browserDir :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_browserDir x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Distribution.d_C_installDir x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))) x3500

d_C_leqQName :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_Bool
d_C_leqQName x1 x2 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_0 x3 x4 x2 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_leqQName x1002 x2 x3500) (d_C_leqQName x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_leqQName z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_leqQName x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showQNameWithMod :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showQNameWithMod x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showQNameWithMod x1002 x3500) (d_C_showQNameWithMod x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showQNameWithMod z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showQNameWithMod x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_noAnalysisText :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_noAnalysisText x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List))))))))))))))))))

nd_C_getAnswer :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_getAnswer x1 x2 x3 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2004 = leftSupply x2006
          x2005 = rightSupply x2006
           in (seq x2004 (seq x2005 (let
               x4 = generate x2005
                in (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_GUI.nd_C_runInitGUI Curry_Prelude.OP_List (Curry_GUI.C_Col Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_GUI.C_Label (Curry_Prelude.OP_Cons (Curry_GUI.C_Text x1) Curry_Prelude.OP_List)) (let
                         x2000 = leftSupply x2002
                         x2001 = rightSupply x2002
                          in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (Curry_GUI.C_Entry (Curry_Prelude.OP_Cons (Curry_GUI.C_Text x2) (Curry_Prelude.OP_Cons (Curry_GUI.C_WRef x4) (Curry_Prelude.OP_Cons (Curry_GUI.nd_C_Cmd (wrapNX id (nd_OP_getAnswer_dot_getinput_dot_540 x4 x3)) x2000 x3500) (Curry_Prelude.OP_Cons Curry_GUI.C_FillX (Curry_Prelude.OP_Cons (Curry_GUI.C_Background (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Width (Curry_Prelude.C_Int 50#)) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_GUI.nd_C_Button (wrapNX id (nd_OP_getAnswer_dot_getinput_dot_540 x4 x3)) (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) Curry_Prelude.OP_List))) Curry_Prelude.OP_List) x2001 x3500) Curry_Prelude.OP_List))))))) (wrapDX id (d_OP_getAnswer_dot___hash_lambda117 x4)) x2003 x3500)))))))))

d_OP_getAnswer_dot_getinput_dot_540 :: Curry_GUI.C_WidgetRef -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_getAnswer_dot_getinput_dot_540 x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_GUI.d_C_getValue x1 x3 x3500) (d_OP_getAnswer_dot_getinput_dot_540_dot___hash_lambda116 x2 x3) x3500

nd_OP_getAnswer_dot_getinput_dot_540 :: Curry_GUI.C_WidgetRef -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_getAnswer_dot_getinput_dot_540 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_GUI.d_C_getValue x1 x3 x3500) (wrapNX id (nd_OP_getAnswer_dot_getinput_dot_540_dot___hash_lambda116 x2 x3)) x2000 x3500))

d_OP_getAnswer_dot_getinput_dot_540_dot___hash_lambda116 :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_GUI.C_GuiPort -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_getAnswer_dot_getinput_dot_540_dot___hash_lambda116 x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply x1 x3 x3500) (Curry_GUI.d_C_exitGUI x2 x3500) x3500

nd_OP_getAnswer_dot_getinput_dot_540_dot___hash_lambda116 :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit) -> Curry_GUI.C_GuiPort -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_getAnswer_dot_getinput_dot_540_dot___hash_lambda116 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) (Curry_GUI.d_C_exitGUI x2 x3500) x3500))

d_OP_getAnswer_dot___hash_lambda117 :: Curry_Prelude.Curry t1665 => Curry_GUI.C_WidgetRef -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1665)
d_OP_getAnswer_dot___hash_lambda117 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_focusInput x1 x2 x3500) (Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500) x3500

d_OP__case_0 x3 x4 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Sort.d_C_leqString x3500) x4 x3500) x6 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 x6 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Sort.d_C_leqString x3500) x3 x3500) x5 x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x3 x4 x1002 x3500) (d_OP__case_0 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x3 x4 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2010 = x3000
           in (seq x2010 (let
               x2004 = leftSupply x2010
               x2009 = rightSupply x2010
                in (seq x2004 (seq x2009 (Curry_Prelude.d_OP_bar_bar (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Sort.nd_C_leqString x2000 x3500) x4 x2001 x3500)))) x6 x2003 x3500)))) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 x6 x3500) (let
                    x2008 = leftSupply x2009
                    x2007 = rightSupply x2009
                     in (seq x2008 (seq x2007 (Curry_Prelude.nd_C_apply (let
                         x2006 = leftSupply x2007
                         x2005 = rightSupply x2007
                          in (seq x2006 (seq x2005 (Curry_Prelude.nd_C_apply (Curry_Sort.nd_C_leqString x2005 x3500) x3 x2006 x3500)))) x5 x2008 x3500)))) x3500) x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x3 x4 x1002 x3000 x3500) (nd_OP__case_0 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> d_C_findFirstDeclLine x1 x5 (Curry_Prelude.d_OP_plus x3 (Curry_Prelude.C_Int 1#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x3 x4 x5 x1002 x3500) (d_OP__case_1 x1 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> d_C_findFirstDeclLine x1 x5 (Curry_Prelude.d_OP_plus x3 (Curry_Prelude.C_Int 1#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_1 x1 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x3 x4 x5 x7 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> d_OP_browserGUI_dot_showExportedFuns_dot_295 x4 x7 x8 (Curry_Maybe.d_C_fromJust x5 x3500) x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x3 x4 x5 x7 x8 x1002 x3500) (d_OP__case_2 x3 x4 x5 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x3 x4 x5 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x3 x4 x5 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x3 x4 x5 x7 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_browserGUI_dot_showExportedFuns_dot_295 x4 x7 x8 (Curry_Maybe.d_C_fromJust x5 x3500) x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x3 x4 x5 x7 x8 x1002 x3000 x3500) (nd_OP__case_2 x3 x4 x5 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x3 x4 x5 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x3 x4 x5 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getFunctionListKind x4 x3500) (d_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94 x1 x2 x3 x4 x8 x5 x6 x7) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 x1002 x3500) (d_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getFunctionListKind x4 x2000 x3500) (wrapNX id (nd_OP_browserGUI_dot_analyzeAllFuns_dot_295_dot___hash_lambda93_dot___hash_lambda94 x1 x2 x3 x4 x8 x5 x6 x7)) x2001 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 x1002 x3000 x3500) (nd_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 x3 x4 x2 (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3500) (Curry_Prelude.d_OP_bang_bang x6 (Curry_Read.d_C_readNat x5 x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x2 x3 x4 x5 x6 x1002 x3500) (d_OP__case_4 x1 x2 x3 x4 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 x2 x3 x4 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x2 x3 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295 x3 x4 x2 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_funcName x2000 x3500) (Curry_Prelude.d_OP_bang_bang x6 (Curry_Read.d_C_readNat x5 x3500) x3500) x2001 x3500)))) x2003 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x2 x3 x4 x5 x6 x1002 x3000 x3500) (nd_OP__case_4 x1 x2 x3 x4 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 x2 x3 x4 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x2 x3 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x3 x4 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_GUI.d_C_seeText x4 (Curry_Prelude.OP_Tuple2 x8 (Curry_Prelude.C_Int 1#)) x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x3 x4 x8 x1002 x3500) (d_OP__case_5 x3 x4 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x3 x4 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x3 x4 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x3 x4 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_GUI.d_C_seeText x4 (Curry_Prelude.OP_Tuple2 x8 (Curry_Prelude.C_Int 1#)) x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x3 x4 x8 x1002 x3000 x3500) (nd_OP__case_5 x3 x4 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x3 x4 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x3 x4 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x3 x4 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_GUI.d_C_seeText x4 (Curry_Prelude.OP_Tuple2 x8 (Curry_Prelude.C_Int 1#)) x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x3 x4 x8 x1002 x3500) (d_OP__case_6 x3 x4 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x3 x4 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x3 x4 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x3 x4 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_GUI.d_C_seeText x4 (Curry_Prelude.OP_Tuple2 x8 (Curry_Prelude.C_Int 1#)) x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x3 x4 x8 x1002 x3000 x3500) (nd_OP__case_6 x3 x4 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x3 x4 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x3 x4 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x1 x2 x3 x4 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getMainContents x4 x3500) (d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda91 x1 x2 x3 x5) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt (d_OP_browserGUI_dot_showSource_dot_295 x4 x5 x1 x3 x3500) (d_C_getMainContents x4 x3500) x3500) (d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda92 x1 x2 x3 x5) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x2 x3 x4 x5 x6 x1002 x3500) (d_OP__case_7 x1 x2 x3 x4 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 x2 x3 x4 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x2 x3 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getMainContents x4 x2000 x3500) (wrapDX id (d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda91 x1 x2 x3 x5)) x2001 x3500)))))
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (let
                    x2000 = leftSupply x2002
                    x2001 = rightSupply x2002
                     in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_gt_gt (nd_OP_browserGUI_dot_showSource_dot_295 x4 x5 x1 x3 x2000 x3500) (nd_C_getMainContents x4 x2001 x3500) x3500)))) (wrapDX id (d_OP_browserGUI_dot_showModuleAndFocusFunction_dot_295_dot___hash_lambda90_dot___hash_lambda92 x1 x2 x3 x5)) x2003 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1 x2 x3 x4 x5 x6 x1002 x3000 x3500) (nd_OP__case_7 x1 x2 x3 x4 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x1 x2 x3 x4 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1 x2 x3 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_OP_browserGUI_dot_performAnalysis_dot_295 x3 (Curry_Maybe.d_C_fromJust x1 x3500) (d_OP_browserGUI_dot_showDoing_dot_295 x6 x2) (Curry_Prelude.d_OP_bang_bang x8 (Curry_Read.d_C_readNat x7 x3500) x3500) x3500) (d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84_dot___hash_lambda85_dot___hash_lambda86 x2 x5) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x2 x3 x4 x5 x6 x7 x8 x1002 x3500) (d_OP__case_8 x1 x2 x3 x4 x5 x6 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 x2 x3 x4 x5 x6 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x2 x3 x4 x5 x6 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_OP_browserGUI_dot_performAnalysis_dot_295 x3 (Curry_Maybe.d_C_fromJust x1 x3500) (wrapDX id (d_OP_browserGUI_dot_showDoing_dot_295 x6 x2)) (Curry_Prelude.d_OP_bang_bang x8 (Curry_Read.d_C_readNat x7 x3500) x3500) x2000 x3500) (wrapDX id (d_OP_browserGUI_dot_analyzeFunctionIfSelected_dot_295_dot___hash_lambda82_dot___hash_lambda83_dot___hash_lambda84_dot___hash_lambda85_dot___hash_lambda86 x2 x5)) x2001 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1 x2 x3 x4 x5 x6 x7 x8 x1002 x3000 x3500) (nd_OP__case_8 x1 x2 x3 x4 x5 x6 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x1 x2 x3 x4 x5 x6 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1 x2 x3 x4 x5 x6 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x1 x2 x3 x4 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getFuns x2 x3500) (d_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79_dot___hash_lambda80 x1 x2 x3 x4 x5 x6) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x2 x3 x4 x5 x6 x1002 x3500) (d_OP__case_9 x1 x2 x3 x4 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 x2 x3 x4 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x2 x3 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getFuns x2 x3500) (wrapNX id (nd_OP_browserGUI_dot_selectInDirectCalls_dot_295_dot___hash_lambda78_dot___hash_lambda79_dot___hash_lambda80 x1 x2 x3 x4 x5 x6)) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x2 x3 x4 x5 x6 x1002 x3000 x3500) (nd_OP__case_9 x1 x2 x3 x4 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 x2 x3 x4 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x2 x3 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x1 x2 x3 x4 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getFuns x2 x3500) (d_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75_dot___hash_lambda76 x1 x2 x3 x4 x5 x6) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1 x2 x3 x4 x5 x6 x1002 x3500) (d_OP__case_10 x1 x2 x3 x4 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x1 x2 x3 x4 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1 x2 x3 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getFuns x2 x3500) (wrapNX id (nd_OP_browserGUI_dot_selectDirectCalls_dot_295_dot___hash_lambda74_dot___hash_lambda75_dot___hash_lambda76 x1 x2 x3 x4 x5 x6)) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x1 x2 x3 x4 x5 x6 x1002 x3000 x3500) (nd_OP__case_10 x1 x2 x3 x4 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x1 x2 x3 x4 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x1 x2 x3 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 x2 Curry_Prelude.OP_List (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_List.d_C_delete x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x3500) x3 x3500)) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x2 x3 x1002 x3500) (d_OP__case_11 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 x2 Curry_Prelude.OP_List (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_List.nd_C_delete x2000 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x2001 x3500)))) x3 x2003 x3500))))) Curry_Prelude.OP_List))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x2 x3 x1002 x3000 x3500) (nd_OP__case_11 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_AnalysisTypes.C_LCurryProg
     Curry_Prelude.C_False -> Curry_AnalysisTypes.C_CurryProg
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1 x1002 x3500) (d_OP__case_12 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_AnalysisTypes.C_LCurryProg
     Curry_Prelude.C_False -> Curry_AnalysisTypes.C_CurryProg
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1 x1002 x3000 x3500) (nd_OP__case_12 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x1 x2 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP_browserGUI_dot_putMainMessage_dot_295 x2 x4 x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))))))))))))))))))) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x3 (Curry_Maybe.d_C_fromJust x5 x3500) x3500) x1 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1 x2 x3 x4 x5 x1002 x3500) (d_OP__case_13 x1 x2 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x1 x2 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1 x2 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x1 x2 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_browserGUI_dot_putMainMessage_dot_295 x2 x4 x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))))))))))))))))))) x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x3 (Curry_Maybe.d_C_fromJust x5 x3500) x2000 x3500) x1 x2001 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x1 x2 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_13 x1 x2 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x1 x2 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x1 x2 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getTrees x1 x3500) (d_OP_browserGUI_dot_getSelectedModName_dot_295_dot___hash_lambda49_dot___hash_lambda50 x2) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1 x2 x1002 x3500) (d_OP__case_14 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getTrees x1 x3500) (wrapDX id (d_OP_browserGUI_dot_getSelectedModName_dot_295_dot___hash_lambda49_dot___hash_lambda50 x2)) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x1 x2 x1002 x3000 x3500) (nd_OP__case_14 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x1 x2 x3 x4 x5 x6 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_gt_gt (d_OP_browserGUI_dot_putMainMessage_dot_295 x2 x6 x1 Curry_Prelude.OP_List x3500) (Curry_GUI.d_C_setConfig x4 (Curry_GUI.C_List Curry_Prelude.OP_List) x1 x3500) x3500) (d_C_getTrees x2 x3500) x3500) (d_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46_dot___hash_lambda47 x1 x2 x3 x5 x7) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x1 x2 x3 x4 x5 x6 x7 x1002 x3500) (d_OP__case_15 x1 x2 x3 x4 x5 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x1 x2 x3 x4 x5 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x1 x2 x3 x4 x5 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt (let
                    x2000 = leftSupply x2002
                    x2001 = rightSupply x2002
                     in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_gt_gt (nd_OP_browserGUI_dot_putMainMessage_dot_295 x2 x6 x1 Curry_Prelude.OP_List x2000 x3500) (Curry_GUI.nd_C_setConfig x4 (Curry_GUI.C_List Curry_Prelude.OP_List) x1 x2001 x3500) x3500)))) (d_C_getTrees x2 x3500) x3500) (wrapNX id (nd_OP_browserGUI_dot_selmod_dot_295_dot___hash_lambda46_dot___hash_lambda47 x1 x2 x3 x5 x7)) x2003 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x1 x2 x3 x4 x5 x6 x7 x1002 x3000 x3500) (nd_OP__case_15 x1 x2 x3 x4 x5 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x1 x2 x3 x4 x5 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x1 x2 x3 x4 x5 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show (Curry_Prelude.d_OP_minus x4 x3 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))))))) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x3 x4 x1002 x3500) (d_OP__case_16 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show (Curry_Prelude.d_OP_minus x4 x3 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))))))) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x3 x4 x1002 x3000 x3500) (nd_OP__case_16 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_ShowGraph.d_C_setDotViewCmd x2 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1 x2 x1002 x3500) (d_OP__case_17 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_ShowGraph.d_C_setDotViewCmd x2 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x1 x2 x1002 x3000 x3500) (nd_OP__case_17 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (Curry_GUI.d_C_getValue x2 x1 x3500) (Curry_Prelude.d_C_writeFile x3) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x1 x2 x3 x1002 x3500) (d_OP__case_18 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_GUI.d_C_getValue x2 x1 x3500) (wrapDX id (Curry_Prelude.d_C_writeFile x3)) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_18 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_20 x2 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> d_OP__case_19 x2 x4 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x2 x3 x4 x1002 x3500) (d_OP__case_20 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x2 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_19 x2 x4 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_20 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x2 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> d_C_findDecl4name x4 x2 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x2 x4 x1002 x3500) (d_OP__case_19 x2 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x2 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x2 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x2 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> d_C_findDecl4name x4 x2 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x2 x4 x1002 x3000 x3500) (nd_OP__case_19 x2 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x2 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x2 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_22 x1 x2 x4 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_21 x1 x2 x4 x7 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1 x2 x4 x1002 x3500) (d_OP__case_22 x1 x2 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x1 x2 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1 x2 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x1 x2 x4 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_21 x1 x2 x4 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x1 x2 x4 x1002 x3000 x3500) (nd_OP__case_22 x1 x2 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x1 x2 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x1 x2 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x1 x2 x4 x7 x3500 = case x7 of
     (Curry_Imports.C_FP x8) -> Curry_Prelude.d_C_done x3500
     (Curry_Imports.C_IF x9) -> Curry_Prelude.d_OP_gt_gt (d_C_readProgAndStore x1 x2 x4 x3500) (Curry_Prelude.d_C_done x3500) x3500
     (Curry_Imports.Choice_C_InterfaceOrFlatProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x1 x2 x4 x1002 x3500) (d_OP__case_21 x1 x2 x4 x1003 x3500)
     (Curry_Imports.Choices_C_InterfaceOrFlatProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x1 x2 x4 z x3500) x1002
     (Curry_Imports.Guard_C_InterfaceOrFlatProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x1 x2 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Imports.Fail_C_InterfaceOrFlatProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x1 x2 x4 x7 x3000 x3500 = case x7 of
     (Curry_Imports.C_FP x8) -> Curry_Prelude.d_C_done x3500
     (Curry_Imports.C_IF x9) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_gt_gt (nd_C_readProgAndStore x1 x2 x4 x2000 x3500) (Curry_Prelude.d_C_done x3500) x3500))
     (Curry_Imports.Choice_C_InterfaceOrFlatProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x1 x2 x4 x1002 x3000 x3500) (nd_OP__case_21 x1 x2 x4 x1003 x3000 x3500)
     (Curry_Imports.Choices_C_InterfaceOrFlatProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x1 x2 x4 z x3000 x3500) x1002
     (Curry_Imports.Guard_C_InterfaceOrFlatProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x1 x2 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Imports.Fail_C_InterfaceOrFlatProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_25 x1 x2 x5 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_24 x1 x2 x5 x6 x7 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x1 x2 x5 x1002 x3500) (d_OP__case_25 x1 x2 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x1 x2 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x1 x2 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_25 x1 x2 x5 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_24 x1 x2 x5 x6 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x1 x2 x5 x1002 x3000 x3500) (nd_OP__case_25 x1 x2 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 x1 x2 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x1 x2 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_24 x1 x2 x5 x6 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> d_OP__case_23 x1 x2 x5 x6 x8 x9 (Curry_Prelude.d_OP_eq_eq x6 x1 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x1 x2 x5 x6 x1002 x3500) (d_OP__case_24 x1 x2 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x1 x2 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x1 x2 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_24 x1 x2 x5 x6 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_23 x1 x2 x5 x6 x8 x9 (Curry_Prelude.d_OP_eq_eq x6 x1 x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x1 x2 x5 x6 x1002 x3000 x3500) (nd_OP__case_24 x1 x2 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x1 x2 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x1 x2 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_23 x1 x2 x5 x6 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 (Curry_Prelude.OP_Tuple2 x8 (Curry_Imports.C_FP x2))) x5
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 (Curry_Prelude.OP_Tuple2 x8 x9)) (d_OP_readProgAndStore_dot_update_dot_269 x1 x2 x5 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x1 x2 x5 x6 x8 x9 x1002 x3500) (d_OP__case_23 x1 x2 x5 x6 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x1 x2 x5 x6 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x1 x2 x5 x6 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_23 x1 x2 x5 x6 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 (Curry_Prelude.OP_Tuple2 x8 (Curry_Imports.C_FP x2))) x5
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 (Curry_Prelude.OP_Tuple2 x8 x9)) (d_OP_readProgAndStore_dot_update_dot_269 x1 x2 x5 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x1 x2 x5 x6 x8 x9 x1002 x3000 x3500) (nd_OP__case_23 x1 x2 x5 x6 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x1 x2 x5 x6 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x1 x2 x5 x6 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_26 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple3 x8 x9 x10) -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x9 x10) x3500
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x1002 x3500) (d_OP__case_26 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_26 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple3 x8 x9 x10) -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x9 x10) x3500
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x1002 x3000 x3500) (nd_OP__case_26 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_27 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple3 x8 x9 x10) -> Curry_Prelude.d_C_return x8 x3500
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x1002 x3500) (d_OP__case_27 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_27 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple3 x8 x9 x10) -> Curry_Prelude.d_C_return x8 x3500
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x1002 x3000 x3500) (nd_OP__case_27 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_28 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x1 x2 x1002 x3500) (d_OP__case_28 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_28 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_28 x1 x2 x1002 x3000 x3500) (nd_OP__case_28 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_28 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_28 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_29 x1 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP_getAllImportsOfModule_dot_collectImports_dot_126 x1 x5 x3 x3500
     Curry_Prelude.C_False -> d_OP_getAllImportsOfModule_dot_collectImports_dot_126 x1 (Curry_Prelude.d_OP_plus_plus x5 (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x4 x1 x3500) x3500) x3500) (Curry_Prelude.OP_Cons x4 x3) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x1 x3 x4 x5 x1002 x3500) (d_OP__case_29 x1 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x1 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x1 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_29 x1 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP_getAllImportsOfModule_dot_collectImports_dot_126 x1 x5 x3 x3500
     Curry_Prelude.C_False -> d_OP_getAllImportsOfModule_dot_collectImports_dot_126 x1 (Curry_Prelude.d_OP_plus_plus x5 (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x4 x1 x3500) x3500) x3500) (Curry_Prelude.OP_Cons x4 x3) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_29 x1 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_29 x1 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_29 x1 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_29 x1 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_32 x2 x3500 = case x2 of
     (C_Leaf x4 x5) -> d_OP__case_31 x5 x3500
     (C_Node x8 x9 x10) -> d_OP__case_30 x9 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x1002 x3500) (d_OP__case_32 x1003 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 z x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x1002) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_32 x2 x3000 x3500 = case x2 of
     (C_Leaf x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_31 x5 x2000 x3500))
     (C_Node x8 x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_30 x9 x2000 x3500))
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_32 x1002 x3000 x3500) (nd_OP__case_32 x1003 x3000 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_32 z x3000 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_32 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_30 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> x12
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x1002 x3500) (d_OP__case_30 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_30 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> x12
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_30 x1002 x3000 x3500) (nd_OP__case_30 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_30 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_30 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_31 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> x7
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x1002 x3500) (d_OP__case_31 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_31 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> x7
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_31 x1002 x3000 x3500) (nd_OP__case_31 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_31 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_31 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_36 x1 x4 x3 x3500 = case x3 of
     (C_Leaf x5 x6) -> d_OP__case_35 x1 x4 x5 x6 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x3500
     (C_Node x7 x8 x9) -> d_OP__case_34 x1 x4 x7 x8 x9 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x1 x4 x1002 x3500) (d_OP__case_36 x1 x4 x1003 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x1 x4 z x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x1 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_36 x1 x4 x3 x3000 x3500 = case x3 of
     (C_Leaf x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_35 x1 x4 x5 x6 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x2000 x3500))
     (C_Node x7 x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_34 x1 x4 x7 x8 x9 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x2000 x3500))
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_36 x1 x4 x1002 x3000 x3500) (nd_OP__case_36 x1 x4 x1003 x3000 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_36 x1 x4 z x3000 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_36 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_34 x1 x4 x7 x8 x9 x11 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons (C_Leaf x7 x8) x4) x3500
     Curry_Prelude.C_False -> let
          x10 = Curry_Prelude.d_C_length (d_C_tree2strings (Curry_Prelude.C_Int 0#) (C_Node x7 x8 x9) x3500) x3500
           in (d_OP__case_33 x1 x4 x7 x8 x9 x10 (Curry_Prelude.d_OP_lt x1 x10 x3500) x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x1 x4 x7 x8 x9 x1002 x3500) (d_OP__case_34 x1 x4 x7 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x1 x4 x7 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x1 x4 x7 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_34 x1 x4 x7 x8 x9 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons (C_Leaf x7 x8) x4) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (let
               x10 = Curry_Prelude.d_C_length (d_C_tree2strings (Curry_Prelude.C_Int 0#) (C_Node x7 x8 x9) x3500) x3500
                in (nd_OP__case_33 x1 x4 x7 x8 x9 x10 (Curry_Prelude.d_OP_lt x1 x10 x3500) x2000 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_34 x1 x4 x7 x8 x9 x1002 x3000 x3500) (nd_OP__case_34 x1 x4 x7 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_34 x1 x4 x7 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_34 x1 x4 x7 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_33 x1 x4 x7 x8 x9 x10 x11 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (d_C_changeTrees (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x9 x3500) (d_OP_changeTrees_dot___hash_lambda11 x7 x4 x8) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_changeTrees (Curry_Prelude.d_OP_minus x1 x10 x3500) x4 x3500) (d_OP_changeTrees_dot___hash_lambda12 x9 x7 x8) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x1 x4 x7 x8 x9 x10 x1002 x3500) (d_OP__case_33 x1 x4 x7 x8 x9 x10 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x1 x4 x7 x8 x9 x10 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x1 x4 x7 x8 x9 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_33 x1 x4 x7 x8 x9 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_changeTrees (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x9 x3500) (wrapDX id (d_OP_changeTrees_dot___hash_lambda11 x7 x4 x8)) x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_changeTrees (Curry_Prelude.d_OP_minus x1 x10 x3500) x4 x3500) (wrapDX id (d_OP_changeTrees_dot___hash_lambda12 x9 x7 x8)) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_33 x1 x4 x7 x8 x9 x10 x1002 x3000 x3500) (nd_OP__case_33 x1 x4 x7 x8 x9 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_33 x1 x4 x7 x8 x9 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_33 x1 x4 x7 x8 x9 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_35 x1 x4 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (d_C_openNode x6 x3500) (d_OP_changeTrees_dot___hash_lambda9 x5 x4 x6) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_changeTrees (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x4 x3500) (d_OP_changeTrees_dot___hash_lambda10 x5 x6) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x1 x4 x5 x6 x1002 x3500) (d_OP__case_35 x1 x4 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x1 x4 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x1 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_35 x1 x4 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_openNode x6 x3500) (wrapDX id (d_OP_changeTrees_dot___hash_lambda9 x5 x4 x6)) x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_changeTrees (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x4 x3500) (wrapDX id (d_OP_changeTrees_dot___hash_lambda10 x5 x6)) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_35 x1 x4 x5 x6 x1002 x3000 x3500) (nd_OP__case_35 x1 x4 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_35 x1 x4 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_35 x1 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_40 x1 x4 x3 x3500 = case x3 of
     (C_Leaf x5 x6) -> d_OP__case_39 x1 x4 x6 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x3500
     (C_Node x7 x8 x9) -> d_OP__case_38 x1 x4 x7 x8 x9 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x1 x4 x1002 x3500) (d_OP__case_40 x1 x4 x1003 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x1 x4 z x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x1 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_40 x1 x4 x3 x3000 x3500 = case x3 of
     (C_Leaf x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_39 x1 x4 x6 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x2000 x3500))
     (C_Node x7 x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_38 x1 x4 x7 x8 x9 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x2000 x3500))
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_40 x1 x4 x1002 x3000 x3500) (nd_OP__case_40 x1 x4 x1003 x3000 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_40 x1 x4 z x3000 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_40 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_38 x1 x4 x7 x8 x9 x11 x3500 = case x11 of
     Curry_Prelude.C_True -> x8
     Curry_Prelude.C_False -> let
          x10 = Curry_Prelude.d_C_length (d_C_tree2strings (Curry_Prelude.C_Int 0#) (C_Node x7 x8 x9) x3500) x3500
           in (d_OP__case_37 x1 x4 x9 x10 (Curry_Prelude.d_OP_lt x1 x10 x3500) x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x1 x4 x7 x8 x9 x1002 x3500) (d_OP__case_38 x1 x4 x7 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x1 x4 x7 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x1 x4 x7 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_38 x1 x4 x7 x8 x9 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> x8
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (let
               x10 = Curry_Prelude.d_C_length (d_C_tree2strings (Curry_Prelude.C_Int 0#) (C_Node x7 x8 x9) x3500) x3500
                in (nd_OP__case_37 x1 x4 x9 x10 (Curry_Prelude.d_OP_lt x1 x10 x3500) x2000 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_38 x1 x4 x7 x8 x9 x1002 x3000 x3500) (nd_OP__case_38 x1 x4 x7 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_38 x1 x4 x7 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_38 x1 x4 x7 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_37 x1 x4 x9 x10 x11 x3500 = case x11 of
     Curry_Prelude.C_True -> d_C_getTreesValue (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x9 x3500
     Curry_Prelude.C_False -> d_C_getTreesValue (Curry_Prelude.d_OP_minus x1 x10 x3500) x4 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x1 x4 x9 x10 x1002 x3500) (d_OP__case_37 x1 x4 x9 x10 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x1 x4 x9 x10 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x1 x4 x9 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_37 x1 x4 x9 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> d_C_getTreesValue (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x9 x3500
     Curry_Prelude.C_False -> d_C_getTreesValue (Curry_Prelude.d_OP_minus x1 x10 x3500) x4 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_37 x1 x4 x9 x10 x1002 x3000 x3500) (nd_OP__case_37 x1 x4 x9 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_37 x1 x4 x9 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_37 x1 x4 x9 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_39 x1 x4 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> d_C_getTreesValue (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x4 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x1 x4 x6 x1002 x3500) (d_OP__case_39 x1 x4 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x1 x4 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x1 x4 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_39 x1 x4 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> d_C_getTreesValue (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x4 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_39 x1 x4 x6 x1002 x3000 x3500) (nd_OP__case_39 x1 x4 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_39 x1 x4 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_39 x1 x4 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_44 x1 x4 x3 x3500 = case x3 of
     (C_Leaf x5 x6) -> d_OP__case_43 x1 x4 x5 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x3500
     (C_Node x7 x8 x9) -> d_OP__case_42 x1 x4 x7 x8 x9 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x1 x4 x1002 x3500) (d_OP__case_44 x1 x4 x1003 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 x1 x4 z x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x1 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_44 x1 x4 x3 x3000 x3500 = case x3 of
     (C_Leaf x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_43 x1 x4 x5 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x2000 x3500))
     (C_Node x7 x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_42 x1 x4 x7 x8 x9 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x2000 x3500))
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_44 x1 x4 x1002 x3000 x3500) (nd_OP__case_44 x1 x4 x1003 x3000 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_44 x1 x4 z x3000 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_44 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_42 x1 x4 x7 x8 x9 x11 x3500 = case x11 of
     Curry_Prelude.C_True -> x7
     Curry_Prelude.C_False -> let
          x10 = Curry_Prelude.d_C_length (d_C_tree2strings (Curry_Prelude.C_Int 0#) (C_Node x7 x8 x9) x3500) x3500
           in (d_OP__case_41 x1 x4 x9 x10 (Curry_Prelude.d_OP_lt x1 x10 x3500) x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x1 x4 x7 x8 x9 x1002 x3500) (d_OP__case_42 x1 x4 x7 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x1 x4 x7 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x1 x4 x7 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_42 x1 x4 x7 x8 x9 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> x7
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (let
               x10 = Curry_Prelude.d_C_length (d_C_tree2strings (Curry_Prelude.C_Int 0#) (C_Node x7 x8 x9) x3500) x3500
                in (nd_OP__case_41 x1 x4 x9 x10 (Curry_Prelude.d_OP_lt x1 x10 x3500) x2000 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_42 x1 x4 x7 x8 x9 x1002 x3000 x3500) (nd_OP__case_42 x1 x4 x7 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_42 x1 x4 x7 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_42 x1 x4 x7 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_41 x1 x4 x9 x10 x11 x3500 = case x11 of
     Curry_Prelude.C_True -> d_C_getTreesNodeName (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x9 x3500
     Curry_Prelude.C_False -> d_C_getTreesNodeName (Curry_Prelude.d_OP_minus x1 x10 x3500) x4 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x1 x4 x9 x10 x1002 x3500) (d_OP__case_41 x1 x4 x9 x10 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x1 x4 x9 x10 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x1 x4 x9 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_41 x1 x4 x9 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> d_C_getTreesNodeName (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x9 x3500
     Curry_Prelude.C_False -> d_C_getTreesNodeName (Curry_Prelude.d_OP_minus x1 x10 x3500) x4 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_41 x1 x4 x9 x10 x1002 x3000 x3500) (nd_OP__case_41 x1 x4 x9 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_41 x1 x4 x9 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_41 x1 x4 x9 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_43 x1 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> x5
     Curry_Prelude.C_False -> d_C_getTreesNodeName (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x4 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x1 x4 x5 x1002 x3500) (d_OP__case_43 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_43 x1 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> x5
     Curry_Prelude.C_False -> d_C_getTreesNodeName (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x4 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_43 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_43 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_43 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_43 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_45 x2 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x2 x6 x1002 x3500) (d_OP__case_45 x2 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 x2 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x2 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_45 x2 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_45 x2 x6 x1002 x3000 x3500) (nd_OP__case_45 x2 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_45 x2 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_45 x2 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_46 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_C_start (Curry_FileGoodies.d_C_baseName (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FileGoodies.nd_C_stripSuffix x2000 x3500) x2 x2001 x3500)))) x3500) x2003 x3500)))))
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))) x3500) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_46 x1 x2 x1002 x3000 x3500) (nd_OP__case_46 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_46 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_46 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_47 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> d_C_version x3500
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_47 x1 x1002 x3500) (d_OP__case_47 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_47 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_47 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_47 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> d_C_version x3500
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_47 x1 x1002 x3000 x3500) (nd_OP__case_47 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_47 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_47 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
