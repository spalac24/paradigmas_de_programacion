{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Curry2JS (C_VarDef (..), d_C_explicitApply, d_C_optimizeSingleVars, d_C_optimizeUniqueCase, d_C_lazyStringConversion, d_C_prelude, d_C_wuiModName, nd_C_flatprog2JS, d_C_isRelevantFunction, d_C_genApply, nd_C_flat2JS, nd_C_flatExp2JS, nd_C_flatExps2JS, nd_C_case2JS, nd_C_branch2JS, d_C_isUniqueConstructor, nd_C_ite2JS, d_C_curryFunc2JSFunc, d_C_consQName2JS, d_C_qname2JS, d_C_encodeCurryId, d_C_jsFunctions, d_C_jsConstructors, d_C_jsOperators, d_C_ignoredFunctions, d_C_trySequentializeLetBindings, nd_C_trySequentializeLetBindings, d_C_trySequentializeLetBinding, d_C_freeVarsInExp, d_C_freeVarsInBranch, d_C_maxVarIndexInExp, d_C_pafsOfProg, d_C_pafsOfFuncs, d_C_pafsOfFunc, d_C_pafsOfExpr, d_C_mapUnion, nd_C_mapUnion, d_C_jscOfProg, d_C_jscOfFuncs, d_C_jscOfFunc, d_C_jscOfExpr, d_C_replaceJscOfProg, d_C_replaceJscOfFunc, d_C_replaceJscOfExpr, d_C_flatEmptyList, d_C_flatString, d_C_uniqueDefsOfStats, d_C_uniqueDefsOfStat, d_C_uniqueDefsOfExp, d_C_uniqueDefsOfExps, d_C_updateAssoc, d_C_removeSingleVarsJSStatements, d_C_removeSingleVarsInStats, d_C_maybeReplaceVar, d_C_removeSingleVarsInStat, d_C_removeSingleVarsInExp, nd_C_curry2js, nd_C_transformWUI, d_C_getAndTransformWUIConditions, nd_C_generateJavaScript, d_C_fileExistsAndNewerThan, d_C_showQName, nd_C_main, nd_C_mp, nd_C_mo) where

import Basics
import qualified Curry_Char
import qualified Curry_CompactFlatCurry
import qualified Curry_Directory
import qualified Curry_Distribution
import qualified Curry_FlatCurry
import qualified Curry_Integer
import qualified Curry_JavaScript
import qualified Curry_List
import qualified Curry_Maybe
import qualified Curry_Prelude
import qualified Curry_ReadNumeric
import qualified Curry_System
import qualified Curry_Time
import qualified Curry_Unsafe
import qualified Curry_FlatCurryShow
data C_VarDef
     = C_SimpleVar Curry_Prelude.C_Int
     | C_ComplexDef Curry_JavaScript.C_JSExp Curry_Prelude.C_Int
     | Choice_C_VarDef Cover ID C_VarDef C_VarDef
     | Choices_C_VarDef Cover ID ([C_VarDef])
     | Fail_C_VarDef Cover FailInfo
     | Guard_C_VarDef Cover Constraints C_VarDef

instance Show C_VarDef where
  showsPrec d (Choice_C_VarDef cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_VarDef cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_VarDef cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_VarDef cd info) = showChar '!'
  showsPrec _ (C_SimpleVar x1) = (showString "(SimpleVar") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_ComplexDef x1 x2) = (showString "(ComplexDef") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_VarDef where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_SimpleVar x1,r1) | (_,r0) <- readQualified "Curry2JS" "SimpleVar" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_ComplexDef x1 x2,r2) | (_,r0) <- readQualified "Curry2JS" "ComplexDef" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)


instance NonDet C_VarDef where
  choiceCons = Choice_C_VarDef
  choicesCons = Choices_C_VarDef
  failCons = Fail_C_VarDef
  guardCons = Guard_C_VarDef
  try (Choice_C_VarDef cd i x y) = tryChoice cd i x y
  try (Choices_C_VarDef cd i xs) = tryChoices cd i xs
  try (Fail_C_VarDef cd info) = Fail cd info
  try (Guard_C_VarDef cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_VarDef cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_VarDef cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_VarDef cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_VarDef cd i _) = error ("Curry2JS.VarDef.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_VarDef cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_VarDef cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_VarDef where
  generate s c = Choices_C_VarDef c (freeID [1,2] s) [(C_SimpleVar (generate (leftSupply s) c)),(C_ComplexDef (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_VarDef where
  ($!!) cont (C_SimpleVar x1) d cs = (((\y1 d cs -> cont (C_SimpleVar y1) d cs) $!! x1) d) cs
  ($!!) cont (C_ComplexDef x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ComplexDef y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_VarDef cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_VarDef cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_VarDef cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_VarDef cd info) _ _ = failCons cd info
  ($##) cont (C_SimpleVar x1) d cs = (((\y1 d cs -> cont (C_SimpleVar y1) d cs) $## x1) d) cs
  ($##) cont (C_ComplexDef x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ComplexDef y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_VarDef cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_VarDef cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_VarDef cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_VarDef cd info) _ _ = failCons cd info
  searchNF search cont (C_SimpleVar x1) = search (\y1 -> cont (C_SimpleVar y1)) x1
  searchNF search cont (C_ComplexDef x1 x2) = search (\y1 -> search (\y2 -> cont (C_ComplexDef y1 y2)) x2) x1
  searchNF _ _ x = error ("Curry2JS.VarDef.searchNF: no constructor: " ++ (show x))


instance Unifiable C_VarDef where
  (=.=) (C_SimpleVar x1) (C_SimpleVar y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_ComplexDef x1 x2) (C_ComplexDef y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_SimpleVar x1) (C_SimpleVar y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_ComplexDef x1 x2) (C_ComplexDef y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_SimpleVar x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_ComplexDef x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_VarDef cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_VarDef cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_VarDef cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_VarDef cd i _) = error ("Curry2JS.VarDef.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_VarDef cd info) = [(Unsolvable info)]
  bind d i (Guard_C_VarDef cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_SimpleVar x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_ComplexDef x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_VarDef cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_VarDef cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_VarDef cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_VarDef cd i _) = error ("Curry2JS.VarDef.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_VarDef cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_VarDef cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_VarDef where
  (=?=) (Choice_C_VarDef cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_VarDef cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_VarDef cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_VarDef cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_VarDef cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_VarDef cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_VarDef cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_VarDef cd info) _ _ = failCons cd info
  (=?=) (C_SimpleVar x1) (C_SimpleVar y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_ComplexDef x1 x2) (C_ComplexDef y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_VarDef cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_VarDef cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_VarDef cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_VarDef cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_VarDef cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_VarDef cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_VarDef cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_VarDef cd info) _ _ = failCons cd info
  (<?=) (C_SimpleVar x1) (C_SimpleVar y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_SimpleVar _) (C_ComplexDef _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ComplexDef x1 x2) (C_ComplexDef y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_explicitApply :: Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_explicitApply x3250 x3500 = Curry_Prelude.C_False

d_C_optimizeSingleVars :: Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_optimizeSingleVars x3250 x3500 = Curry_Prelude.C_True

d_C_optimizeUniqueCase :: Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_optimizeUniqueCase x3250 x3500 = Curry_Prelude.C_True

d_C_lazyStringConversion :: Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_lazyStringConversion x3250 x3500 = Curry_Prelude.C_True

d_C_prelude :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_prelude x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))

d_C_wuiModName :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_wuiModName x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))

nd_C_flatprog2JS :: Curry_FlatCurry.C_Prog -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSFDecl
nd_C_flatprog2JS x1 x3000 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (let
                    x7 = Curry_Prelude.nd_C_filter (wrapDX id d_C_isRelevantFunction) x5 x2000 x3250 x3500
                     in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_map (wrapNX id (nd_C_flat2JS x4)) x7 x2001 x3250 x3500) (d_OP__case_143 x7 (d_C_explicitApply x3250 x3500) x3250 x3500) x3250 x3500))))))
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_flatprog2JS x1002 x3000 x3250 x3500) (nd_C_flatprog2JS x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_flatprog2JS z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_flatprog2JS x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isRelevantFunction :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isRelevantFunction x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_142 x2 x6 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isRelevantFunction x1002 x3250 x3500) (d_C_isRelevantFunction x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isRelevantFunction z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isRelevantFunction x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_genApply :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_JavaScript.C_JSFDecl
d_C_genApply x1 x3250 x3500 = Curry_JavaScript.C_JSFDecl (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 1#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 2#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSSwitch (Curry_JavaScript.C_JSIArrayIdx (Curry_Prelude.C_Int 1#) (Curry_Prelude.C_Int 0#)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_genApply_dot_branch4fun_dot_33 x3250 x3500) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSDefault (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSReturn (Curry_JavaScript.C_JSFCall (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSIVar (Curry_Prelude.C_Int 1#)) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSIVar (Curry_Prelude.C_Int 2#)) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List)) Curry_Prelude.OP_List) x3250 x3500)) Curry_Prelude.OP_List)

d_OP_genApply_dot_branch4fun_dot_33 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSBranch
d_OP_genApply_dot_branch4fun_dot_33 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_139 x3 x2 (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_genApply_dot_branch4fun_dot_33 x1002 x3250 x3500) (d_OP_genApply_dot_branch4fun_dot_33 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_genApply_dot_branch4fun_dot_33 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_genApply_dot_branch4fun_dot_33 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_genApply_dot_branch4fun_dot_33_dot___hash_lambda1 :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_OP_genApply_dot_branch4fun_dot_33_dot___hash_lambda1 x1 x3250 x3500 = Curry_JavaScript.C_JSIArrayIdx (Curry_Prelude.C_Int 1#) x1

nd_C_flat2JS :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_FuncDecl -> IDSupply -> Cover -> ConstStore -> Curry_JavaScript.C_JSFDecl
nd_C_flat2JS x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Func x3 x4 x5 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_138 x1 x3 x7 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_flat2JS x1 x1002 x3000 x3250 x3500) (nd_C_flat2JS x1 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_flat2JS x1 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_flat2JS x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_flatExp2JS :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
nd_C_flatExp2JS x1 x2 x3 x4 x5 x6 x3000 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_Var x7) -> d_OP___cond_0_flatExp2JS x5 x7 x4 (Curry_Prelude.d_OP_eq_colon_eq x3 x2 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Lit x8) -> d_OP__case_137 x5 x2 x3 x8 x3250 x3500
     (Curry_FlatCurry.C_Comb x12 x13 x14) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_136 x14 x2 x13 x5 x4 x3 x1 x12 x2000 x3250 x3500))
     (Curry_FlatCurry.C_Let x31 x32) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_maybe (Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) (wrapNX id (nd_OP_flatExp2JS_dot___hash_lambda3 x1 x32 x3 x2 x4 x5)) (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_trySequentializeLetBindings x2000 x3250 x3500) x31 x2001 x3250 x3500)))) x2003 x3250 x3500)))))
     (Curry_FlatCurry.C_Case x33 x34 x35) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_131 x35 x34 x5 x4 x3 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x35 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_OP_flatExp2JS_dot_hasConstPattern_dot_122 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_bang_bang x35 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500) (d_OP_flatExp2JS_dot_hasConstPattern_dot_122 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_bang_bang x35 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_FlatCurry.C_Free x36 x37) -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_FlatCurry.C_Or x38 x39) -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_flatExp2JS x1 x2 x3 x4 x5 x1002 x3000 x3250 x3500) (nd_C_flatExp2JS x1 x2 x3 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_flatExp2JS x1 x2 x3 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_flatExp2JS x1 x2 x3 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___cond_0_flatExp2JS :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Success -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP___cond_0_flatExp2JS x1 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_Success -> Curry_Prelude.d_C_maybe (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (Curry_JavaScript.C_JSIVar x1) (Curry_JavaScript.C_JSIVar x2)) Curry_Prelude.OP_List) (d_OP_flatExp2JS_dot___hash_lambda2 x1) (Curry_Prelude.d_C_lookup x2 x3 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_flatExp2JS x1 x2 x3 x1002 x3250 x3500) (d_OP___cond_0_flatExp2JS x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_flatExp2JS x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_flatExp2JS x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_flatExp2JS_dot___hash_lambda2 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP_flatExp2JS_dot___hash_lambda2 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (Curry_JavaScript.C_JSIVar x1) (Curry_JavaScript.C_JSIArrayIdx x3 x4)) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_flatExp2JS_dot___hash_lambda2 x1 x1002 x3250 x3500) (d_OP_flatExp2JS_dot___hash_lambda2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_flatExp2JS_dot___hash_lambda2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_flatExp2JS_dot___hash_lambda2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_flatExp2JS_dot_genLambda_dot_92 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_OP_flatExp2JS_dot_genLambda_dot_92 x1 x2 x3 x4 x5 x3250 x3500 = d_OP__case_130 x3 x5 x4 x2 x1 (Curry_Prelude.d_OP_eq_eq x5 x3 x3250 x3500) x3250 x3500

d_OP_flatExp2JS_dot_genLambda_dot_107 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_OP_flatExp2JS_dot_genLambda_dot_107 x1 x2 x3 x4 x5 x3250 x3500 = d_OP__case_129 x3 x5 x4 x2 x1 (Curry_Prelude.d_OP_eq_eq x5 x3 x3250 x3500) x3250 x3500

nd_OP_flatExp2JS_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
nd_OP_flatExp2JS_dot___hash_lambda3 x1 x2 x3 x4 x5 x6 x7 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2002 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2002 (seq x2003 (let
               x8 = generate x2003 x3250
                in (let
                    x2000 = leftSupply x2002
                    x2001 = rightSupply x2002
                     in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_flatExps2JS x1 x4 x8 x5 x7 x2000 x3250 x3500) (nd_C_flatExp2JS x1 x8 x3 x5 x6 x2 x2001 x3250 x3500) x3250 x3500)))))))))

d_OP_flatExp2JS_dot_hasConstPattern_dot_122 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_flatExp2JS_dot_hasConstPattern_dot_122 x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Branch x3 x4) -> d_OP__case_128 x1 x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_flatExp2JS_dot_hasConstPattern_dot_122 x1 x1002 x3250 x3500) (d_OP_flatExp2JS_dot_hasConstPattern_dot_122 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_flatExp2JS_dot_hasConstPattern_dot_122 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_flatExp2JS_dot_hasConstPattern_dot_122 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_flatExp2JS_dot_expOfBranch_dot_122 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP_flatExp2JS_dot_expOfBranch_dot_122 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> x3
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_flatExp2JS_dot_expOfBranch_dot_122 x1002 x3250 x3500) (d_OP_flatExp2JS_dot_expOfBranch_dot_122 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_flatExp2JS_dot_expOfBranch_dot_122 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_flatExp2JS_dot_expOfBranch_dot_122 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_flatExps2JS :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
nd_C_flatExps2JS x1 x2 x3 x4 x5 x3000 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> d_OP___cond_0_flatExps2JS (Curry_Prelude.d_OP_eq_colon_eq x3 x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_110 x7 x4 x3 x1 x2 x6 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_flatExps2JS x1 x2 x3 x4 x1002 x3000 x3250 x3500) (nd_C_flatExps2JS x1 x2 x3 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_flatExps2JS x1 x2 x3 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_flatExps2JS x1 x2 x3 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___cond_0_flatExps2JS :: Curry_Prelude.C_Success -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP___cond_0_flatExps2JS x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_Success -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_flatExps2JS x1002 x3250 x3500) (d_OP___cond_0_flatExps2JS x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_flatExps2JS z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_flatExps2JS x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_case2JS :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
nd_C_case2JS x1 x2 x3 x4 x5 x6 x7 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x8 = Curry_Prelude.d_OP_plus x2 (Curry_Prelude.C_Int 1#) x3250 x3500
           in (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x9 = generate x2003 x3250
                     in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSVarDecl x8) Curry_Prelude.OP_List) (let
                         x2000 = leftSupply x2002
                         x2001 = rightSupply x2002
                          in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_flatExp2JS x1 x8 x9 x4 x8 x6 x2000 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP_case2JS_dot_caseStringProlog_dot_164 x7 x8 x3250 x3500) (nd_OP__case_109 x7 x1 x5 x8 x4 x3 x9 (Curry_Prelude.d_OP_ampersand_ampersand (d_C_optimizeUniqueCase x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x7 x3250 x3500) (Curry_Prelude.C_Int 1#) x3250 x3500) (d_OP_case2JS_dot_branchWithUniqueCase_dot_164 x1 (Curry_Prelude.d_C_head x7 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x2001 x3250 x3500) x3250 x3500) x3250 x3500)))) x3250 x3500)))))))

d_OP_case2JS_dot_listBranches_dot_164 :: Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_case2JS_dot_listBranches_dot_164 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_108 x2 x3250 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_case2JS_dot_listBranches_dot_164 x1002 x3250 x3500) (d_OP_case2JS_dot_listBranches_dot_164 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_case2JS_dot_listBranches_dot_164 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_case2JS_dot_listBranches_dot_164 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_case2JS_dot_caseStringProlog_dot_164 :: Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP_case2JS_dot_caseStringProlog_dot_164 x1 x2 x3250 x3500 = d_OP__case_106 x1 x2 (Curry_Prelude.d_OP_ampersand_ampersand (d_C_lazyStringConversion x3250 x3500) (d_OP_case2JS_dot_listBranches_dot_164 x1 x3250 x3500) x3250 x3500) x3250 x3500

d_OP_case2JS_dot_branchWithUniqueCase_dot_164 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_case2JS_dot_branchWithUniqueCase_dot_164 x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Branch x3 x4) -> d_OP__case_105 x1 x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_case2JS_dot_branchWithUniqueCase_dot_164 x1 x1002 x3250 x3500) (d_OP_case2JS_dot_branchWithUniqueCase_dot_164 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_case2JS_dot_branchWithUniqueCase_dot_164 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_case2JS_dot_branchWithUniqueCase_dot_164 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_case2JS_dot___hash_selFP2_hash_cstats :: Curry_Prelude.OP_List Curry_JavaScript.C_JSBranch -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP_case2JS_dot___hash_selFP2_hash_cstats x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_104 x3 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_case2JS_dot___hash_selFP2_hash_cstats x1002 x3250 x3500) (d_OP_case2JS_dot___hash_selFP2_hash_cstats x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_case2JS_dot___hash_selFP2_hash_cstats z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_case2JS_dot___hash_selFP2_hash_cstats x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_branch2JS :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSBranch
nd_C_branch2JS x1 x2 x3 x4 x5 x6 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.OP_List -> d_OP___cond_0_branch2JS (Curry_Prelude.d_OP_eq_colon_eq x3 x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_102 x2 x5 x9 x6 x4 x3 x1 x8 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_branch2JS x1 x2 x3 x4 x5 x6 x1002 x3000 x3250 x3500) (nd_C_branch2JS x1 x2 x3 x4 x5 x6 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_branch2JS x1 x2 x3 x4 x5 x6 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_branch2JS x1 x2 x3 x4 x5 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___cond_0_branch2JS :: Curry_Prelude.C_Success -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSBranch
d_OP___cond_0_branch2JS x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_Success -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_branch2JS x1002 x3250 x3500) (d_OP___cond_0_branch2JS x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_branch2JS z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_branch2JS x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_branch2JS_dot___hash_lambda6 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)
d_OP_branch2JS_dot___hash_lambda6 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Tuple2 x1 x3)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_branch2JS_dot___hash_lambda6 x1 x1002 x3250 x3500) (d_OP_branch2JS_dot___hash_lambda6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_branch2JS_dot___hash_lambda6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_branch2JS_dot___hash_lambda6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isUniqueConstructor :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isUniqueConstructor x1 x2 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))))))))))) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_100 x2 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isUniqueConstructor x1002 x2 x3250 x3500) (d_C_isUniqueConstructor x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isUniqueConstructor z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isUniqueConstructor x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_isUniqueConstructor_dot___hash_lambda7 :: Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_isUniqueConstructor_dot___hash_lambda7 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x2 x3 x4 x5) -> x2
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isUniqueConstructor_dot___hash_lambda7 x1002 x3250 x3500) (d_OP_isUniqueConstructor_dot___hash_lambda7 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isUniqueConstructor_dot___hash_lambda7 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isUniqueConstructor_dot___hash_lambda7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_ite2JS :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
nd_C_ite2JS x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3250 x3500 = let
     x2007 = x3000
      in (seq x2007 (let
          x9 = Curry_Prelude.d_OP_plus x2 (Curry_Prelude.C_Int 1#) x3250 x3500
           in (let
               x2004 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2004 (seq x2008 (let
                    x2005 = leftSupply x2008
                    x2006 = rightSupply x2008
                     in (seq x2005 (seq x2006 (let
                         x10 = generate x2005 x3250
                         x11 = generate x2006 x3250
                          in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSVarDecl x9) Curry_Prelude.OP_List) (let
                              x2000 = leftSupply x2004
                              x2003 = rightSupply x2004
                               in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (nd_C_flatExp2JS x1 x9 x10 x4 x9 x6 x2000 x3250 x3500) (Curry_Prelude.OP_Cons (let
                                   x2001 = leftSupply x2003
                                   x2002 = rightSupply x2003
                                    in (seq x2001 (seq x2002 (Curry_JavaScript.C_JSIf (Curry_JavaScript.C_JSIVar x9) (nd_C_flatExp2JS x1 x10 x11 x4 x5 x7 x2001 x3250 x3500) (nd_C_flatExp2JS x1 x11 x3 x4 x5 x8 x2002 x3250 x3500))))) Curry_Prelude.OP_List) x3250 x3500)))) x3250 x3500))))))))))

d_C_curryFunc2JSFunc :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_JavaScript.C_JSExp -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_C_curryFunc2JSFunc x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_98 x2 x1 x3 x4 x3250 x3500
     Curry_Prelude.OP_List -> Curry_JavaScript.C_JSFCall (d_C_qname2JS x1 x3250 x3500) x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_curryFunc2JSFunc x1 x1002 x3250 x3500) (d_C_curryFunc2JSFunc x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_curryFunc2JSFunc x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_curryFunc2JSFunc x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_curryFunc2JSFunc_dot___hash_lambda9 :: Curry_JavaScript.C_JSExp -> Curry_JavaScript.C_JSExp -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_OP_curryFunc2JSFunc_dot___hash_lambda9 x1 x2 x3 x3250 x3500 = Curry_JavaScript.C_JSOp x3 x1 x2

d_C_consQName2JS :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_consQName2JS x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_95 x3 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 2#) x3 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_consQName2JS x1002 x3250 x3500) (d_C_consQName2JS x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_consQName2JS z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_consQName2JS x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_qname2JS :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_qname2JS x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_C_maybe (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List) (d_C_encodeCurryId x3 x3250 x3500) x3250 x3500) x3250 x3500) Curry_Prelude.d_C_id (Curry_Prelude.d_C_lookup x1 (Curry_Prelude.d_OP_plus_plus (d_C_jsFunctions x3250 x3500) (d_C_jsOperators x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_qname2JS x1002 x3250 x3500) (d_C_qname2JS x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_qname2JS z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_qname2JS x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_encodeCurryId :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_encodeCurryId x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_93 x2 x3 (Curry_Char.d_C_isAlphaNum x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_encodeCurryId x1002 x3250 x3500) (d_C_encodeCurryId x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_encodeCurryId z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_encodeCurryId x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_encodeCurryId_dot_int2hex_dot_247 :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Char
d_OP_encodeCurryId_dot_int2hex_dot_247 x1 x3250 x3500 = d_OP__case_91 x1 (Curry_Prelude.d_OP_lt x1 (Curry_Prelude.C_Int 10#) x3250 x3500) x3250 x3500

d_C_jsFunctions :: Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_jsFunctions x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List)))))))))

d_C_jsConstructors :: Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_jsConstructors x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List))) Curry_Prelude.OP_List)

d_C_jsOperators :: Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_jsOperators x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '%'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) Curry_Prelude.OP_List)) Curry_Prelude.OP_List))))))))))

d_C_ignoredFunctions :: Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_ignoredFunctions x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst (Curry_Prelude.d_OP_plus_plus (d_C_jsFunctions x3250 x3500) (d_C_jsOperators x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))) Curry_Prelude.OP_List)))))))) x3250 x3500

d_C_trySequentializeLetBindings :: Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr))
d_C_trySequentializeLetBindings x3250 x3500 = d_C_trySequentializeLetBinding (Curry_Prelude.C_Int 0#)

nd_C_trySequentializeLetBindings :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr)) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr)))
nd_C_trySequentializeLetBindings x3000 x3250 x3500 = wrapDX id (d_C_trySequentializeLetBinding (Curry_Prelude.C_Int 0#))

d_C_trySequentializeLetBinding :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr))
d_C_trySequentializeLetBinding x1 x2 x3250 x3500 = d_OP__case_90 x2 x1 (Curry_Prelude.d_OP_eq_eq x2 Curry_Prelude.OP_List x3250 x3500) x3250 x3500

d_OP_trySequentializeLetBinding_dot___hash_lambda10 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr))
d_OP_trySequentializeLetBinding_dot___hash_lambda10 x1 x2 x3 x3250 x3500 = Curry_Prelude.C_Just (Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_bang_bang x1 x2 x3250 x3500) x3)

d_C_freeVarsInExp :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_C_freeVarsInExp x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Lit x3) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_freeVarsInExp x3250 x3500) x6 x3250 x3500
     (Curry_FlatCurry.C_Or x7 x8) -> Curry_Prelude.d_OP_plus_plus (d_C_freeVarsInExp x7 x3250 x3500) (d_C_freeVarsInExp x8 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Let x9 x10) -> let
          x11 = Curry_Prelude.d_C_unzip x9 x3250 x3500
          x12 = d_OP_freeVarsInExp_dot___hash_selFP4_hash_bvs x11 x3250 x3500
          x13 = d_OP_freeVarsInExp_dot___hash_selFP5_hash_bes x11 x3250 x3500
           in (Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip Curry_Prelude.d_C_notElem x12) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_freeVarsInExp x3250 x3500) (Curry_Prelude.OP_Cons x10 x13) x3250 x3500) x3250 x3500)
     (Curry_FlatCurry.C_Free x14 x15) -> Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip Curry_Prelude.d_C_notElem x14) (d_C_freeVarsInExp x15 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Case x16 x17 x18) -> Curry_Prelude.d_OP_plus_plus (d_C_freeVarsInExp x17 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_freeVarsInBranch x3250 x3500) x18 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_freeVarsInExp x1002 x3250 x3500) (d_C_freeVarsInExp x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_freeVarsInExp z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_freeVarsInExp x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_freeVarsInExp_dot___hash_selFP4_hash_bvs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_FlatCurry.C_Expr) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_freeVarsInExp_dot___hash_selFP4_hash_bvs x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_freeVarsInExp_dot___hash_selFP4_hash_bvs x1002 x3250 x3500) (d_OP_freeVarsInExp_dot___hash_selFP4_hash_bvs x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_freeVarsInExp_dot___hash_selFP4_hash_bvs z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_freeVarsInExp_dot___hash_selFP4_hash_bvs x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_freeVarsInExp_dot___hash_selFP5_hash_bes :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_FlatCurry.C_Expr) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr
d_OP_freeVarsInExp_dot___hash_selFP5_hash_bes x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_freeVarsInExp_dot___hash_selFP5_hash_bes x1002 x3250 x3500) (d_OP_freeVarsInExp_dot___hash_selFP5_hash_bes x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_freeVarsInExp_dot___hash_selFP5_hash_bes z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_freeVarsInExp_dot___hash_selFP5_hash_bes x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_freeVarsInBranch :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_C_freeVarsInBranch x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> d_OP__case_86 x3 x2 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_freeVarsInBranch x1002 x3250 x3500) (d_C_freeVarsInBranch x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_freeVarsInBranch z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_freeVarsInBranch x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_maxVarIndexInExp :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_maxVarIndexInExp x1 x3250 x3500 = let
     x2 = d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 x1 x3250 x3500
      in (d_OP__case_85 x2 (Curry_Prelude.d_C_null x2 x3250 x3500) x3250 x3500)

d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Lit x3) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 x3250 x3500) x6 x3250 x3500
     (Curry_FlatCurry.C_Or x7 x8) -> Curry_Prelude.d_OP_plus_plus (d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 x7 x3250 x3500) (d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 x8 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Let x9 x10) -> let
          x11 = Curry_Prelude.d_C_unzip x9 x3250 x3500
          x12 = d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP7_hash_bvs x11 x3250 x3500
          x13 = d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP8_hash_bes x11 x3250 x3500
           in (Curry_Prelude.d_OP_plus_plus x12 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 x3250 x3500) (Curry_Prelude.OP_Cons x10 x13) x3250 x3500) x3250 x3500)
     (Curry_FlatCurry.C_Free x14 x15) -> Curry_Prelude.d_OP_plus_plus x14 (d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 x15 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Case x16 x17 x18) -> Curry_Prelude.d_OP_plus_plus (d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 x17 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_maxVarIndexInExp_dot_allVarsInBranch_dot_300 x3250 x3500) x18 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 x1002 x3250 x3500) (d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP7_hash_bvs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_FlatCurry.C_Expr) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP7_hash_bvs x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP7_hash_bvs x1002 x3250 x3500) (d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP7_hash_bvs x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP7_hash_bvs z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP7_hash_bvs x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP8_hash_bes :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_FlatCurry.C_Expr) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr
d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP8_hash_bes x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP8_hash_bes x1002 x3250 x3500) (d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP8_hash_bes x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP8_hash_bes z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300_dot___hash_selFP8_hash_bes x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_maxVarIndexInExp_dot_allVarsInBranch_dot_300 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_maxVarIndexInExp_dot_allVarsInBranch_dot_300 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> d_OP__case_84 x3 x2 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_maxVarIndexInExp_dot_allVarsInBranch_dot_300 x1002 x3250 x3500) (d_OP_maxVarIndexInExp_dot_allVarsInBranch_dot_300 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_maxVarIndexInExp_dot_allVarsInBranch_dot_300 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_maxVarIndexInExp_dot_allVarsInBranch_dot_300 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_pafsOfProg :: Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int)
d_C_pafsOfProg x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> d_C_pafsOfFuncs (Curry_Prelude.d_C_filter d_C_isRelevantFunction x5 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_pafsOfProg x1002 x3250 x3500) (d_C_pafsOfProg x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_pafsOfProg z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_pafsOfProg x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_pafsOfFuncs :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int)
d_C_pafsOfFuncs x1 x3250 x3500 = Curry_Prelude.d_C_apply (d_C_mapUnion x3250 x3500) (Curry_Prelude.d_C_map d_C_pafsOfFunc x1 x3250 x3500) x3250 x3500

d_C_pafsOfFunc :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int)
d_C_pafsOfFunc x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_83 x6 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_pafsOfFunc x1002 x3250 x3500) (d_C_pafsOfFunc x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_pafsOfFunc z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_pafsOfFunc x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_pafsOfExpr :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int)
d_C_pafsOfExpr x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Lit x3) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> d_OP__case_82 x6 x5 x4 x3250 x3500
     (Curry_FlatCurry.C_Case x9 x10 x11) -> Curry_Prelude.d_C_apply (d_C_mapUnion x3250 x3500) (Curry_Prelude.d_C_map d_C_pafsOfExpr (Curry_Prelude.OP_Cons x10 (Curry_Prelude.d_C_map d_OP_pafsOfExpr_dot___hash_lambda11 x11 x3250 x3500)) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Let x12 x13) -> Curry_Prelude.d_C_apply (d_C_mapUnion x3250 x3500) (Curry_Prelude.d_C_map d_C_pafsOfExpr (Curry_Prelude.OP_Cons x13 (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x12 x3250 x3500)) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Free x14 x15) -> d_C_pafsOfExpr x15 x3250 x3500
     (Curry_FlatCurry.C_Or x16 x17) -> Curry_List.d_C_union (d_C_pafsOfExpr x16 x3250 x3500) (d_C_pafsOfExpr x17 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_pafsOfExpr x1002 x3250 x3500) (d_C_pafsOfExpr x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_pafsOfExpr z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_pafsOfExpr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_pafsOfExpr_dot___hash_lambda11 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP_pafsOfExpr_dot___hash_lambda11 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> x3
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_pafsOfExpr_dot___hash_lambda11 x1002 x3250 x3500) (d_OP_pafsOfExpr_dot___hash_lambda11 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_pafsOfExpr_dot___hash_lambda11 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_pafsOfExpr_dot___hash_lambda11 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_mapUnion :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_mapUnion x3250 x3500 = Curry_Prelude.d_C_foldr (acceptCs id Curry_List.d_C_union) Curry_Prelude.OP_List

nd_C_mapUnion :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)) (Curry_Prelude.OP_List t0)
nd_C_mapUnion x3000 x3250 x3500 = wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapDX id) (acceptCs id Curry_List.d_C_union)) Curry_Prelude.OP_List)

d_C_jscOfProg :: Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
d_C_jscOfProg x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> d_C_jscOfFuncs x5 x3250 x3500
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_jscOfProg x1002 x3250 x3500) (d_C_jscOfProg x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_jscOfProg z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_jscOfProg x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_jscOfFuncs :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
d_C_jscOfFuncs x1 x3250 x3500 = Curry_Prelude.d_C_apply (d_C_mapUnion x3250 x3500) (Curry_Prelude.d_C_map d_C_jscOfFunc x1 x3250 x3500) x3250 x3500

d_C_jscOfFunc :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
d_C_jscOfFunc x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_81 x6 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_jscOfFunc x1002 x3250 x3500) (d_C_jscOfFunc x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_jscOfFunc z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_jscOfFunc x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_jscOfExpr :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
d_C_jscOfExpr x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Lit x3) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> d_OP__case_80 x6 x4 x5 x3250 x3500
     (Curry_FlatCurry.C_Case x10 x11 x12) -> Curry_Prelude.d_C_apply (d_C_mapUnion x3250 x3500) (Curry_Prelude.d_C_map d_C_jscOfExpr (Curry_Prelude.OP_Cons x11 (Curry_Prelude.d_C_map d_OP_jscOfExpr_dot___hash_lambda15 x12 x3250 x3500)) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Let x13 x14) -> Curry_Prelude.d_C_apply (d_C_mapUnion x3250 x3500) (Curry_Prelude.d_C_map d_C_jscOfExpr (Curry_Prelude.OP_Cons x14 (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x13 x3250 x3500)) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Free x15 x16) -> d_C_jscOfExpr x16 x3250 x3500
     (Curry_FlatCurry.C_Or x17 x18) -> Curry_List.d_C_union (d_C_jscOfExpr x17 x3250 x3500) (d_C_jscOfExpr x18 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_jscOfExpr x1002 x3250 x3500) (d_C_jscOfExpr x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_jscOfExpr z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_jscOfExpr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_jscOfExpr_dot_getCurryFunc_dot_413 :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_jscOfExpr_dot_getCurryFunc_dot_413 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Comb x2 x3 x4) -> d_OP__case_76 x4 x3 x2 x3250 x3500
     (Curry_FlatCurry.C_Var x10) -> Curry_Prelude.C_Nothing
     (Curry_FlatCurry.C_Lit x11) -> Curry_Prelude.C_Nothing
     (Curry_FlatCurry.C_Let x12 x13) -> Curry_Prelude.C_Nothing
     (Curry_FlatCurry.C_Free x14 x15) -> Curry_Prelude.C_Nothing
     (Curry_FlatCurry.C_Or x16 x17) -> Curry_Prelude.C_Nothing
     (Curry_FlatCurry.C_Case x18 x19 x20) -> Curry_Prelude.C_Nothing
     (Curry_FlatCurry.C_Typed x21 x22) -> Curry_Prelude.C_Nothing
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_jscOfExpr_dot_getCurryFunc_dot_413 x1002 x3250 x3500) (d_OP_jscOfExpr_dot_getCurryFunc_dot_413 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_jscOfExpr_dot_getCurryFunc_dot_413 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_jscOfExpr_dot_getCurryFunc_dot_413 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_jscOfExpr_dot_fstFlatCurry_dot_413 :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP_jscOfExpr_dot_fstFlatCurry_dot_413 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Comb x2 x3 x4) -> d_OP__case_73 x4 x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_jscOfExpr_dot_fstFlatCurry_dot_413 x1002 x3250 x3500) (d_OP_jscOfExpr_dot_fstFlatCurry_dot_413 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_jscOfExpr_dot_fstFlatCurry_dot_413 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_jscOfExpr_dot_fstFlatCurry_dot_413 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_jscOfExpr_dot___hash_lambda13 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
d_OP_jscOfExpr_dot___hash_lambda13 x1 x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 Curry_Prelude.C_True) Curry_Prelude.OP_List

d_OP_jscOfExpr_dot___hash_lambda14 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
d_OP_jscOfExpr_dot___hash_lambda14 x1 x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 Curry_Prelude.C_False) Curry_Prelude.OP_List

d_OP_jscOfExpr_dot___hash_lambda15 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP_jscOfExpr_dot___hash_lambda15 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> x3
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_jscOfExpr_dot___hash_lambda15 x1002 x3250 x3500) (d_OP_jscOfExpr_dot___hash_lambda15 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_jscOfExpr_dot___hash_lambda15 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_jscOfExpr_dot___hash_lambda15 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_replaceJscOfProg :: Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_FlatCurry.C_Prog
d_C_replaceJscOfProg x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> Curry_FlatCurry.C_Prog x2 x3 x4 (Curry_Prelude.d_C_map d_C_replaceJscOfFunc x5 x3250 x3500) x6
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_replaceJscOfProg x1002 x3250 x3500) (d_C_replaceJscOfProg x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_replaceJscOfProg z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_replaceJscOfProg x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_replaceJscOfFunc :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_FlatCurry.C_FuncDecl
d_C_replaceJscOfFunc x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_70 x5 x4 x3 x2 x1 x6 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_replaceJscOfFunc x1002 x3250 x3500) (d_C_replaceJscOfFunc x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_replaceJscOfFunc z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_replaceJscOfFunc x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_replaceJscOfExpr :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_C_replaceJscOfExpr x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_FlatCurry.C_Var x2
     (Curry_FlatCurry.C_Lit x3) -> Curry_FlatCurry.C_Lit x3
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> d_OP__case_69 x6 x4 x5 x3250 x3500
     (Curry_FlatCurry.C_Case x10 x11 x12) -> Curry_FlatCurry.C_Case x10 (d_C_replaceJscOfExpr x11 x3250 x3500) (Curry_Prelude.d_C_map d_OP_replaceJscOfExpr_dot___hash_lambda19 x12 x3250 x3500)
     (Curry_FlatCurry.C_Let x13 x14) -> Curry_FlatCurry.C_Let (Curry_Prelude.d_C_map d_OP_replaceJscOfExpr_dot___hash_lambda20 x13 x3250 x3500) (d_C_replaceJscOfExpr x14 x3250 x3500)
     (Curry_FlatCurry.C_Free x15 x16) -> Curry_FlatCurry.C_Free x15 (d_C_replaceJscOfExpr x16 x3250 x3500)
     (Curry_FlatCurry.C_Or x17 x18) -> Curry_FlatCurry.C_Or (d_C_replaceJscOfExpr x17 x3250 x3500) (d_C_replaceJscOfExpr x18 x3250 x3500)
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_replaceJscOfExpr x1002 x3250 x3500) (d_C_replaceJscOfExpr x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_replaceJscOfExpr z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_replaceJscOfExpr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_replaceJscOfExpr_dot_isWCons_dot_460 :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_replaceJscOfExpr_dot_isWCons_dot_460 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Comb x2 x3 x4) -> d_OP__case_65 x3 x4 x2 x3250 x3500
     (Curry_FlatCurry.C_Var x11) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Lit x12) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Let x13 x14) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Free x15 x16) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Or x17 x18) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Case x19 x20 x21) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Typed x22 x23) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_replaceJscOfExpr_dot_isWCons_dot_460 x1002 x3250 x3500) (d_OP_replaceJscOfExpr_dot_isWCons_dot_460 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_replaceJscOfExpr_dot_isWCons_dot_460 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_replaceJscOfExpr_dot_isWCons_dot_460 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_replaceJscOfExpr_dot_wConsArity_dot_460 :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_replaceJscOfExpr_dot_wConsArity_dot_460 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Comb x2 x3 x4) -> d_OP__case_62 x4 x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_replaceJscOfExpr_dot_wConsArity_dot_460 x1002 x3250 x3500) (d_OP_replaceJscOfExpr_dot_wConsArity_dot_460 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_replaceJscOfExpr_dot_wConsArity_dot_460 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_replaceJscOfExpr_dot_wConsArity_dot_460 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_replaceJscOfExpr_dot_isDataCons_dot_460 :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_replaceJscOfExpr_dot_isDataCons_dot_460 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Comb x2 x3 x4) -> d_OP__case_60 x4 x2 x3250 x3500
     (Curry_FlatCurry.C_Var x10) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Lit x11) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Let x12 x13) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Free x14 x15) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Or x16 x17) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Case x18 x19 x20) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Typed x21 x22) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_replaceJscOfExpr_dot_isDataCons_dot_460 x1002 x3250 x3500) (d_OP_replaceJscOfExpr_dot_isDataCons_dot_460 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_replaceJscOfExpr_dot_isDataCons_dot_460 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_replaceJscOfExpr_dot_isDataCons_dot_460 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_replaceJscOfExpr_dot_dataConsName_dot_460 :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_replaceJscOfExpr_dot_dataConsName_dot_460 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Comb x2 x3 x4) -> x3
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_replaceJscOfExpr_dot_dataConsName_dot_460 x1002 x3250 x3500) (d_OP_replaceJscOfExpr_dot_dataConsName_dot_460 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_replaceJscOfExpr_dot_dataConsName_dot_460 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_replaceJscOfExpr_dot_dataConsName_dot_460 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_replaceJscOfExpr_dot_isJSTranslatable_dot_460 :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_replaceJscOfExpr_dot_isJSTranslatable_dot_460 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Comb x2 x3 x4) -> d_OP__case_57 x4 x2 x3250 x3500
     (Curry_FlatCurry.C_Var x10) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Lit x11) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Let x12 x13) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Free x14 x15) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Or x16 x17) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Case x18 x19 x20) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Typed x21 x22) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_replaceJscOfExpr_dot_isJSTranslatable_dot_460 x1002 x3250 x3500) (d_OP_replaceJscOfExpr_dot_isJSTranslatable_dot_460 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_replaceJscOfExpr_dot_isJSTranslatable_dot_460 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_replaceJscOfExpr_dot_isJSTranslatable_dot_460 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_replaceJscOfExpr_dot_replaceCurryFunc_dot_460 :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr
d_OP_replaceJscOfExpr_dot_replaceCurryFunc_dot_460 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Comb x2 x3 x4) -> Curry_Prelude.OP_Cons x1 (Curry_Prelude.OP_Cons (d_C_flatString (d_C_qname2JS x3 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List)
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_replaceJscOfExpr_dot_replaceCurryFunc_dot_460 x1002 x3250 x3500) (d_OP_replaceJscOfExpr_dot_replaceCurryFunc_dot_460 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_replaceJscOfExpr_dot_replaceCurryFunc_dot_460 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_replaceJscOfExpr_dot_replaceCurryFunc_dot_460 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_replaceJscOfExpr_dot___hash_lambda19 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_FlatCurry.C_BranchExpr
d_OP_replaceJscOfExpr_dot___hash_lambda19 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> Curry_FlatCurry.C_Branch x2 (d_C_replaceJscOfExpr x3 x3250 x3500)
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_replaceJscOfExpr_dot___hash_lambda19 x1002 x3250 x3500) (d_OP_replaceJscOfExpr_dot___hash_lambda19 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_replaceJscOfExpr_dot___hash_lambda19 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_replaceJscOfExpr_dot___hash_lambda19 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_replaceJscOfExpr_dot___hash_lambda20 :: Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr
d_OP_replaceJscOfExpr_dot___hash_lambda20 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.OP_Tuple2 x2 (d_C_replaceJscOfExpr x3 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_replaceJscOfExpr_dot___hash_lambda20 x1002 x3250 x3500) (d_OP_replaceJscOfExpr_dot___hash_lambda20 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_replaceJscOfExpr_dot___hash_lambda20 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_replaceJscOfExpr_dot___hash_lambda20 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_flatEmptyList :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_flatEmptyList x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Comb x2 x3 x4) -> d_OP__case_54 x3 x4 x2 x3250 x3500
     (Curry_FlatCurry.C_Var x42) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Lit x43) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Let x44 x45) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Free x46 x47) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Or x48 x49) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Case x50 x51 x52) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Typed x53 x54) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_flatEmptyList x1002 x3250 x3500) (d_C_flatEmptyList x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_flatEmptyList z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_flatEmptyList x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_flatString :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_C_flatString x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_FlatCurry.C_Comb Curry_FlatCurry.C_ConsCall (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List))) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_FlatCurry.C_Comb Curry_FlatCurry.C_ConsCall (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Lit (Curry_FlatCurry.C_Charc x2)) (Curry_Prelude.OP_Cons (d_C_flatString x3 x3250 x3500) Curry_Prelude.OP_List))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_flatString x1002 x3250 x3500) (d_C_flatString x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_flatString z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_flatString x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_uniqueDefsOfStats :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef))
d_C_uniqueDefsOfStats x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> x1
     (Curry_Prelude.OP_Cons x3 x4) -> d_C_uniqueDefsOfStats (d_C_uniqueDefsOfStat x1 x3 x3250 x3500) x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_uniqueDefsOfStats x1 x1002 x3250 x3500) (d_C_uniqueDefsOfStats x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_uniqueDefsOfStats x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_uniqueDefsOfStats x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_uniqueDefsOfStat :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_JavaScript.C_JSStat -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef))
d_C_uniqueDefsOfStat x1 x2 x3250 x3500 = case x2 of
     (Curry_JavaScript.C_JSAssign x3 x4) -> let
          x5 = d_C_uniqueDefsOfExp x1 x4 x3250 x3500
           in (d_OP__case_31 x5 x4 x3 x3250 x3500)
     (Curry_JavaScript.C_JSIf x21 x22 x23) -> d_C_uniqueDefsOfStats (d_C_uniqueDefsOfStats (d_C_uniqueDefsOfExp x1 x21 x3250 x3500) x22 x3250 x3500) x23 x3250 x3500
     (Curry_JavaScript.C_JSSwitch x24 x25) -> d_C_uniqueDefsOfStats (d_C_uniqueDefsOfExp x1 x24 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_uniqueDefsOfStat_dot_statsOf_dot_557 x3250 x3500) x25 x3250 x3500) x3250 x3500
     (Curry_JavaScript.C_JSPCall x26 x27) -> d_C_uniqueDefsOfExps x1 x27 x3250 x3500
     (Curry_JavaScript.C_JSReturn x28) -> d_C_uniqueDefsOfExp x1 x28 x3250 x3500
     (Curry_JavaScript.C_JSVarDecl x29) -> x1
     (Curry_JavaScript.Choice_C_JSStat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_uniqueDefsOfStat x1 x1002 x3250 x3500) (d_C_uniqueDefsOfStat x1 x1003 x3250 x3500)
     (Curry_JavaScript.Choices_C_JSStat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_uniqueDefsOfStat x1 z x3250 x3500) x1002
     (Curry_JavaScript.Guard_C_JSStat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_uniqueDefsOfStat x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_JavaScript.Fail_C_JSStat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_uniqueDefsOfStat_dot_rhs2vardef_dot_537 :: Curry_JavaScript.C_JSExp -> Cover -> ConstStore -> C_VarDef
d_OP_uniqueDefsOfStat_dot_rhs2vardef_dot_537 x1 x3250 x3500 = case x1 of
     (Curry_JavaScript.C_JSIVar x2) -> C_SimpleVar x2
     (Curry_JavaScript.C_JSString x3) -> C_ComplexDef x1 (Curry_Prelude.C_Int 0#)
     (Curry_JavaScript.C_JSInt x4) -> C_ComplexDef x1 (Curry_Prelude.C_Int 0#)
     (Curry_JavaScript.C_JSBool x5) -> C_ComplexDef x1 (Curry_Prelude.C_Int 0#)
     (Curry_JavaScript.C_JSIArrayIdx x6 x7) -> C_ComplexDef x1 (Curry_Prelude.C_Int 0#)
     (Curry_JavaScript.C_JSOp x8 x9 x10) -> C_ComplexDef x1 (Curry_Prelude.C_Int 0#)
     (Curry_JavaScript.C_JSFCall x11 x12) -> C_ComplexDef x1 (Curry_Prelude.C_Int 0#)
     (Curry_JavaScript.C_JSApply x13 x14) -> C_ComplexDef x1 (Curry_Prelude.C_Int 0#)
     (Curry_JavaScript.C_JSLambda x15 x16) -> C_ComplexDef x1 (Curry_Prelude.C_Int 0#)
     (Curry_JavaScript.Choice_C_JSExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_uniqueDefsOfStat_dot_rhs2vardef_dot_537 x1002 x3250 x3500) (d_OP_uniqueDefsOfStat_dot_rhs2vardef_dot_537 x1003 x3250 x3500)
     (Curry_JavaScript.Choices_C_JSExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_uniqueDefsOfStat_dot_rhs2vardef_dot_537 z x3250 x3500) x1002
     (Curry_JavaScript.Guard_C_JSExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_uniqueDefsOfStat_dot_rhs2vardef_dot_537 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_JavaScript.Fail_C_JSExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_uniqueDefsOfStat_dot___hash_lambda24 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_Prelude.C_Maybe C_VarDef -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef))
d_OP_uniqueDefsOfStat_dot___hash_lambda24 x1 x2 x3 x3250 x3500 = d_C_updateAssoc x1 Curry_Prelude.C_Nothing x2 x3250 x3500

d_OP_uniqueDefsOfStat_dot_statsOf_dot_557 :: Curry_JavaScript.C_JSBranch -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP_uniqueDefsOfStat_dot_statsOf_dot_557 x1 x3250 x3500 = case x1 of
     (Curry_JavaScript.C_JSCase x2 x3) -> x3
     (Curry_JavaScript.C_JSDefault x4) -> x4
     (Curry_JavaScript.Choice_C_JSBranch x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_uniqueDefsOfStat_dot_statsOf_dot_557 x1002 x3250 x3500) (d_OP_uniqueDefsOfStat_dot_statsOf_dot_557 x1003 x3250 x3500)
     (Curry_JavaScript.Choices_C_JSBranch x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_uniqueDefsOfStat_dot_statsOf_dot_557 z x3250 x3500) x1002
     (Curry_JavaScript.Guard_C_JSBranch x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_uniqueDefsOfStat_dot_statsOf_dot_557 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_JavaScript.Fail_C_JSBranch x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_uniqueDefsOfExp :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_JavaScript.C_JSExp -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef))
d_C_uniqueDefsOfExp x1 x2 x3250 x3500 = case x2 of
     (Curry_JavaScript.C_JSString x3) -> x1
     (Curry_JavaScript.C_JSInt x4) -> x1
     (Curry_JavaScript.C_JSBool x5) -> x1
     (Curry_JavaScript.C_JSIVar x6) -> Curry_Prelude.d_C_maybe x1 (Curry_Prelude.d_C_maybe x1 (d_OP_uniqueDefsOfExp_dot___hash_lambda25 x1 x6)) (Curry_Prelude.d_C_lookup x6 x1 x3250 x3500) x3250 x3500
     (Curry_JavaScript.C_JSIArrayIdx x7 x8) -> Curry_Prelude.d_C_maybe x1 (Curry_Prelude.d_C_maybe x1 (d_OP_uniqueDefsOfExp_dot___hash_lambda26 x1 x7)) (Curry_Prelude.d_C_lookup x7 x1 x3250 x3500) x3250 x3500
     (Curry_JavaScript.C_JSOp x9 x10 x11) -> d_C_uniqueDefsOfExp (d_C_uniqueDefsOfExp x1 x10 x3250 x3500) x11 x3250 x3500
     (Curry_JavaScript.C_JSFCall x12 x13) -> d_C_uniqueDefsOfExps x1 x13 x3250 x3500
     (Curry_JavaScript.C_JSApply x14 x15) -> d_C_uniqueDefsOfExp (d_C_uniqueDefsOfExp x1 x14 x3250 x3500) x15 x3250 x3500
     (Curry_JavaScript.C_JSLambda x16 x17) -> d_C_uniqueDefsOfStats x1 x17 x3250 x3500
     (Curry_JavaScript.Choice_C_JSExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_uniqueDefsOfExp x1 x1002 x3250 x3500) (d_C_uniqueDefsOfExp x1 x1003 x3250 x3500)
     (Curry_JavaScript.Choices_C_JSExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_uniqueDefsOfExp x1 z x3250 x3500) x1002
     (Curry_JavaScript.Guard_C_JSExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_uniqueDefsOfExp x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_JavaScript.Fail_C_JSExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_uniqueDefsOfExp_dot_incVarDef_dot_581 :: C_VarDef -> Cover -> ConstStore -> C_VarDef
d_OP_uniqueDefsOfExp_dot_incVarDef_dot_581 x1 x3250 x3500 = case x1 of
     (C_ComplexDef x2 x3) -> C_ComplexDef x2 (Curry_Prelude.d_OP_plus x3 (Curry_Prelude.C_Int 1#) x3250 x3500)
     (C_SimpleVar x4) -> C_SimpleVar x4
     (Choice_C_VarDef x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_uniqueDefsOfExp_dot_incVarDef_dot_581 x1002 x3250 x3500) (d_OP_uniqueDefsOfExp_dot_incVarDef_dot_581 x1003 x3250 x3500)
     (Choices_C_VarDef x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_uniqueDefsOfExp_dot_incVarDef_dot_581 z x3250 x3500) x1002
     (Guard_C_VarDef x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_uniqueDefsOfExp_dot_incVarDef_dot_581 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_VarDef x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_uniqueDefsOfExp_dot___hash_lambda25 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_Prelude.C_Int -> C_VarDef -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef))
d_OP_uniqueDefsOfExp_dot___hash_lambda25 x1 x2 x3 x3250 x3500 = d_C_updateAssoc x2 (Curry_Prelude.C_Just (d_OP_uniqueDefsOfExp_dot_incVarDef_dot_581 x3 x3250 x3500)) x1 x3250 x3500

d_OP_uniqueDefsOfExp_dot_newVarDef_dot_590 :: C_VarDef -> Cover -> ConstStore -> Curry_Prelude.C_Maybe C_VarDef
d_OP_uniqueDefsOfExp_dot_newVarDef_dot_590 x1 x3250 x3500 = case x1 of
     (C_SimpleVar x2) -> Curry_Prelude.C_Just (C_SimpleVar x2)
     (C_ComplexDef x3 x4) -> Curry_Prelude.C_Nothing
     (Choice_C_VarDef x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_uniqueDefsOfExp_dot_newVarDef_dot_590 x1002 x3250 x3500) (d_OP_uniqueDefsOfExp_dot_newVarDef_dot_590 x1003 x3250 x3500)
     (Choices_C_VarDef x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_uniqueDefsOfExp_dot_newVarDef_dot_590 z x3250 x3500) x1002
     (Guard_C_VarDef x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_uniqueDefsOfExp_dot_newVarDef_dot_590 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_VarDef x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_uniqueDefsOfExp_dot___hash_lambda26 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_Prelude.C_Int -> C_VarDef -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef))
d_OP_uniqueDefsOfExp_dot___hash_lambda26 x1 x2 x3 x3250 x3500 = d_C_updateAssoc x2 (d_OP_uniqueDefsOfExp_dot_newVarDef_dot_590 x3 x3250 x3500) x1 x3250 x3500

d_C_uniqueDefsOfExps :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_Prelude.OP_List Curry_JavaScript.C_JSExp -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef))
d_C_uniqueDefsOfExps x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> x1
     (Curry_Prelude.OP_Cons x3 x4) -> d_C_uniqueDefsOfExps (d_C_uniqueDefsOfExp x1 x3 x3250 x3500) x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_uniqueDefsOfExps x1 x1002 x3250 x3500) (d_C_uniqueDefsOfExps x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_uniqueDefsOfExps x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_uniqueDefsOfExps x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_updateAssoc :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> t1 -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1)
d_C_updateAssoc x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_30 x1 x5 x2 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_updateAssoc x1 x2 x1002 x3250 x3500) (d_C_updateAssoc x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_updateAssoc x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_updateAssoc x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_removeSingleVarsJSStatements :: Curry_Prelude.OP_List Curry_JavaScript.C_JSStat -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_C_removeSingleVarsJSStatements x1 x3250 x3500 = d_OP__case_28 x1 (d_C_optimizeSingleVars x3250 x3500) x3250 x3500

d_C_removeSingleVarsInStats :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_C_removeSingleVarsInStats x1 x2 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_removeSingleVarsInStat x1) x3250 x3500) x2 x3250 x3500

d_C_maybeReplaceVar :: Curry_Prelude.Curry t0 => t0 -> t0 -> C_VarDef -> Cover -> ConstStore -> t0
d_C_maybeReplaceVar x1 x2 x3 x3250 x3500 = case x3 of
     (C_SimpleVar x4) -> x1
     (C_ComplexDef x5 x6) -> d_OP__case_27 x6 x2 x1 (Curry_Prelude.d_OP_lt_eq x6 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500
     (Choice_C_VarDef x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_maybeReplaceVar x1 x2 x1002 x3250 x3500) (d_C_maybeReplaceVar x1 x2 x1003 x3250 x3500)
     (Choices_C_VarDef x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_maybeReplaceVar x1 x2 z x3250 x3500) x1002
     (Guard_C_VarDef x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_maybeReplaceVar x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_VarDef x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_removeSingleVarsInStat :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_JavaScript.C_JSStat -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_C_removeSingleVarsInStat x1 x2 x3250 x3500 = case x2 of
     (Curry_JavaScript.C_JSAssign x3 x4) -> let
          x5 = d_C_removeSingleVarsInExp x1 x4 x3250 x3500
           in (d_OP__case_26 x5 x1 x3 x3250 x3500)
     (Curry_JavaScript.C_JSIf x21 x22 x23) -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSIf (d_C_removeSingleVarsInExp x1 x21 x3250 x3500) (d_C_removeSingleVarsInStats x1 x22 x3250 x3500) (d_C_removeSingleVarsInStats x1 x23 x3250 x3500)) Curry_Prelude.OP_List
     (Curry_JavaScript.C_JSSwitch x24 x25) -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSSwitch (d_C_removeSingleVarsInExp x1 x24 x3250 x3500) (Curry_Prelude.d_C_map (d_OP_removeSingleVarsInStat_dot_removeInBranch_dot_638 x1) x25 x3250 x3500)) Curry_Prelude.OP_List
     (Curry_JavaScript.C_JSPCall x26 x27) -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSPCall x26 (Curry_Prelude.d_C_map (d_C_removeSingleVarsInExp x1) x27 x3250 x3500)) Curry_Prelude.OP_List
     (Curry_JavaScript.C_JSReturn x28) -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSReturn (d_C_removeSingleVarsInExp x1 x28 x3250 x3500)) Curry_Prelude.OP_List
     (Curry_JavaScript.C_JSVarDecl x29) -> Curry_Prelude.d_C_maybe (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) (Curry_Prelude.d_C_maybe (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) (d_C_maybeReplaceVar Curry_Prelude.OP_List (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List))) (Curry_Prelude.d_C_lookup x29 x1 x3250 x3500) x3250 x3500
     (Curry_JavaScript.Choice_C_JSStat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_removeSingleVarsInStat x1 x1002 x3250 x3500) (d_C_removeSingleVarsInStat x1 x1003 x3250 x3500)
     (Curry_JavaScript.Choices_C_JSStat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_removeSingleVarsInStat x1 z x3250 x3500) x1002
     (Curry_JavaScript.Guard_C_JSStat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_removeSingleVarsInStat x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_JavaScript.Fail_C_JSStat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_removeSingleVarsInStat_dot_removeInBranch_dot_638 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_JavaScript.C_JSBranch -> Cover -> ConstStore -> Curry_JavaScript.C_JSBranch
d_OP_removeSingleVarsInStat_dot_removeInBranch_dot_638 x1 x2 x3250 x3500 = case x2 of
     (Curry_JavaScript.C_JSCase x3 x4) -> Curry_JavaScript.C_JSCase x3 (d_C_removeSingleVarsInStats x1 x4 x3250 x3500)
     (Curry_JavaScript.C_JSDefault x5) -> Curry_JavaScript.C_JSDefault (d_C_removeSingleVarsInStats x1 x5 x3250 x3500)
     (Curry_JavaScript.Choice_C_JSBranch x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_removeSingleVarsInStat_dot_removeInBranch_dot_638 x1 x1002 x3250 x3500) (d_OP_removeSingleVarsInStat_dot_removeInBranch_dot_638 x1 x1003 x3250 x3500)
     (Curry_JavaScript.Choices_C_JSBranch x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_removeSingleVarsInStat_dot_removeInBranch_dot_638 x1 z x3250 x3500) x1002
     (Curry_JavaScript.Guard_C_JSBranch x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_removeSingleVarsInStat_dot_removeInBranch_dot_638 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_JavaScript.Fail_C_JSBranch x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_removeSingleVarsInExp :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_JavaScript.C_JSExp -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_C_removeSingleVarsInExp x1 x2 x3250 x3500 = case x2 of
     (Curry_JavaScript.C_JSString x3) -> Curry_JavaScript.C_JSString x3
     (Curry_JavaScript.C_JSInt x4) -> Curry_JavaScript.C_JSInt x4
     (Curry_JavaScript.C_JSBool x5) -> Curry_JavaScript.C_JSBool x5
     (Curry_JavaScript.C_JSIVar x6) -> Curry_Prelude.d_C_maybe (Curry_JavaScript.C_JSIVar x6) (Curry_Prelude.d_C_maybe (Curry_JavaScript.C_JSIVar x6) (d_OP_removeSingleVarsInExp_dot_replaceVar_dot_659 x1 x6)) (Curry_Prelude.d_C_lookup x6 x1 x3250 x3500) x3250 x3500
     (Curry_JavaScript.C_JSIArrayIdx x7 x8) -> Curry_Prelude.d_C_maybe (Curry_JavaScript.C_JSIArrayIdx x7 x8) (Curry_Prelude.d_C_maybe (Curry_JavaScript.C_JSIArrayIdx x7 x8) (d_OP_removeSingleVarsInExp_dot_replaceVar_dot_666 x8)) (Curry_Prelude.d_C_lookup x7 x1 x3250 x3500) x3250 x3500
     (Curry_JavaScript.C_JSOp x9 x10 x11) -> Curry_JavaScript.C_JSOp x9 (d_C_removeSingleVarsInExp x1 x10 x3250 x3500) (d_C_removeSingleVarsInExp x1 x11 x3250 x3500)
     (Curry_JavaScript.C_JSFCall x12 x13) -> Curry_JavaScript.C_JSFCall x12 (Curry_Prelude.d_C_map (d_C_removeSingleVarsInExp x1) x13 x3250 x3500)
     (Curry_JavaScript.C_JSApply x14 x15) -> Curry_JavaScript.C_JSApply (d_C_removeSingleVarsInExp x1 x14 x3250 x3500) (d_C_removeSingleVarsInExp x1 x15 x3250 x3500)
     (Curry_JavaScript.C_JSLambda x16 x17) -> Curry_JavaScript.C_JSLambda x16 (d_C_removeSingleVarsInStats x1 x17 x3250 x3500)
     (Curry_JavaScript.Choice_C_JSExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_removeSingleVarsInExp x1 x1002 x3250 x3500) (d_C_removeSingleVarsInExp x1 x1003 x3250 x3500)
     (Curry_JavaScript.Choices_C_JSExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_removeSingleVarsInExp x1 z x3250 x3500) x1002
     (Curry_JavaScript.Guard_C_JSExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_removeSingleVarsInExp x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_JavaScript.Fail_C_JSExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_removeSingleVarsInExp_dot_replaceVar_dot_659 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_Prelude.C_Int -> C_VarDef -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_OP_removeSingleVarsInExp_dot_replaceVar_dot_659 x1 x2 x3 x3250 x3500 = case x3 of
     (C_SimpleVar x4) -> d_C_removeSingleVarsInExp x1 (Curry_JavaScript.C_JSIVar x4) x3250 x3500
     (C_ComplexDef x5 x6) -> d_OP__case_25 x6 x2 x5 x1 (Curry_Prelude.d_OP_lt_eq x6 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500
     (Choice_C_VarDef x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_removeSingleVarsInExp_dot_replaceVar_dot_659 x1 x2 x1002 x3250 x3500) (d_OP_removeSingleVarsInExp_dot_replaceVar_dot_659 x1 x2 x1003 x3250 x3500)
     (Choices_C_VarDef x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_removeSingleVarsInExp_dot_replaceVar_dot_659 x1 x2 z x3250 x3500) x1002
     (Guard_C_VarDef x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_removeSingleVarsInExp_dot_replaceVar_dot_659 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_VarDef x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_removeSingleVarsInExp_dot_replaceVar_dot_666 :: Curry_Prelude.C_Int -> C_VarDef -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_OP_removeSingleVarsInExp_dot_replaceVar_dot_666 x1 x2 x3250 x3500 = case x2 of
     (C_SimpleVar x3) -> Curry_JavaScript.C_JSIArrayIdx x3 x1
     (C_ComplexDef x4 x5) -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'V'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Choice_C_VarDef x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_removeSingleVarsInExp_dot_replaceVar_dot_666 x1 x1002 x3250 x3500) (d_OP_removeSingleVarsInExp_dot_replaceVar_dot_666 x1 x1003 x3250 x3500)
     (Choices_C_VarDef x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_removeSingleVarsInExp_dot_replaceVar_dot_666 x1 z x3250 x3500) x1002
     (Guard_C_VarDef x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_removeSingleVarsInExp_dot_replaceVar_dot_666 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_VarDef x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_curry2js :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_curry2js x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurry x1 x3250 x3500) (wrapNX id nd_OP_curry2js_dot___hash_lambda28) x2000 x3250 x3500))

nd_OP_curry2js_dot___hash_lambda28 :: Curry_FlatCurry.C_Prog -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_curry2js_dot___hash_lambda28 x1 x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (Curry_Prelude.d_C_return (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id Curry_JavaScript.d_C_showJSFDecl) x2000 x3250 x3500) (nd_C_flatprog2JS x1 x2001 x3250 x3500) x2002 x3250 x3500))))))) x3250 x3500))

nd_C_transformWUI :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_transformWUI x1 x2 x3 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_mapIO (wrapDX id d_C_getAndTransformWUIConditions) x2000 x3250 x3500) (Curry_Prelude.OP_Cons x1 x2) x2001 x3250 x3500)))) (wrapNX id (nd_OP_transformWUI_dot___hash_lambda29 x2 x1 x3)) x2003 x3250 x3500)))))

nd_OP_transformWUI_dot___hash_lambda29 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_transformWUI_dot___hash_lambda29 x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2000 = leftSupply x2008
          x2007 = rightSupply x2008
           in (seq x2000 (seq x2007 (let
               x5 = Curry_Prelude.d_C_concat x4 x3250 x3500
               x6 = Curry_List.d_C_nub (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_fst) x5 x2000 x3250 x3500) x3250 x3500
                in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) (let
                    x2005 = leftSupply x2007
                    x2006 = rightSupply x2007
                     in (seq x2005 (seq x2006 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStr (let
                         x2004 = leftSupply x2005
                         x2003 = rightSupply x2005
                          in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (let
                              x2002 = leftSupply x2003
                              x2001 = rightSupply x2003
                               in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_concatMap (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_plus_plus)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) (wrapDX id d_C_showQName) x2001 x3250 x3500) x2002 x3250 x3500)))) x6 x2004 x3250 x3500)))) x3250 x3500) (Curry_Prelude.nd_OP_gt_gt_eq (d_C_fileExistsAndNewerThan x3 (Curry_FlatCurry.d_C_flatCurryFileName x2 x3250 x3500) x3250 x3500) (wrapNX id (nd_OP_transformWUI_dot___hash_lambda29_dot___hash_lambda30 x1 x6 x5 x2 x3)) x2006 x3250 x3500) x3250 x3500)))) x3250 x3500))))))

nd_OP_transformWUI_dot___hash_lambda29_dot___hash_lambda30 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_transformWUI_dot___hash_lambda29_dot___hash_lambda30 x1 x2 x3 x4 x5 x6 x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2003 = rightSupply x2006
           in (seq x2005 (seq x2003 (nd_OP__case_24 x3 x6 x5 x2 x1 x4 (Curry_Prelude.d_OP_ampersand_ampersand x6 (Curry_Prelude.d_C_not (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_or x2000 x3250 x3500) (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_snd) x3 x2001 x3250 x3500) x2002 x3250 x3500))))))) x3250 x3500) x3250 x3500) x2005 x3250 x3500)))))

d_C_getAndTransformWUIConditions :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool))
d_C_getAndTransformWUIConditions x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurry x1 x3250 x3500) (d_OP_getAndTransformWUIConditions_dot___hash_lambda31 x1) x3250 x3500

d_OP_getAndTransformWUIConditions_dot___hash_lambda31 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool))
d_OP_getAndTransformWUIConditions_dot___hash_lambda31 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_findFileInLoadPath (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) (d_OP_getAndTransformWUIConditions_dot___hash_lambda31_dot___hash_lambda32 x1 x2) x3250 x3500

d_OP_getAndTransformWUIConditions_dot___hash_lambda31_dot___hash_lambda32 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool))
d_OP_getAndTransformWUIConditions_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 x3250 x3500 = let
     x4 = d_C_jscOfProg x2 x3250 x3500
     x5 = Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt (d_OP__case_23 x4 x3 x5 x2 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_or x3250 x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x4 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return x4 x3250 x3500) x3250 x3500)

nd_C_generateJavaScript :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_generateJavaScript x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_CompactFlatCurry.d_C_computeCompactFlatCurry (Curry_Prelude.OP_Cons (Curry_CompactFlatCurry.C_InitFuncs x3) (Curry_Prelude.nd_C_map (wrapDX id (acceptCs id Curry_CompactFlatCurry.C_Import)) x2 x2000 x3250 x3500)) x1 x3250 x3500) (wrapNX id (nd_OP_generateJavaScript_dot___hash_lambda33 x4)) x2001 x3250 x3500)))) x3250 x3500))

nd_OP_generateJavaScript_dot___hash_lambda33 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_Prog -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_generateJavaScript_dot___hash_lambda33 x1 x2 x3000 x3250 x3500 = let
     x2012 = x3000
      in (seq x2012 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) (let
          x2003 = leftSupply x2012
          x2011 = rightSupply x2012
           in (seq x2003 (seq x2011 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_writeFile x1 (Curry_Prelude.d_OP_plus_plus (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id Curry_JavaScript.d_C_showJSFDecl) x2000 x3250 x3500) (nd_C_flatprog2JS x2 x2001 x3250 x3500) x2002 x3250 x3500))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (d_OP__case_22 (d_C_lazyStringConversion x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (let
               x2005 = leftSupply x2011
               x2010 = rightSupply x2011
                in (seq x2005 (seq x2010 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_C_readFile (Curry_Prelude.d_OP_plus_plus (Curry_Distribution.d_C_installDir x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))) x3250 x3500) x3250 x3500) (wrapDX id (Curry_Prelude.d_C_appendFile x1)) x2005 x3250 x3500) (let
                    x2006 = leftSupply x2010
                    x2009 = rightSupply x2010
                     in (seq x2006 (seq x2009 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_C_readFile (Curry_Prelude.d_OP_plus_plus (Curry_Distribution.d_C_installDir x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))) x3250 x3500) x3250 x3500) (wrapDX id (Curry_Prelude.d_C_appendFile x1)) x2006 x3250 x3500) (let
                         x2007 = leftSupply x2009
                         x2008 = rightSupply x2009
                          in (seq x2007 (seq x2008 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x2007 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_System.d_C_system) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '6'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '4'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '4'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))) x1 x3250 x3500) x2008 x3250 x3500) (Curry_Prelude.d_C_done x3250 x3500) x3250 x3500) x3250 x3500)))) x3250 x3500)))) x3250 x3500)))) x3250 x3500)))) x3250 x3500))

d_C_fileExistsAndNewerThan :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_fileExistsAndNewerThan x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x1 x3250 x3500) (d_OP_fileExistsAndNewerThan_dot___hash_lambda34 x1 x2) x3250 x3500

d_OP_fileExistsAndNewerThan_dot___hash_lambda34 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_fileExistsAndNewerThan_dot___hash_lambda34 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x2 x3250 x3500) (d_OP_fileExistsAndNewerThan_dot___hash_lambda34_dot___hash_lambda35 x1 x3 x2) x3250 x3500

d_OP_fileExistsAndNewerThan_dot___hash_lambda34_dot___hash_lambda35 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_fileExistsAndNewerThan_dot___hash_lambda34_dot___hash_lambda35 x1 x2 x3 x4 x3250 x3500 = d_OP__case_21 x4 x2 x1 x3 (Curry_Prelude.d_OP_ampersand_ampersand x2 x4 x3250 x3500) x3250 x3500

d_OP_fileExistsAndNewerThan_dot___hash_lambda34_dot___hash_lambda35_dot___hash_lambda36 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_fileExistsAndNewerThan_dot___hash_lambda34_dot___hash_lambda35_dot___hash_lambda36 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x1 x3250 x3500) (d_OP_fileExistsAndNewerThan_dot___hash_lambda34_dot___hash_lambda35_dot___hash_lambda36_dot___hash_lambda37 x2) x3250 x3500

d_OP_fileExistsAndNewerThan_dot___hash_lambda34_dot___hash_lambda35_dot___hash_lambda36_dot___hash_lambda37 :: Curry_Time.C_ClockTime -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_fileExistsAndNewerThan_dot___hash_lambda34_dot___hash_lambda35_dot___hash_lambda36_dot___hash_lambda37 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_OP_gt x2 x1 x3250 x3500) x3250 x3500

d_C_showQName :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showQName x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) x3) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showQName x1002 x3250 x3500) (d_C_showQName x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showQName z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showQName x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_main :: IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_main x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_System.d_C_getArgs x3250 x3500) (wrapNX id nd_OP_main_dot___hash_lambda38) x2000 x3250 x3500))

nd_OP_main_dot___hash_lambda38 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_main_dot___hash_lambda38 x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_20 x2 x1 x3 x2000 x3250 x3500))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_main_dot___hash_lambda38 x1002 x3000 x3250 x3500) (nd_OP_main_dot___hash_lambda38 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_main_dot___hash_lambda38 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_main_dot___hash_lambda38 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_mp :: IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_mp x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_curry2js (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))) x2000 x3250 x3500) (wrapDX id Curry_Prelude.d_C_putStrLn) x2001 x3250 x3500)))))

nd_C_mo :: IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_mo x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_curry2js (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))) x2000 x3250 x3500) (wrapDX id d_OP_mo_dot___hash_lambda40) x2001 x3250 x3500)))))

d_OP_mo_dot___hash_lambda40 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_mo_dot___hash_lambda40 x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_writeFile (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_System.d_C_system (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '6'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '4'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '4'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))) x3250 x3500) (Curry_Prelude.d_C_done x3250 x3500) x3250 x3500) x3250 x3500

nd_OP__case_20 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_20 x2 x1 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_curry2js x2 x2000 x3250 x3500) (wrapDX id Curry_Prelude.d_C_putStrLn) x2001 x3250 x3500)))))
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_19 x1 x4 x5 x2 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_20 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_19 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_19 x1 x4 x5 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (let
               x8 = x6
                in (nd_OP__case_18 x8 x1 x7 x4 x5 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char '-'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x1 x4 x5 x1002 x3000 x3250 x3500) (nd_OP__case_19 x1 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x1 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x1 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_18 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_18 x8 x1 x7 x4 x5 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_17 x1 x4 x5 x7 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x8 x1 x7 x4 x5 x1002 x3000 x3250 x3500) (nd_OP__case_18 x8 x1 x7 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x8 x1 x7 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x8 x1 x7 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_17 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_17 x1 x4 x5 x7 x3000 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (let
               x11 = x9
                in (nd_OP__case_16 x11 x1 x10 x4 x5 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char 'o'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x1 x4 x5 x1002 x3000 x3250 x3500) (nd_OP__case_17 x1 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x1 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x1 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_16 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_16 x11 x1 x10 x4 x5 x12 x3000 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_15 x1 x5 x4 x10 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x11 x1 x10 x4 x5 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char 'w'#) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x11 x1 x10 x4 x5 x1002 x3000 x3250 x3500) (nd_OP__case_16 x11 x1 x10 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x11 x1 x10 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x11 x1 x10 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_12 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_12 x11 x1 x10 x4 x5 x12 x3000 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_11 x1 x4 x5 x10 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x11 x1 x10 x4 x5 x1002 x3000 x3250 x3500) (nd_OP__case_12 x11 x1 x10 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x11 x1 x10 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x11 x1 x10 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_11 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_11 x1 x4 x5 x10 x3000 x3250 x3500 = case x10 of
     (Curry_Prelude.OP_Cons x18 x19) -> let
          x2000 = x3000
           in (seq x2000 (let
               x20 = x18
                in (nd_OP__case_10 x20 x1 x19 x4 x5 (Curry_Prelude.d_OP_eq_eq x20 (Curry_Prelude.C_Char 'u'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1 x4 x5 x1002 x3000 x3250 x3500) (nd_OP__case_11 x1 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x1 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_10 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_10 x20 x1 x19 x4 x5 x21 x3000 x3250 x3500 = case x21 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x1 x4 x5 x19 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x20 x1 x19 x4 x5 x1002 x3000 x3250 x3500) (nd_OP__case_10 x20 x1 x19 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x20 x1 x19 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x20 x1 x19 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_9 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_9 x1 x4 x5 x19 x3000 x3250 x3500 = case x19 of
     (Curry_Prelude.OP_Cons x21 x22) -> let
          x2000 = x3000
           in (seq x2000 (let
               x23 = x21
                in (nd_OP__case_8 x23 x1 x22 x4 x5 (Curry_Prelude.d_OP_eq_eq x23 (Curry_Prelude.C_Char 'i'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x4 x5 x1002 x3000 x3250 x3500) (nd_OP__case_9 x1 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_8 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_8 x23 x1 x22 x4 x5 x24 x3000 x3250 x3500 = case x24 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x1 x4 x5 x22 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x23 x1 x22 x4 x5 x1002 x3000 x3250 x3500) (nd_OP__case_8 x23 x1 x22 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x23 x1 x22 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x23 x1 x22 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_7 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_7 x1 x4 x5 x22 x3000 x3250 x3500 = case x22 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x5 x4 x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x36 x37) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1 x4 x5 x1002 x3000 x3250 x3500) (nd_OP__case_7 x1 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x1 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_6 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_6 x5 x4 x3000 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x24 x25) -> let
          x2000 = x3000
           in (seq x2000 (let
               x26 = x24
                in (nd_OP__case_5 x26 x4 x5 x25 (Curry_Prelude.d_OP_eq_eq x26 (Curry_Prelude.C_Char '-'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_C_transformWUI Curry_Prelude.OP_List x5 (Curry_Prelude.d_OP_plus_plus Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x5 x1002 x3000 x3250 x3500) (nd_OP__case_6 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_5 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_5 x26 x4 x5 x25 x27 x3000 x3250 x3500 = case x27 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x4 x5 x25 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_transformWUI x4 x5 (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x26 x4 x5 x25 x1002 x3000 x3250 x3500) (nd_OP__case_5 x26 x4 x5 x25 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x26 x4 x5 x25 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x26 x4 x5 x25 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_4 x4 x5 x25 x3000 x3250 x3500 = case x25 of
     (Curry_Prelude.OP_Cons x27 x28) -> let
          x2000 = x3000
           in (seq x2000 (let
               x29 = x27
                in (nd_OP__case_3 x29 x4 x5 x28 (Curry_Prelude.d_OP_eq_eq x29 (Curry_Prelude.C_Char 'o'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_C_transformWUI x4 x5 (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x4 x5 x1002 x3000 x3250 x3500) (nd_OP__case_4 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_3 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_3 x29 x4 x5 x28 x30 x3000 x3250 x3500 = case x30 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x4 x5 x28 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_transformWUI x4 x5 (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x29 x4 x5 x28 x1002 x3000 x3250 x3500) (nd_OP__case_3 x29 x4 x5 x28 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x29 x4 x5 x28 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x29 x4 x5 x28 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_2 x4 x5 x28 x3000 x3250 x3500 = case x28 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x4 x5 x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x34 x35) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_transformWUI x4 x5 (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x4 x5 x1002 x3000 x3250 x3500) (nd_OP__case_2 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_1 x4 x5 x3000 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x30 x31) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x4 x5 x30 x31 x2000 x3250 x3500))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_C_transformWUI x4 Curry_Prelude.OP_List (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_0 x4 x5 x30 x31 x3000 x3250 x3500 = case x31 of
     (Curry_Prelude.OP_Cons x32 x33) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_transformWUI x32 x33 x30 x2000 x3250 x3500))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_C_transformWUI x4 x5 (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x4 x5 x30 x1002 x3000 x3250 x3500) (nd_OP__case_0 x4 x5 x30 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x4 x5 x30 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x4 x5 x30 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_15 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_15 x1 x5 x4 x10 x3000 x3250 x3500 = case x10 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x1 x4 x5 x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x16 x17) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x1 x5 x4 x1002 x3000 x3250 x3500) (nd_OP__case_15 x1 x5 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x1 x5 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x1 x5 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_14 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_14 x1 x4 x5 x3000 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_13 x1 x4 x12 x13 x2000 x3250 x3500))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_14 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_13 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_13 x1 x4 x12 x13 x3000 x3250 x3500 = case x13 of
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_curry2js x12 x2000 x3250 x3500) (wrapDX id (Curry_Prelude.d_C_writeFile x4)) x2001 x3250 x3500)))))
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x1 x4 x12 x1002 x3000 x3250 x3500) (nd_OP__case_13 x1 x4 x12 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x1 x4 x12 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x1 x4 x12 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP__case_21 x4 x2 x1 x3 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x3 x3250 x3500) (d_OP_fileExistsAndNewerThan_dot___hash_lambda34_dot___hash_lambda35_dot___hash_lambda36 x1) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return Curry_Prelude.C_False x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x4 x2 x1 x3 x1002 x3250 x3500) (d_OP__case_21 x4 x2 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x4 x2 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x4 x2 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_22 x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1002 x3250 x3500) (d_OP__case_22 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_23 x4 x3 x5 x2 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStr (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_FlatCurry.d_C_writeFCY x5 (d_C_replaceJscOfProg x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_putStrLn (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x4 x3 x5 x2 x1 x1002 x3250 x3500) (d_OP__case_23 x4 x3 x5 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x4 x3 x5 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x4 x3 x5 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_24 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_24 x3 x6 x5 x2 x1 x4 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_putStrLn (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))))))))))))))))))) x3250 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_generateJavaScript x4 x1 x2 x5 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x3 x6 x5 x2 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_24 x3 x6 x5 x2 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x3 x6 x5 x2 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x3 x6 x5 x2 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_JavaScript.C_JSExp -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_OP__case_25 x6 x2 x5 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_removeSingleVarsInExp x1 x5 x3250 x3500
     Curry_Prelude.C_False -> Curry_JavaScript.C_JSIVar x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x6 x2 x5 x1 x1002 x3250 x3500) (d_OP__case_25 x6 x2 x5 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x6 x2 x5 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x6 x2 x5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: Curry_JavaScript.C_JSExp -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_JavaScript.C_JSExp -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP__case_26 x5 x1 x3 x3250 x3500 = case x3 of
     (Curry_JavaScript.C_JSIVar x6) -> Curry_Prelude.d_C_maybe (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign x3 x5) Curry_Prelude.OP_List) (Curry_Prelude.d_C_maybe (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign x3 x5) Curry_Prelude.OP_List) (d_C_maybeReplaceVar Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign x3 x5) Curry_Prelude.OP_List))) (Curry_Prelude.d_C_lookup x6 x1 x3250 x3500) x3250 x3500
     (Curry_JavaScript.C_JSString x7) -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (d_C_removeSingleVarsInExp x1 x3 x3250 x3500) x5) Curry_Prelude.OP_List
     (Curry_JavaScript.C_JSInt x8) -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (d_C_removeSingleVarsInExp x1 x3 x3250 x3500) x5) Curry_Prelude.OP_List
     (Curry_JavaScript.C_JSBool x9) -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (d_C_removeSingleVarsInExp x1 x3 x3250 x3500) x5) Curry_Prelude.OP_List
     (Curry_JavaScript.C_JSIArrayIdx x10 x11) -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (d_C_removeSingleVarsInExp x1 x3 x3250 x3500) x5) Curry_Prelude.OP_List
     (Curry_JavaScript.C_JSOp x12 x13 x14) -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (d_C_removeSingleVarsInExp x1 x3 x3250 x3500) x5) Curry_Prelude.OP_List
     (Curry_JavaScript.C_JSFCall x15 x16) -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (d_C_removeSingleVarsInExp x1 x3 x3250 x3500) x5) Curry_Prelude.OP_List
     (Curry_JavaScript.C_JSApply x17 x18) -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (d_C_removeSingleVarsInExp x1 x3 x3250 x3500) x5) Curry_Prelude.OP_List
     (Curry_JavaScript.C_JSLambda x19 x20) -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (d_C_removeSingleVarsInExp x1 x3 x3250 x3500) x5) Curry_Prelude.OP_List
     (Curry_JavaScript.Choice_C_JSExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x5 x1 x1002 x3250 x3500) (d_OP__case_26 x5 x1 x1003 x3250 x3500)
     (Curry_JavaScript.Choices_C_JSExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x5 x1 z x3250 x3500) x1002
     (Curry_JavaScript.Guard_C_JSExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_JavaScript.Fail_C_JSExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> t0 -> t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> t0
d_OP__case_27 x6 x2 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x6 x2 x1 x1002 x3250 x3500) (d_OP__case_27 x6 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x6 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x6 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: Curry_Prelude.OP_List Curry_JavaScript.C_JSStat -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP__case_28 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar_hash_hash (d_C_removeSingleVarsInStats (d_C_uniqueDefsOfStats Curry_Prelude.OP_List x1 x3250 x3500)) x1 x3250 x3500
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x1 x1002 x3250 x3500) (d_OP__case_28 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_30 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> t1 -> Curry_Prelude.OP_Tuple2 t0 t1 -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1)
d_OP__case_30 x1 x5 x2 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_29 x6 x1 x5 x2 x7 (Curry_Prelude.d_OP_eq_eq x1 x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x1 x5 x2 x1002 x3250 x3500) (d_OP__case_30 x1 x5 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x1 x5 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x1 x5 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_29 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> t1 -> t1 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1)
d_OP__case_29 x6 x1 x5 x2 x7 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 x2) x5
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 x7) (d_C_updateAssoc x1 x2 x5 x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x6 x1 x5 x2 x7 x1002 x3250 x3500) (d_OP__case_29 x6 x1 x5 x2 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x6 x1 x5 x2 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x6 x1 x5 x2 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_31 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef)) -> Curry_JavaScript.C_JSExp -> Curry_JavaScript.C_JSExp -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_VarDef))
d_OP__case_31 x5 x4 x3 x3250 x3500 = case x3 of
     (Curry_JavaScript.C_JSIVar x6) -> Curry_Prelude.d_C_maybe (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 (Curry_Prelude.C_Just (d_OP_uniqueDefsOfStat_dot_rhs2vardef_dot_537 x4 x3250 x3500))) x5) (d_OP_uniqueDefsOfStat_dot___hash_lambda24 x6 x5) (Curry_Prelude.d_C_lookup x6 x5 x3250 x3500) x3250 x3500
     (Curry_JavaScript.C_JSString x7) -> d_C_uniqueDefsOfExp x5 x3 x3250 x3500
     (Curry_JavaScript.C_JSInt x8) -> d_C_uniqueDefsOfExp x5 x3 x3250 x3500
     (Curry_JavaScript.C_JSBool x9) -> d_C_uniqueDefsOfExp x5 x3 x3250 x3500
     (Curry_JavaScript.C_JSIArrayIdx x10 x11) -> d_C_uniqueDefsOfExp x5 x3 x3250 x3500
     (Curry_JavaScript.C_JSOp x12 x13 x14) -> d_C_uniqueDefsOfExp x5 x3 x3250 x3500
     (Curry_JavaScript.C_JSFCall x15 x16) -> d_C_uniqueDefsOfExp x5 x3 x3250 x3500
     (Curry_JavaScript.C_JSApply x17 x18) -> d_C_uniqueDefsOfExp x5 x3 x3250 x3500
     (Curry_JavaScript.C_JSLambda x19 x20) -> d_C_uniqueDefsOfExp x5 x3 x3250 x3500
     (Curry_JavaScript.Choice_C_JSExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x5 x4 x1002 x3250 x3500) (d_OP__case_31 x5 x4 x1003 x3250 x3500)
     (Curry_JavaScript.Choices_C_JSExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x5 x4 z x3250 x3500) x1002
     (Curry_JavaScript.Guard_C_JSExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_JavaScript.Fail_C_JSExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_54 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_CombType -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_54 x3 x4 x2 x3250 x3500 = case x2 of
     Curry_FlatCurry.C_ConsCall -> d_OP__case_53 x4 x3 x3250 x3500
     Curry_FlatCurry.C_FuncCall -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_FuncPartCall x40) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_ConsPartCall x41) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_CombType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_54 x3 x4 x1002 x3250 x3500) (d_OP__case_54 x3 x4 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_CombType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_54 x3 x4 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_CombType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_54 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CombType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_53 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_53 x4 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_52 x6 x4 x5 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_53 x4 x1002 x3250 x3500) (d_OP__case_53 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_53 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_53 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_52 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_52 x6 x4 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x7 x8) -> let
          x9 = x7
           in (d_OP__case_51 x9 x8 x6 x4 (Curry_Prelude.d_OP_eq_eq x9 (Curry_Prelude.C_Char 'P'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_52 x6 x4 x1002 x3250 x3500) (d_OP__case_52 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_52 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_52 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_51 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_51 x9 x8 x6 x4 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> d_OP__case_50 x6 x4 x8 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_51 x9 x8 x6 x4 x1002 x3250 x3500) (d_OP__case_51 x9 x8 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_51 x9 x8 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_51 x9 x8 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_50 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_50 x6 x4 x8 x3250 x3500 = case x8 of
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x12 = x10
           in (d_OP__case_49 x12 x11 x6 x4 (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.C_Char 'r'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_50 x6 x4 x1002 x3250 x3500) (d_OP__case_50 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_50 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_50 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_49 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_49 x12 x11 x6 x4 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> d_OP__case_48 x6 x4 x11 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_49 x12 x11 x6 x4 x1002 x3250 x3500) (d_OP__case_49 x12 x11 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_49 x12 x11 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_49 x12 x11 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_48 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_48 x6 x4 x11 x3250 x3500 = case x11 of
     (Curry_Prelude.OP_Cons x13 x14) -> let
          x15 = x13
           in (d_OP__case_47 x15 x14 x6 x4 (Curry_Prelude.d_OP_eq_eq x15 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_48 x6 x4 x1002 x3250 x3500) (d_OP__case_48 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_48 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_48 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_47 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_47 x15 x14 x6 x4 x16 x3250 x3500 = case x16 of
     Curry_Prelude.C_True -> d_OP__case_46 x6 x4 x14 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_47 x15 x14 x6 x4 x1002 x3250 x3500) (d_OP__case_47 x15 x14 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_47 x15 x14 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_47 x15 x14 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_46 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_46 x6 x4 x14 x3250 x3500 = case x14 of
     (Curry_Prelude.OP_Cons x16 x17) -> let
          x18 = x16
           in (d_OP__case_45 x18 x17 x6 x4 (Curry_Prelude.d_OP_eq_eq x18 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_46 x6 x4 x1002 x3250 x3500) (d_OP__case_46 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_46 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_46 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_45 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_45 x18 x17 x6 x4 x19 x3250 x3500 = case x19 of
     Curry_Prelude.C_True -> d_OP__case_44 x6 x4 x17 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x18 x17 x6 x4 x1002 x3250 x3500) (d_OP__case_45 x18 x17 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 x18 x17 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x18 x17 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_44 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_44 x6 x4 x17 x3250 x3500 = case x17 of
     (Curry_Prelude.OP_Cons x19 x20) -> let
          x21 = x19
           in (d_OP__case_43 x21 x20 x6 x4 (Curry_Prelude.d_OP_eq_eq x21 (Curry_Prelude.C_Char 'u'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x6 x4 x1002 x3250 x3500) (d_OP__case_44 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_43 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_43 x21 x20 x6 x4 x22 x3250 x3500 = case x22 of
     Curry_Prelude.C_True -> d_OP__case_42 x6 x4 x20 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x21 x20 x6 x4 x1002 x3250 x3500) (d_OP__case_43 x21 x20 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x21 x20 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x21 x20 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_42 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_42 x6 x4 x20 x3250 x3500 = case x20 of
     (Curry_Prelude.OP_Cons x22 x23) -> let
          x24 = x22
           in (d_OP__case_41 x24 x23 x6 x4 (Curry_Prelude.d_OP_eq_eq x24 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x6 x4 x1002 x3250 x3500) (d_OP__case_42 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_41 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_41 x24 x23 x6 x4 x25 x3250 x3500 = case x25 of
     Curry_Prelude.C_True -> d_OP__case_40 x6 x4 x23 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x24 x23 x6 x4 x1002 x3250 x3500) (d_OP__case_41 x24 x23 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x24 x23 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x24 x23 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_40 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_40 x6 x4 x23 x3250 x3500 = case x23 of
     (Curry_Prelude.OP_Cons x25 x26) -> let
          x27 = x25
           in (d_OP__case_39 x27 x26 x6 x4 (Curry_Prelude.d_OP_eq_eq x27 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x6 x4 x1002 x3250 x3500) (d_OP__case_40 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_39 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_39 x27 x26 x6 x4 x28 x3250 x3500 = case x28 of
     Curry_Prelude.C_True -> d_OP__case_38 x6 x4 x26 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x27 x26 x6 x4 x1002 x3250 x3500) (d_OP__case_39 x27 x26 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x27 x26 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x27 x26 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_38 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_38 x6 x4 x26 x3250 x3500 = case x26 of
     Curry_Prelude.OP_List -> d_OP__case_37 x4 x6 x3250 x3500
     (Curry_Prelude.OP_Cons x38 x39) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x6 x4 x1002 x3250 x3500) (d_OP__case_38 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_37 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_37 x4 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x28 x29) -> let
          x30 = x28
           in (d_OP__case_36 x30 x29 x4 (Curry_Prelude.d_OP_eq_eq x30 (Curry_Prelude.C_Char '['#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x4 x1002 x3250 x3500) (d_OP__case_37 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_36 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_36 x30 x29 x4 x31 x3250 x3500 = case x31 of
     Curry_Prelude.C_True -> d_OP__case_35 x4 x29 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x30 x29 x4 x1002 x3250 x3500) (d_OP__case_36 x30 x29 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x30 x29 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x30 x29 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_35 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_35 x4 x29 x3250 x3500 = case x29 of
     (Curry_Prelude.OP_Cons x31 x32) -> let
          x33 = x31
           in (d_OP__case_34 x33 x32 x4 (Curry_Prelude.d_OP_eq_eq x33 (Curry_Prelude.C_Char ']'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x4 x1002 x3250 x3500) (d_OP__case_35 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_34 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_34 x33 x32 x4 x34 x3250 x3500 = case x34 of
     Curry_Prelude.C_True -> d_OP__case_33 x4 x32 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x33 x32 x4 x1002 x3250 x3500) (d_OP__case_34 x33 x32 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x33 x32 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x33 x32 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_33 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_33 x4 x32 x3250 x3500 = case x32 of
     Curry_Prelude.OP_List -> d_OP__case_32 x4 x3250 x3500
     (Curry_Prelude.OP_Cons x36 x37) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x4 x1002 x3250 x3500) (d_OP__case_33 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_32 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_32 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_True
     (Curry_Prelude.OP_Cons x34 x35) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x1002 x3250 x3500) (d_OP__case_32 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_57 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_CombType -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_57 x4 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_FuncPartCall x5) -> let
          x6 = x5
           in (d_OP__case_56 x6 x4 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500)
     Curry_FlatCurry.C_FuncCall -> Curry_Prelude.C_False
     Curry_FlatCurry.C_ConsCall -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_ConsPartCall x9) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_CombType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_57 x4 x1002 x3250 x3500) (d_OP__case_57 x4 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_CombType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_57 x4 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_CombType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_57 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CombType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_56 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_56 x6 x4 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_OP__case_55 x4 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_56 x6 x4 x1002 x3250 x3500) (d_OP__case_56 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_56 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_56 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_55 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_55 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_True
     (Curry_Prelude.OP_Cons x7 x8) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_55 x1002 x3250 x3500) (d_OP__case_55 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_55 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_55 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_60 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_CombType -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_60 x4 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_ConsPartCall x5) -> let
          x6 = x5
           in (d_OP__case_59 x6 x4 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.C_Int 3#) x3250 x3500) x3250 x3500)
     Curry_FlatCurry.C_FuncCall -> Curry_Prelude.C_False
     Curry_FlatCurry.C_ConsCall -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_FuncPartCall x9) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_CombType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_60 x4 x1002 x3250 x3500) (d_OP__case_60 x4 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_CombType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_60 x4 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_CombType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_60 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CombType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_59 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_59 x6 x4 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_OP__case_58 x4 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_59 x6 x4 x1002 x3250 x3500) (d_OP__case_59 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_59 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_59 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_58 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_58 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_True
     (Curry_Prelude.OP_Cons x7 x8) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_58 x1002 x3250 x3500) (d_OP__case_58 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_58 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_58 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_62 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_62 x4 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_61 x6 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_62 x4 x1002 x3250 x3500) (d_OP__case_62 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_62 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_62 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_61 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_61 x6 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_fst (Curry_Maybe.d_C_fromJust (Curry_ReadNumeric.d_C_readNat (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 5#) x6 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_61 x6 x1002 x3250 x3500) (d_OP__case_61 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_61 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_61 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_65 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_CombType -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_65 x3 x4 x2 x3250 x3500 = case x2 of
     Curry_FlatCurry.C_FuncCall -> d_OP__case_64 x4 x3 x3250 x3500
     Curry_FlatCurry.C_ConsCall -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_FuncPartCall x9) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_ConsPartCall x10) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_CombType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_65 x3 x4 x1002 x3250 x3500) (d_OP__case_65 x3 x4 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_CombType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_65 x3 x4 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_CombType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_65 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CombType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_64 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_64 x4 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_63 x6 x5 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_64 x4 x1002 x3250 x3500) (d_OP__case_64 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_64 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_64 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_63 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_63 x6 x5 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x5 (d_C_wuiModName x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 5#) x6 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x7 x8) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_63 x6 x5 x1002 x3250 x3500) (d_OP__case_63 x6 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_63 x6 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_63 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_69 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_CombType -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP__case_69 x6 x4 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> d_OP__case_68 x6 x8 x7 x4 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 Curry_FlatCurry.C_FuncCall x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x7 (d_C_wuiModName x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) Curry_Prelude.OP_List))))))))))))))) x3250 x3500) (d_OP_replaceJscOfExpr_dot_isJSTranslatable_dot_460 (Curry_Prelude.d_OP_bang_bang x6 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_69 x6 x4 x1002 x3250 x3500) (d_OP__case_69 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_69 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_69 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_68 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_CombType -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP__case_68 x6 x8 x7 x4 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_FlatCurry.C_Comb x4 (Curry_Prelude.OP_Tuple2 x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_Prelude.OP_Cons (d_C_replaceJscOfExpr (Curry_Prelude.d_C_head x6 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Comb Curry_FlatCurry.C_ConsCall (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)))) (d_OP_replaceJscOfExpr_dot_replaceCurryFunc_dot_460 (Curry_Prelude.d_OP_bang_bang x6 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List))
     Curry_Prelude.C_False -> d_OP__case_67 x6 x8 x7 x4 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 Curry_FlatCurry.C_FuncCall x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x7 (d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (d_OP_replaceJscOfExpr_dot_isWCons_dot_460 (Curry_Prelude.d_OP_bang_bang x6 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500) (d_OP_replaceJscOfExpr_dot_isDataCons_dot_460 (Curry_Prelude.d_OP_bang_bang x6 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_68 x6 x8 x7 x4 x1002 x3250 x3500) (d_OP__case_68 x6 x8 x7 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_68 x6 x8 x7 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_68 x6 x8 x7 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_67 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_CombType -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP__case_67 x6 x8 x7 x4 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x9 = d_OP_replaceJscOfExpr_dot_wConsArity_dot_460 (Curry_Prelude.d_OP_bang_bang x6 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500
           in (Curry_FlatCurry.C_Comb x4 (Curry_Prelude.OP_Tuple2 x7 x8) (Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Comb (Curry_FlatCurry.C_FuncPartCall (Curry_Prelude.d_OP_plus x9 (Curry_Prelude.C_Int 1#) x3250 x3500)) (Curry_Prelude.OP_Tuple2 (d_C_wuiModName x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x9 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500)) (Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Comb Curry_FlatCurry.C_ConsCall (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Comb (Curry_FlatCurry.C_FuncPartCall (Curry_Prelude.C_Int 1#)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.OP_Cons (d_C_flatString (d_C_consQName2JS (d_OP_replaceJscOfExpr_dot_dataConsName_dot_460 (Curry_Prelude.d_OP_bang_bang x6 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) Curry_Prelude.OP_List)) Curry_Prelude.OP_List)) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_bang_bang x6 (Curry_Prelude.C_Int 1#) x3250 x3500) Curry_Prelude.OP_List)))
     Curry_Prelude.C_False -> d_OP__case_66 x6 x8 x7 x4 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_67 x6 x8 x7 x4 x1002 x3250 x3500) (d_OP__case_67 x6 x8 x7 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_67 x6 x8 x7 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_67 x6 x8 x7 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_66 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_CombType -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP__case_66 x6 x8 x7 x4 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_FlatCurry.C_Comb x4 (Curry_Prelude.OP_Tuple2 x7 x8) (Curry_Prelude.d_C_map d_C_replaceJscOfExpr x6 x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_66 x6 x8 x7 x4 x1002 x3250 x3500) (d_OP__case_66 x6 x8 x7 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_66 x6 x8 x7 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_66 x6 x8 x7 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_70 :: Curry_FlatCurry.C_TypeExpr -> Curry_FlatCurry.C_Visibility -> Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_FuncDecl -> Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_FlatCurry.C_FuncDecl
d_OP__case_70 x5 x4 x3 x2 x1 x6 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_External x7) -> x1
     (Curry_FlatCurry.C_Rule x8 x9) -> Curry_FlatCurry.C_Func x2 x3 x4 x5 (Curry_FlatCurry.C_Rule x8 (d_C_replaceJscOfExpr x9 x3250 x3500))
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_70 x5 x4 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_70 x5 x4 x3 x2 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_70 x5 x4 x3 x2 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_70 x5 x4 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_73 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP__case_73 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP__case_72 x5 x6 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_73 x1002 x3250 x3500) (d_OP__case_73 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_73 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_73 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_72 :: Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP__case_72 x5 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x7 x8) -> d_OP__case_71 x5 x8 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_72 x5 x1002 x3250 x3500) (d_OP__case_72 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_72 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_72 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_71 :: Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP__case_71 x5 x8 x3250 x3500 = case x8 of
     Curry_Prelude.OP_List -> x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_71 x5 x1002 x3250 x3500) (d_OP__case_71 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_71 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_71 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_76 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_CombType -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_76 x4 x3 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_FuncPartCall x5) -> let
          x6 = x5
           in (d_OP__case_75 x6 x4 x3 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500)
     Curry_FlatCurry.C_FuncCall -> Curry_Prelude.C_Nothing
     Curry_FlatCurry.C_ConsCall -> Curry_Prelude.C_Nothing
     (Curry_FlatCurry.C_ConsPartCall x9) -> Curry_Prelude.C_Nothing
     (Curry_FlatCurry.Choice_C_CombType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_76 x4 x3 x1002 x3250 x3500) (d_OP__case_76 x4 x3 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_CombType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_76 x4 x3 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_CombType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_76 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CombType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_75 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_75 x6 x4 x3 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_OP__case_74 x3 x4 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_75 x6 x4 x3 x1002 x3250 x3500) (d_OP__case_75 x6 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_75 x6 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_75 x6 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_74 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_74 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Just x3
     (Curry_Prelude.OP_Cons x7 x8) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_74 x3 x1002 x3250 x3500) (d_OP__case_74 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_74 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_74 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_80 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_CombType -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
d_OP__case_80 x6 x4 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> let
          x9 = Curry_Prelude.d_C_apply (d_C_mapUnion x3250 x3500) (Curry_Prelude.d_C_map d_C_jscOfExpr x6 x3250 x3500) x3250 x3500
           in (d_OP__case_79 x8 x7 x4 x9 x6 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 Curry_FlatCurry.C_FuncCall x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x7 (d_C_wuiModName x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) Curry_Prelude.OP_List))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_80 x6 x4 x1002 x3250 x3500) (d_OP__case_80 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_80 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_80 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_79 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_CombType -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
d_OP__case_79 x8 x7 x4 x9 x6 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_List.d_C_union (Curry_Prelude.d_C_maybe Curry_Prelude.OP_List d_OP_jscOfExpr_dot___hash_lambda13 (d_OP_jscOfExpr_dot_getCurryFunc_dot_413 (Curry_Prelude.d_OP_bang_bang x6 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x3250 x3500) x9 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_78 x8 x7 x4 x9 x6 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 Curry_FlatCurry.C_FuncCall x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x7 (d_C_wuiModName x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_79 x8 x7 x4 x9 x6 x1002 x3250 x3500) (d_OP__case_79 x8 x7 x4 x9 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_79 x8 x7 x4 x9 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_79 x8 x7 x4 x9 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_78 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_CombType -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
d_OP__case_78 x8 x7 x4 x9 x6 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_List.d_C_union (Curry_Prelude.d_C_maybe Curry_Prelude.OP_List d_OP_jscOfExpr_dot___hash_lambda14 (d_OP_jscOfExpr_dot_getCurryFunc_dot_413 (d_OP_jscOfExpr_dot_fstFlatCurry_dot_413 (Curry_Prelude.d_OP_bang_bang x6 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x9 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_77 x9 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_78 x8 x7 x4 x9 x6 x1002 x3250 x3500) (d_OP__case_78 x8 x7 x4 x9 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_78 x8 x7 x4 x9 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_78 x8 x7 x4 x9 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_77 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
d_OP__case_77 x9 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> x9
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_77 x9 x1002 x3250 x3500) (d_OP__case_77 x9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_77 x9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_77 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_81 :: Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
d_OP__case_81 x6 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_External x7) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Rule x8 x9) -> d_C_jscOfExpr x9 x3250 x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_81 x1002 x3250 x3500) (d_OP__case_81 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_81 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_81 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_82 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_CombType -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int)
d_OP__case_82 x6 x5 x4 x3250 x3500 = case x4 of
     Curry_FlatCurry.C_FuncCall -> Curry_Prelude.d_C_apply (d_C_mapUnion x3250 x3500) (Curry_Prelude.d_C_map d_C_pafsOfExpr x6 x3250 x3500) x3250 x3500
     Curry_FlatCurry.C_ConsCall -> Curry_Prelude.d_C_apply (d_C_mapUnion x3250 x3500) (Curry_Prelude.d_C_map d_C_pafsOfExpr x6 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_ConsPartCall x7) -> Curry_Prelude.d_C_apply (d_C_mapUnion x3250 x3500) (Curry_Prelude.d_C_map d_C_pafsOfExpr x6 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_FuncPartCall x8) -> Curry_List.d_C_union (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x5 (Curry_Prelude.d_OP_plus x8 (Curry_Prelude.d_C_length x6 x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List) (Curry_Prelude.d_C_apply (d_C_mapUnion x3250 x3500) (Curry_Prelude.d_C_map d_C_pafsOfExpr x6 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_CombType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_82 x6 x5 x1002 x3250 x3500) (d_OP__case_82 x6 x5 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_CombType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_82 x6 x5 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_CombType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_82 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CombType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_83 :: Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int)
d_OP__case_83 x6 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_External x7) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Rule x8 x9) -> d_C_pafsOfExpr x9 x3250 x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_83 x1002 x3250 x3500) (d_OP__case_83 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_83 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_83 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_84 :: Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP__case_84 x3 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Pattern x4 x5) -> Curry_Prelude.d_OP_plus_plus x5 (d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 x3 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_LPattern x6) -> d_OP_maxVarIndexInExp_dot_allVarsInExp_dot_300 x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_84 x3 x1002 x3250 x3500) (d_OP__case_84 x3 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_84 x3 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_84 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_85 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_85 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 0#
     Curry_Prelude.C_False -> Curry_Integer.d_C_maxlist x2 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_85 x2 x1002 x3250 x3500) (d_OP__case_85 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_85 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_85 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_86 :: Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP__case_86 x3 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Pattern x4 x5) -> Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip Curry_Prelude.d_C_notElem x5) (d_C_freeVarsInExp x3 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_LPattern x6) -> d_C_freeVarsInExp x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_86 x3 x1002 x3250 x3500) (d_OP__case_86 x3 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_86 x3 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_86 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_90 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr))
d_OP__case_90 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_89 x2 x1 (Curry_Prelude.d_OP_gt_eq x1 (Curry_Prelude.d_C_length x2 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_90 x2 x1 x1002 x3250 x3500) (d_OP__case_90 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_90 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_90 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_89 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr))
d_OP__case_89 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Nothing
     Curry_Prelude.C_False -> d_OP__case_88 x2 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem (d_C_freeVarsInExp (Curry_Prelude.d_C_snd (Curry_Prelude.d_OP_bang_bang x2 x1 x3250 x3500) x3250 x3500) x3250 x3500)) x3250 x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst x2 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_89 x2 x1 x1002 x3250 x3500) (d_OP__case_89 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_89 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_89 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_88 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr))
d_OP__case_88 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> d_C_trySequentializeLetBinding (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 1#) x3250 x3500) x2 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_87 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_88 x2 x1 x1002 x3250 x3500) (d_OP__case_88 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_88 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_88 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_87 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr))
d_OP__case_87 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_maybe Curry_Prelude.C_Nothing (d_OP_trySequentializeLetBinding_dot___hash_lambda10 x2 x1) (d_C_trySequentializeLetBinding (Curry_Prelude.C_Int 0#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_take x1 x2 x3250 x3500) (Curry_Prelude.d_C_drop (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 1#) x3250 x3500) x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_87 x2 x1 x1002 x3250 x3500) (d_OP__case_87 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_87 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_87 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_91 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Char
d_OP__case_91 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_chr (Curry_Prelude.d_OP_plus (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char '0'#) x3250 x3500) x1 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_chr (Curry_Prelude.d_OP_minus (Curry_Prelude.d_OP_plus (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'A'#) x3250 x3500) x1 x3250 x3500) (Curry_Prelude.C_Int 10#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_91 x1 x1002 x3250 x3500) (d_OP__case_91 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_91 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_91 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_93 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_93 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 (d_C_encodeCurryId x3 x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_92 x2 x3 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_93 x2 x3 x1002 x3250 x3500) (d_OP__case_93 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_93 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_93 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_92 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_92 x2 x3 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x4 = Curry_Prelude.d_C_ord x2 x3250 x3500
           in (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (d_OP_encodeCurryId_dot_int2hex_dot_247 (Curry_Prelude.d_C_div x4 (Curry_Prelude.C_Int 16#) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (d_OP_encodeCurryId_dot_int2hex_dot_247 (Curry_Prelude.d_C_mod x4 (Curry_Prelude.C_Int 16#) x3250 x3500) x3250 x3500) (d_C_encodeCurryId x3 x3250 x3500))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_92 x2 x3 x1002 x3250 x3500) (d_OP__case_92 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_92 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_92 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_95 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_95 x3 x1 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> d_OP__case_94 x1 x3 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_95 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_95 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_95 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_95 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_94 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_94 x1 x3 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_maybe (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List) (d_C_encodeCurryId x3 x3250 x3500) x3250 x3500) x3250 x3500) Curry_Prelude.d_C_id (Curry_Prelude.d_C_lookup x1 (d_C_jsConstructors x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_94 x1 x3 x2 x1002 x3250 x3500) (d_OP__case_94 x1 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_94 x1 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_94 x1 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_98 :: Curry_Prelude.OP_List Curry_JavaScript.C_JSExp -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_JavaScript.C_JSExp -> Curry_Prelude.OP_List Curry_JavaScript.C_JSExp -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_OP__case_98 x2 x1 x3 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP__case_97 x2 x1 x5 x3 x6 x3250 x3500
     Curry_Prelude.OP_List -> Curry_JavaScript.C_JSFCall (d_C_qname2JS x1 x3250 x3500) x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_98 x2 x1 x3 x1002 x3250 x3500) (d_OP__case_98 x2 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_98 x2 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_98 x2 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_97 :: Curry_Prelude.OP_List Curry_JavaScript.C_JSExp -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_JavaScript.C_JSExp -> Curry_JavaScript.C_JSExp -> Curry_Prelude.OP_List Curry_JavaScript.C_JSExp -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_OP__case_97 x2 x1 x5 x3 x6 x3250 x3500 = case x6 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_maybe (let
          x7 = d_C_qname2JS x1 x3250 x3500
           in (d_OP__case_96 x7 x5 x3 x1 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_slash_eq x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))) x3250 x3500) (d_C_explicitApply x3250 x3500) x3250 x3500) x3250 x3500)) (d_OP_curryFunc2JSFunc_dot___hash_lambda9 x3 x5) (Curry_Prelude.d_C_lookup x1 (d_C_jsOperators x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x8 x9) -> Curry_JavaScript.C_JSFCall (d_C_qname2JS x1 x3250 x3500) x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_97 x2 x1 x5 x3 x1002 x3250 x3500) (d_OP__case_97 x2 x1 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_97 x2 x1 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_97 x2 x1 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_96 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_JavaScript.C_JSExp -> Curry_JavaScript.C_JSExp -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_OP__case_96 x7 x5 x3 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_JavaScript.C_JSFCall (d_C_qname2JS x1 x3250 x3500) (Curry_Prelude.OP_Cons x3 (Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List))
     Curry_Prelude.C_False -> Curry_JavaScript.C_JSApply x3 x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_96 x7 x5 x3 x1 x1002 x3250 x3500) (d_OP__case_96 x7 x5 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_96 x7 x5 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_96 x7 x5 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_100 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_100 x2 x4 x3 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_TypeSyn x5 x6 x7 x8) -> d_C_isUniqueConstructor x4 x2 x3250 x3500
     (Curry_FlatCurry.C_Type x9 x10 x11 x12) -> d_OP__case_99 x12 x2 x4 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x2 x3250 x3500) (Curry_Prelude.d_C_map d_OP_isUniqueConstructor_dot___hash_lambda7 x12 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_100 x2 x4 x1002 x3250 x3500) (d_OP__case_100 x2 x4 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_100 x2 x4 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_100 x2 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_99 :: Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_99 x12 x2 x4 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x12 x3250 x3500) (Curry_Prelude.C_Int 1#) x3250 x3500
     Curry_Prelude.C_False -> d_C_isUniqueConstructor x4 x2 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_99 x12 x2 x4 x1002 x3250 x3500) (d_OP__case_99 x12 x2 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_99 x12 x2 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_99 x12 x2 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_102 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_BranchExpr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSBranch
nd_OP__case_102 x2 x5 x9 x6 x4 x3 x1 x8 x3000 x3250 x3500 = case x8 of
     (Curry_FlatCurry.C_Branch x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_101 x2 x5 x9 x6 x4 x3 x1 x11 x10 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_102 x2 x5 x9 x6 x4 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_102 x2 x5 x9 x6 x4 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_102 x2 x5 x9 x6 x4 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_102 x2 x5 x9 x6 x4 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_101 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Pattern -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSBranch
nd_OP__case_101 x2 x5 x9 x6 x4 x3 x1 x11 x10 x3000 x3250 x3500 = case x10 of
     (Curry_FlatCurry.C_Pattern x12 x13) -> let
          x2006 = x3000
           in (seq x2006 (let
               x14 = Curry_Prelude.d_OP_plus x2 (Curry_Prelude.d_C_length x13 x3250 x3500) x3250 x3500
                in (let
                    x2004 = leftSupply x2006
                    x2005 = rightSupply x2006
                     in (seq x2004 (seq x2005 (let
                         x15 = generate x2005 x3250
                          in (let
                              x2000 = leftSupply x2004
                              x2003 = rightSupply x2004
                               in (seq x2000 (seq x2003 (let
                                   x16 = Curry_Prelude.nd_C_map (wrapDX id (d_OP_branch2JS_dot___hash_lambda6 x5)) (Curry_Prelude.d_C_zip (Curry_Prelude.d_C_enumFrom (Curry_Prelude.C_Int 1#) x3250 x3500) x13 x3250 x3500) x2000 x3250 x3500
                                    in (let
                                        x2001 = leftSupply x2003
                                        x2002 = rightSupply x2003
                                         in (seq x2001 (seq x2002 (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSCase (d_C_consQName2JS x12 x3250 x3500) (nd_C_flatExp2JS x1 x14 x15 (Curry_Prelude.d_OP_plus_plus x16 x4 x3250 x3500) x6 x11 x2001 x3250 x3500)) (nd_C_branch2JS x1 x15 x3 x4 x5 x6 x9 x2002 x3250 x3500)))))))))))))))
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_101 x2 x5 x9 x6 x4 x3 x1 x11 x1002 x3000 x3250 x3500) (nd_OP__case_101 x2 x5 x9 x6 x4 x3 x1 x11 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_101 x2 x5 x9 x6 x4 x3 x1 x11 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_101 x2 x5 x9 x6 x4 x3 x1 x11 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_104 :: Curry_Prelude.OP_List Curry_JavaScript.C_JSBranch -> Curry_JavaScript.C_JSBranch -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP__case_104 x3 x2 x3250 x3500 = case x2 of
     (Curry_JavaScript.C_JSCase x4 x5) -> d_OP__case_103 x5 x3 x3250 x3500
     (Curry_JavaScript.Choice_C_JSBranch x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_104 x3 x1002 x3250 x3500) (d_OP__case_104 x3 x1003 x3250 x3500)
     (Curry_JavaScript.Choices_C_JSBranch x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_104 x3 z x3250 x3500) x1002
     (Curry_JavaScript.Guard_C_JSBranch x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_104 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_JavaScript.Fail_C_JSBranch x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_103 :: Curry_Prelude.OP_List Curry_JavaScript.C_JSStat -> Curry_Prelude.OP_List Curry_JavaScript.C_JSBranch -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP__case_103 x5 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_103 x5 x1002 x3250 x3500) (d_OP__case_103 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_103 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_103 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_105 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_105 x1 x3 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_LPattern x5) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Pattern x6 x7) -> d_C_isUniqueConstructor x1 x6 x3250 x3500
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_105 x1 x1002 x3250 x3500) (d_OP__case_105 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_105 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_105 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_106 :: Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP__case_106 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSIf (Curry_JavaScript.C_JSOp (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)) (Curry_JavaScript.C_JSFCall (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSIVar x2) Curry_Prelude.OP_List)) (Curry_JavaScript.C_JSString (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (Curry_JavaScript.C_JSIVar x2) (Curry_JavaScript.C_JSFCall (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSIVar x2) Curry_Prelude.OP_List))) Curry_Prelude.OP_List) Curry_Prelude.OP_List) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_106 x1 x2 x1002 x3250 x3500) (d_OP__case_106 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_106 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_106 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_108 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_108 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Branch x4 x5) -> d_OP__case_107 x4 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_108 x1002 x3250 x3500) (d_OP__case_108 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_108 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_108 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_107 :: Curry_FlatCurry.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_107 x4 x3250 x3500 = case x4 of
     (Curry_FlatCurry.C_Pattern x6 x7) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem (Curry_Prelude.d_C_snd x6 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) Curry_Prelude.OP_List)) x3250 x3500
     (Curry_FlatCurry.C_LPattern x8) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_107 x1002 x3250 x3500) (d_OP__case_107 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_107 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_107 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_109 :: Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
nd_OP__case_109 x7 x1 x5 x8 x4 x3 x9 x11 x3000 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (let
               x10 = nd_C_branch2JS x1 x9 x3 x4 x8 x5 x7 x2000 x3250 x3500
                in (d_OP_case2JS_dot___hash_selFP2_hash_cstats x10 x3250 x3500)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSSwitch (Curry_JavaScript.C_JSIArrayIdx x8 (Curry_Prelude.C_Int 0#)) (nd_C_branch2JS x1 x9 x3 x4 x8 x5 x7 x2000 x3250 x3500)) Curry_Prelude.OP_List))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_109 x7 x1 x5 x8 x4 x3 x9 x1002 x3000 x3250 x3500) (nd_OP__case_109 x7 x1 x5 x8 x4 x3 x9 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_109 x7 x1 x5 x8 x4 x3 x9 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_109 x7 x1 x5 x8 x4 x3 x9 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_110 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
nd_OP__case_110 x7 x4 x3 x1 x2 x6 x3000 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x10 = generate x2003 x3250
                     in (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSVarDecl x8) (let
                         x2000 = leftSupply x2002
                         x2001 = rightSupply x2002
                          in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_flatExp2JS x1 x2 x10 x4 x8 x9 x2000 x3250 x3500) (nd_C_flatExps2JS x1 x10 x3 x4 x7 x2001 x3250 x3500) x3250 x3500))))))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_110 x7 x4 x3 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_110 x7 x4 x3 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_110 x7 x4 x3 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_110 x7 x4 x3 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_128 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_128 x1 x3 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_Pattern x5 x6) -> d_OP__case_127 x6 x1 x5 x3250 x3500
     (Curry_FlatCurry.C_LPattern x34) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_128 x1 x1002 x3250 x3500) (d_OP__case_128 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_128 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_128 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_127 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_127 x6 x1 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> d_OP__case_126 x6 x1 x8 x7 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_127 x6 x1 x1002 x3250 x3500) (d_OP__case_127 x6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_127 x6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_127 x6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_126 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_126 x6 x1 x8 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x9 x10) -> let
          x11 = x9
           in (d_OP__case_125 x11 x10 x6 x1 x8 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char 'P'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_126 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_126 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_126 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_126 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_125 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_125 x11 x10 x6 x1 x8 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> d_OP__case_124 x6 x1 x8 x10 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_125 x11 x10 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_125 x11 x10 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_125 x11 x10 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_125 x11 x10 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_124 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_124 x6 x1 x8 x10 x3250 x3500 = case x10 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x14 = x12
           in (d_OP__case_123 x14 x13 x6 x1 x8 (Curry_Prelude.d_OP_eq_eq x14 (Curry_Prelude.C_Char 'r'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_124 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_124 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_124 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_124 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_123 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_123 x14 x13 x6 x1 x8 x15 x3250 x3500 = case x15 of
     Curry_Prelude.C_True -> d_OP__case_122 x6 x1 x8 x13 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_123 x14 x13 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_123 x14 x13 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_123 x14 x13 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_123 x14 x13 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_122 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_122 x6 x1 x8 x13 x3250 x3500 = case x13 of
     (Curry_Prelude.OP_Cons x15 x16) -> let
          x17 = x15
           in (d_OP__case_121 x17 x16 x6 x1 x8 (Curry_Prelude.d_OP_eq_eq x17 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_122 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_122 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_122 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_122 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_121 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_121 x17 x16 x6 x1 x8 x18 x3250 x3500 = case x18 of
     Curry_Prelude.C_True -> d_OP__case_120 x6 x1 x8 x16 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_121 x17 x16 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_121 x17 x16 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_121 x17 x16 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_121 x17 x16 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_120 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_120 x6 x1 x8 x16 x3250 x3500 = case x16 of
     (Curry_Prelude.OP_Cons x18 x19) -> let
          x20 = x18
           in (d_OP__case_119 x20 x19 x6 x1 x8 (Curry_Prelude.d_OP_eq_eq x20 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_120 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_120 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_120 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_120 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_119 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_119 x20 x19 x6 x1 x8 x21 x3250 x3500 = case x21 of
     Curry_Prelude.C_True -> d_OP__case_118 x6 x1 x8 x19 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_119 x20 x19 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_119 x20 x19 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_119 x20 x19 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_119 x20 x19 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_118 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_118 x6 x1 x8 x19 x3250 x3500 = case x19 of
     (Curry_Prelude.OP_Cons x21 x22) -> let
          x23 = x21
           in (d_OP__case_117 x23 x22 x6 x1 x8 (Curry_Prelude.d_OP_eq_eq x23 (Curry_Prelude.C_Char 'u'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_118 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_118 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_118 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_118 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_117 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_117 x23 x22 x6 x1 x8 x24 x3250 x3500 = case x24 of
     Curry_Prelude.C_True -> d_OP__case_116 x6 x1 x8 x22 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_117 x23 x22 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_117 x23 x22 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_117 x23 x22 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_117 x23 x22 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_116 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_116 x6 x1 x8 x22 x3250 x3500 = case x22 of
     (Curry_Prelude.OP_Cons x24 x25) -> let
          x26 = x24
           in (d_OP__case_115 x26 x25 x6 x1 x8 (Curry_Prelude.d_OP_eq_eq x26 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_116 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_116 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_116 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_116 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_115 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_115 x26 x25 x6 x1 x8 x27 x3250 x3500 = case x27 of
     Curry_Prelude.C_True -> d_OP__case_114 x6 x1 x8 x25 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_115 x26 x25 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_115 x26 x25 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_115 x26 x25 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_115 x26 x25 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_114 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_114 x6 x1 x8 x25 x3250 x3500 = case x25 of
     (Curry_Prelude.OP_Cons x27 x28) -> let
          x29 = x27
           in (d_OP__case_113 x29 x28 x6 x1 x8 (Curry_Prelude.d_OP_eq_eq x29 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_114 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_114 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_114 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_114 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_113 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_113 x29 x28 x6 x1 x8 x30 x3250 x3500 = case x30 of
     Curry_Prelude.C_True -> d_OP__case_112 x6 x1 x8 x28 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_113 x29 x28 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_113 x29 x28 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_113 x29 x28 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_113 x29 x28 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_112 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_112 x6 x1 x8 x28 x3250 x3500 = case x28 of
     Curry_Prelude.OP_List -> d_OP__case_111 x1 x8 x6 x3250 x3500
     (Curry_Prelude.OP_Cons x32 x33) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_112 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_112 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_112 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_112 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_111 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_111 x1 x8 x6 x3250 x3500 = case x6 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_eq_eq x8 x1 x3250 x3500
     (Curry_Prelude.OP_Cons x30 x31) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_111 x1 x8 x1002 x3250 x3500) (d_OP__case_111 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_111 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_111 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_129 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_OP__case_129 x3 x5 x4 x2 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_JavaScript.C_JSLambda (Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSReturn (Curry_JavaScript.d_C_jsConsTerm (d_C_consQName2JS x2 x3250 x3500) (Curry_Prelude.d_C_map (acceptCs id Curry_JavaScript.C_JSIVar) (Curry_Prelude.d_OP_plus_plus x1 x4 x3250 x3500) x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> Curry_JavaScript.C_JSLambda (Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSReturn (d_OP_flatExp2JS_dot_genLambda_dot_107 x1 x2 x3 x4 (Curry_Prelude.d_OP_plus x5 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_129 x3 x5 x4 x2 x1 x1002 x3250 x3500) (d_OP__case_129 x3 x5 x4 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_129 x3 x5 x4 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_129 x3 x5 x4 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_130 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_JavaScript.C_JSExp
d_OP__case_130 x3 x5 x4 x2 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_JavaScript.C_JSLambda (Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSReturn (d_C_curryFunc2JSFunc x2 (Curry_Prelude.d_C_map (acceptCs id Curry_JavaScript.C_JSIVar) (Curry_Prelude.d_OP_plus_plus x1 x4 x3250 x3500) x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> Curry_JavaScript.C_JSLambda (Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSReturn (d_OP_flatExp2JS_dot_genLambda_dot_92 x1 x2 x3 x4 (Curry_Prelude.d_OP_plus x5 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_130 x3 x5 x4 x2 x1 x1002 x3250 x3500) (d_OP__case_130 x3 x5 x4 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_130 x3 x5 x4 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_130 x3 x5 x4 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_131 :: Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
nd_OP__case_131 x35 x34 x5 x4 x3 x2 x1 x36 x3000 x3250 x3500 = case x36 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_ite2JS x1 x2 x3 x4 x5 x34 (d_OP_flatExp2JS_dot_expOfBranch_dot_122 (Curry_Prelude.d_OP_bang_bang x35 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500) (d_OP_flatExp2JS_dot_expOfBranch_dot_122 (Curry_Prelude.d_OP_bang_bang x35 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_case2JS x1 x2 x3 x4 x5 x34 x35 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_131 x35 x34 x5 x4 x3 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_131 x35 x34 x5 x4 x3 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_131 x35 x34 x5 x4 x3 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_131 x35 x34 x5 x4 x3 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_136 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_CombType -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
nd_OP__case_136 x14 x2 x13 x5 x4 x3 x1 x12 x3000 x3250 x3500 = case x12 of
     Curry_FlatCurry.C_FuncCall -> let
          x2002 = x3000
           in (seq x2002 (let
               x15 = Curry_Prelude.d_OP_plus x2 (Curry_Prelude.d_C_length x14 x3250 x3500) x3250 x3500
               x16 = Curry_Prelude.d_C_enumFromTo (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.C_Int 1#) x3250 x3500) x15 x3250 x3500
                in (let
                    x2000 = leftSupply x2002
                    x2001 = rightSupply x2002
                     in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_flatExps2JS x1 x15 x3 x4 (Curry_Prelude.d_C_zip x16 x14 x3250 x3500) x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (Curry_JavaScript.C_JSIVar x5) (d_C_curryFunc2JSFunc x13 (Curry_Prelude.nd_C_map (wrapDX id (acceptCs id Curry_JavaScript.C_JSIVar)) x16 x2001 x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List) x3250 x3500))))))
     Curry_FlatCurry.C_ConsCall -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_135 x13 x2 x14 x5 x4 x3 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_fst x13 x3250 x3500) (d_C_prelude x3250 x3500) x3250 x3500) (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_elem (Curry_Prelude.d_C_snd x13 x3250 x3500) x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List)) x2001 x3250 x3500)))) x3250 x3500) x2003 x3250 x3500)))))
     (Curry_FlatCurry.C_FuncPartCall x19) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_133 x14 x2 x19 x13 x5 x4 x3 x1 (d_C_explicitApply x3250 x3500) x2000 x3250 x3500))
     (Curry_FlatCurry.C_ConsPartCall x25) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_132 x14 x2 x25 x13 x5 x4 x3 x1 (d_C_explicitApply x3250 x3500) x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_CombType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_136 x14 x2 x13 x5 x4 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_136 x14 x2 x13 x5 x4 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_CombType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_136 x14 x2 x13 x5 x4 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_CombType x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_136 x14 x2 x13 x5 x4 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CombType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_132 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
nd_OP__case_132 x14 x2 x25 x13 x5 x4 x3 x1 x31 x3000 x3250 x3500 = case x31 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x26 = Curry_Prelude.d_OP_plus x2 (Curry_Prelude.d_C_length x14 x3250 x3500) x3250 x3500
               x27 = Curry_Prelude.d_C_enumFromTo (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.C_Int 1#) x3250 x3500) x26 x3250 x3500
                in (let
                    x2000 = leftSupply x2002
                    x2001 = rightSupply x2002
                     in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_flatExps2JS x1 x26 x3 x4 (Curry_Prelude.d_C_zip x27 x14 x3250 x3500) x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (Curry_JavaScript.C_JSIVar x5) (Curry_JavaScript.C_JSFCall (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSString (d_C_consQName2JS x13 x3250 x3500)) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSInt x25) (Curry_Prelude.nd_C_map (wrapDX id (acceptCs id Curry_JavaScript.C_JSIVar)) x27 x2001 x3250 x3500))))) Curry_Prelude.OP_List) x3250 x3500))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (let
               x28 = Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.d_C_length x14 x3250 x3500) x3250 x3500) x25 x3250 x3500
               x29 = Curry_Prelude.d_C_enumFromTo (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.C_Int 1#) x3250 x3500) (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.d_C_length x14 x3250 x3500) x3250 x3500) x3250 x3500
               x30 = Curry_Prelude.d_C_enumFromTo (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.d_C_length x14 x3250 x3500) x3250 x3500) (Curry_Prelude.C_Int 1#) x3250 x3500) x28 x3250 x3500
                in (Curry_Prelude.d_OP_plus_plus (nd_C_flatExps2JS x1 x28 x3 x4 (Curry_Prelude.d_C_zip x29 x14 x3250 x3500) x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (Curry_JavaScript.C_JSIVar x5) (d_OP_flatExp2JS_dot_genLambda_dot_107 x29 x13 x28 x30 (Curry_Prelude.d_C_head x30 x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List) x3250 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_132 x14 x2 x25 x13 x5 x4 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_132 x14 x2 x25 x13 x5 x4 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_132 x14 x2 x25 x13 x5 x4 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_132 x14 x2 x25 x13 x5 x4 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_133 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
nd_OP__case_133 x14 x2 x19 x13 x5 x4 x3 x1 x25 x3000 x3250 x3500 = case x25 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x20 = Curry_Prelude.d_OP_plus x2 (Curry_Prelude.d_C_length x14 x3250 x3500) x3250 x3500
               x21 = Curry_Prelude.d_C_enumFromTo (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.C_Int 1#) x3250 x3500) x20 x3250 x3500
                in (let
                    x2000 = leftSupply x2002
                    x2001 = rightSupply x2002
                     in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_flatExps2JS x1 x20 x3 x4 (Curry_Prelude.d_C_zip x21 x14 x3250 x3500) x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (Curry_JavaScript.C_JSIVar x5) (Curry_JavaScript.C_JSFCall (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSString (d_C_qname2JS x13 x3250 x3500)) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSInt x19) (Curry_Prelude.nd_C_map (wrapDX id (acceptCs id Curry_JavaScript.C_JSIVar)) x21 x2001 x3250 x3500))))) Curry_Prelude.OP_List) x3250 x3500))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (let
               x22 = Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.d_C_length x14 x3250 x3500) x3250 x3500) x19 x3250 x3500
               x23 = Curry_Prelude.d_C_enumFromTo (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.C_Int 1#) x3250 x3500) (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.d_C_length x14 x3250 x3500) x3250 x3500) x3250 x3500
               x24 = Curry_Prelude.d_C_enumFromTo (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.d_C_length x14 x3250 x3500) x3250 x3500) (Curry_Prelude.C_Int 1#) x3250 x3500) x22 x3250 x3500
                in (Curry_Prelude.d_OP_plus_plus (nd_C_flatExps2JS x1 x22 x3 x4 (Curry_Prelude.d_C_zip x23 x14 x3250 x3500) x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (Curry_JavaScript.C_JSIVar x5) (d_OP_flatExp2JS_dot_genLambda_dot_92 x23 x13 x22 x24 (Curry_Prelude.d_C_head x24 x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List) x3250 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_133 x14 x2 x19 x13 x5 x4 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_133 x14 x2 x19 x13 x5 x4 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_133 x14 x2 x19 x13 x5 x4 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_133 x14 x2 x19 x13 x5 x4 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_135 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
nd_OP__case_135 x13 x2 x14 x5 x4 x3 x1 x15 x3000 x3250 x3500 = case x15 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (Curry_JavaScript.C_JSIVar x5) (Curry_JavaScript.C_JSBool (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_snd x13 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x3250 x3500))) Curry_Prelude.OP_List) x3250 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_134 x2 x14 x13 x5 x4 x3 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_135 x13 x2 x14 x5 x4 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_135 x13 x2 x14 x5 x4 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_135 x13 x2 x14 x5 x4 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_135 x13 x2 x14 x5 x4 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_134 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
nd_OP__case_134 x2 x14 x13 x5 x4 x3 x1 x19 x3000 x3250 x3500 = case x19 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x17 = Curry_Prelude.d_OP_plus x2 (Curry_Prelude.d_C_length x14 x3250 x3500) x3250 x3500
               x18 = Curry_Prelude.d_C_enumFromTo (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.C_Int 1#) x3250 x3500) x17 x3250 x3500
                in (let
                    x2000 = leftSupply x2002
                    x2001 = rightSupply x2002
                     in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_flatExps2JS x1 x17 x3 x4 (Curry_Prelude.d_C_zip x18 x14 x3250 x3500) x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (Curry_JavaScript.C_JSIVar x5) (Curry_JavaScript.d_C_jsConsTerm (d_C_consQName2JS x13 x3250 x3500) (Curry_Prelude.nd_C_map (wrapDX id (acceptCs id Curry_JavaScript.C_JSIVar)) x18 x2001 x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List) x3250 x3500))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_134 x2 x14 x13 x5 x4 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_134 x2 x14 x13 x5 x4 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_134 x2 x14 x13 x5 x4 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_134 x2 x14 x13 x5 x4 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_137 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_FlatCurry.C_Literal -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP__case_137 x5 x2 x3 x8 x3250 x3500 = case x8 of
     (Curry_FlatCurry.C_Intc x9) -> d_OP___cond_0__case_137 x5 x9 (Curry_Prelude.d_OP_eq_colon_eq x3 x2 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Floatc x10) -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_FlatCurry.C_Charc x11) -> d_OP___cond_1__case_137 x5 x11 (Curry_Prelude.d_OP_eq_colon_eq x3 x2 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Literal x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_137 x5 x2 x3 x1002 x3250 x3500) (d_OP__case_137 x5 x2 x3 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Literal x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_137 x5 x2 x3 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Literal x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_137 x5 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Literal x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___cond_1__case_137 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Char -> Curry_Prelude.C_Success -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP___cond_1__case_137 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_Success -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (Curry_JavaScript.C_JSIVar x1) (Curry_JavaScript.C_JSString (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List))) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_1__case_137 x1 x2 x1002 x3250 x3500) (d_OP___cond_1__case_137 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_1__case_137 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_1__case_137 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___cond_0__case_137 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Success -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSStat
d_OP___cond_0__case_137 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_Success -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSAssign (Curry_JavaScript.C_JSIVar x1) (Curry_JavaScript.C_JSInt x2)) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0__case_137 x1 x2 x1002 x3250 x3500) (d_OP___cond_0__case_137 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0__case_137 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0__case_137 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_138 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> IDSupply -> Cover -> ConstStore -> Curry_JavaScript.C_JSFDecl
nd_OP__case_138 x1 x3 x7 x3000 x3250 x3500 = case x7 of
     (Curry_FlatCurry.C_Rule x8 x9) -> let
          x2002 = x3000
           in (seq x2002 (let
               x10 = Curry_Prelude.d_OP_plus (Curry_Integer.d_C_maxlist (Curry_Prelude.OP_Cons (d_C_maxVarIndexInExp x9 x3250 x3500) x8) x3250 x3500) (Curry_Prelude.C_Int 1#) x3250 x3500
                in (Curry_JavaScript.C_JSFDecl (d_C_qname2JS x3 x3250 x3500) x8 (d_C_removeSingleVarsJSStatements (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSVarDecl x10) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_flatExp2JS x1 x10 (Curry_Prelude.nd_C_unknown x2000 x3250 x3500) Curry_Prelude.OP_List x10 x9 x2001 x3250 x3500)))) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSReturn (Curry_JavaScript.C_JSIVar x10)) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500))))
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_138 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_138 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_138 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_138 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_139 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSBranch
d_OP__case_139 x3 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSCase (d_C_qname2JS x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSReturn (d_C_curryFunc2JSFunc x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_map d_OP_genApply_dot_branch4fun_dot_33_dot___hash_lambda1 (Curry_Prelude.d_C_enumFromTo (Curry_Prelude.C_Int 2#) x3 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_JavaScript.C_JSIVar (Curry_Prelude.C_Int 2#)) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500)) Curry_Prelude.OP_List)) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_139 x3 x2 x1002 x3250 x3500) (d_OP__case_139 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_139 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_139 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_142 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_142 x2 x6 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_External x7) -> d_OP__case_141 x2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x2 x3250 x3500) (d_C_ignoredFunctions x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Rule x8 x9) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_notElem x2 x3250 x3500) (d_C_ignoredFunctions x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_142 x2 x1002 x3250 x3500) (d_OP__case_142 x2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_142 x2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_142 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_141 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_141 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_False
     Curry_Prelude.C_False -> d_OP__case_140 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_141 x2 x1002 x3250 x3500) (d_OP__case_141 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_141 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_141 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_140 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_140 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_140 x2 x1002 x3250 x3500) (d_OP__case_140 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_140 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_140 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_143 :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_JavaScript.C_JSFDecl
d_OP__case_143 x7 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (d_C_genApply (d_C_pafsOfFuncs x7 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_143 x7 x1002 x3250 x3500) (d_OP__case_143 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_143 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_143 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
