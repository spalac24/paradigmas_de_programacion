{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_JavaScript (C_JSExp (..), C_JSStat (..), C_JSBranch (..), C_JSFDecl (..), d_C_showJSExp, d_C_showJSStat, d_C_showJSFDecl, d_C_jsConsTerm) where

import Basics
import qualified Curry_List
import qualified Curry_Prelude
data C_JSExp
     = C_JSString (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_JSInt Curry_Prelude.C_Int
     | C_JSBool Curry_Prelude.C_Bool
     | C_JSIVar Curry_Prelude.C_Int
     | C_JSIArrayIdx Curry_Prelude.C_Int Curry_Prelude.C_Int
     | C_JSOp (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_JSExp C_JSExp
     | C_JSFCall (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List C_JSExp)
     | C_JSApply C_JSExp C_JSExp
     | C_JSLambda (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List C_JSStat)
     | Choice_C_JSExp Cover ID C_JSExp C_JSExp
     | Choices_C_JSExp Cover ID ([C_JSExp])
     | Fail_C_JSExp Cover FailInfo
     | Guard_C_JSExp Cover Constraints C_JSExp

instance Show C_JSExp where
  showsPrec d (Choice_C_JSExp cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_JSExp cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_JSExp cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_JSExp cd info) = showChar '!'
  showsPrec _ (C_JSString x1) = (showString "(JSString") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_JSInt x1) = (showString "(JSInt") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_JSBool x1) = (showString "(JSBool") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_JSIVar x1) = (showString "(JSIVar") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_JSIArrayIdx x1 x2) = (showString "(JSIArrayIdx") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_JSOp x1 x2 x3) = (showString "(JSOp") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_JSFCall x1 x2) = (showString "(JSFCall") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_JSApply x1 x2) = (showString "(JSApply") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_JSLambda x1 x2) = (showString "(JSLambda") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_JSExp where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_JSString x1,r1) | (_,r0) <- readQualified "JavaScript" "JSString" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_JSInt x1,r1) | (_,r0) <- readQualified "JavaScript" "JSInt" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_JSBool x1,r1) | (_,r0) <- readQualified "JavaScript" "JSBool" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_JSIVar x1,r1) | (_,r0) <- readQualified "JavaScript" "JSIVar" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_JSIArrayIdx x1 x2,r2) | (_,r0) <- readQualified "JavaScript" "JSIArrayIdx" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_JSOp x1 x2 x3,r3) | (_,r0) <- readQualified "JavaScript" "JSOp" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ ((readParen (d > 10) (\r -> [ (C_JSFCall x1 x2,r2) | (_,r0) <- readQualified "JavaScript" "JSFCall" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_JSApply x1 x2,r2) | (_,r0) <- readQualified "JavaScript" "JSApply" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_JSLambda x1 x2,r2) | (_,r0) <- readQualified "JavaScript" "JSLambda" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s))))))))


instance NonDet C_JSExp where
  choiceCons = Choice_C_JSExp
  choicesCons = Choices_C_JSExp
  failCons = Fail_C_JSExp
  guardCons = Guard_C_JSExp
  try (Choice_C_JSExp cd i x y) = tryChoice cd i x y
  try (Choices_C_JSExp cd i xs) = tryChoices cd i xs
  try (Fail_C_JSExp cd info) = Fail cd info
  try (Guard_C_JSExp cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_JSExp cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_JSExp cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_JSExp cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_JSExp cd i _) = error ("JavaScript.JSExp.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_JSExp cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_JSExp cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_JSExp where
  generate s = Choices_C_JSExp defCover (freeID [1,1,1,1,2,3,2,2,2] s) [(C_JSString (generate (leftSupply s))),(C_JSInt (generate (leftSupply s))),(C_JSBool (generate (leftSupply s))),(C_JSIVar (generate (leftSupply s))),(C_JSIArrayIdx (generate (leftSupply s)) (generate (rightSupply s))),(C_JSOp (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s))),(C_JSFCall (generate (leftSupply s)) (generate (rightSupply s))),(C_JSApply (generate (leftSupply s)) (generate (rightSupply s))),(C_JSLambda (generate (leftSupply s)) (generate (rightSupply s)))]


instance NormalForm C_JSExp where
  ($!!) cont (C_JSString x1) cs = ((\y1 cs -> cont (C_JSString y1) cs) $!! x1) cs
  ($!!) cont (C_JSInt x1) cs = ((\y1 cs -> cont (C_JSInt y1) cs) $!! x1) cs
  ($!!) cont (C_JSBool x1) cs = ((\y1 cs -> cont (C_JSBool y1) cs) $!! x1) cs
  ($!!) cont (C_JSIVar x1) cs = ((\y1 cs -> cont (C_JSIVar y1) cs) $!! x1) cs
  ($!!) cont (C_JSIArrayIdx x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSIArrayIdx y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_JSOp x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_JSOp y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_JSFCall x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSFCall y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_JSApply x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSApply y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_JSLambda x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSLambda y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_JSExp cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_JSExp cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_JSExp cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_JSExp cd info) _ = failCons cd info
  ($##) cont (C_JSString x1) cs = ((\y1 cs -> cont (C_JSString y1) cs) $## x1) cs
  ($##) cont (C_JSInt x1) cs = ((\y1 cs -> cont (C_JSInt y1) cs) $## x1) cs
  ($##) cont (C_JSBool x1) cs = ((\y1 cs -> cont (C_JSBool y1) cs) $## x1) cs
  ($##) cont (C_JSIVar x1) cs = ((\y1 cs -> cont (C_JSIVar y1) cs) $## x1) cs
  ($##) cont (C_JSIArrayIdx x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSIArrayIdx y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_JSOp x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_JSOp y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_JSFCall x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSFCall y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_JSApply x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSApply y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_JSLambda x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSLambda y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_JSExp cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_JSExp cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_JSExp cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_JSExp cd info) _ = failCons cd info
  searchNF search cont (C_JSString x1) = search (\y1 -> cont (C_JSString y1)) x1
  searchNF search cont (C_JSInt x1) = search (\y1 -> cont (C_JSInt y1)) x1
  searchNF search cont (C_JSBool x1) = search (\y1 -> cont (C_JSBool y1)) x1
  searchNF search cont (C_JSIVar x1) = search (\y1 -> cont (C_JSIVar y1)) x1
  searchNF search cont (C_JSIArrayIdx x1 x2) = search (\y1 -> search (\y2 -> cont (C_JSIArrayIdx y1 y2)) x2) x1
  searchNF search cont (C_JSOp x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_JSOp y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_JSFCall x1 x2) = search (\y1 -> search (\y2 -> cont (C_JSFCall y1 y2)) x2) x1
  searchNF search cont (C_JSApply x1 x2) = search (\y1 -> search (\y2 -> cont (C_JSApply y1 y2)) x2) x1
  searchNF search cont (C_JSLambda x1 x2) = search (\y1 -> search (\y2 -> cont (C_JSLambda y1 y2)) x2) x1
  searchNF _ _ x = error ("JavaScript.JSExp.searchNF: no constructor: " ++ (show x))


instance Unifiable C_JSExp where
  (=.=) (C_JSString x1) (C_JSString y1) cs = (x1 =:= y1) cs
  (=.=) (C_JSInt x1) (C_JSInt y1) cs = (x1 =:= y1) cs
  (=.=) (C_JSBool x1) (C_JSBool y1) cs = (x1 =:= y1) cs
  (=.=) (C_JSIVar x1) (C_JSIVar y1) cs = (x1 =:= y1) cs
  (=.=) (C_JSIArrayIdx x1 x2) (C_JSIArrayIdx y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_JSOp x1 x2 x3) (C_JSOp y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) (C_JSFCall x1 x2) (C_JSFCall y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_JSApply x1 x2) (C_JSApply y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_JSLambda x1 x2) (C_JSLambda y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_JSString x1) (C_JSString y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_JSInt x1) (C_JSInt y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_JSBool x1) (C_JSBool y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_JSIVar x1) (C_JSIVar y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_JSIArrayIdx x1 x2) (C_JSIArrayIdx y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_JSOp x1 x2 x3) (C_JSOp y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) (C_JSFCall x1 x2) (C_JSFCall y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_JSApply x1 x2) (C_JSApply y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_JSLambda x1 x2) (C_JSLambda y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_JSString x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_JSInt x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_JSBool x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_JSIVar x2) = ((i :=: (ChooseN 3 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_JSIArrayIdx x2 x3) = ((i :=: (ChooseN 4 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_JSOp x2 x3 x4) = ((i :=: (ChooseN 5 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (C_JSFCall x2 x3) = ((i :=: (ChooseN 6 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_JSApply x2 x3) = ((i :=: (ChooseN 7 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_JSLambda x2 x3) = ((i :=: (ChooseN 8 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (Choice_C_JSExp cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_JSExp cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_JSExp cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_JSExp cd i _) = error ("JavaScript.JSExp.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_JSExp cd info) = [(Unsolvable info)]
  bind i (Guard_C_JSExp cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_JSString x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_JSInt x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_JSBool x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_JSIVar x2) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_JSIArrayIdx x2 x3) = [(i :=: (ChooseN 4 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_JSOp x2 x3 x4) = [(i :=: (ChooseN 5 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (C_JSFCall x2 x3) = [(i :=: (ChooseN 6 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_JSApply x2 x3) = [(i :=: (ChooseN 7 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_JSLambda x2 x3) = [(i :=: (ChooseN 8 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (Choice_C_JSExp cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_JSExp cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_JSExp cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_JSExp cd i _) = error ("JavaScript.JSExp.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_JSExp cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_JSExp cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_JSExp where
  (=?=) (Choice_C_JSExp cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_JSExp cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_JSExp cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_JSExp cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_JSExp cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_JSExp cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_JSExp cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_JSExp cd info) _ = failCons cd info
  (=?=) (C_JSString x1) (C_JSString y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_JSInt x1) (C_JSInt y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_JSBool x1) (C_JSBool y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_JSIVar x1) (C_JSIVar y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_JSIArrayIdx x1 x2) (C_JSIArrayIdx y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_JSOp x1 x2 x3) (C_JSOp y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (=?=) (C_JSFCall x1 x2) (C_JSFCall y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_JSApply x1 x2) (C_JSApply y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_JSLambda x1 x2) (C_JSLambda y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_JSExp cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_JSExp cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_JSExp cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_JSExp cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_JSExp cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_JSExp cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_JSExp cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_JSExp cd info) _ = failCons cd info
  (<?=) (C_JSString x1) (C_JSString y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_JSString _) (C_JSInt _) _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSBool _) _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSIVar _) _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSIArrayIdx _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSOp _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSFCall _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSApply _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSLambda _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSInt x1) (C_JSInt y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_JSInt _) (C_JSBool _) _ = Curry_Prelude.C_True
  (<?=) (C_JSInt _) (C_JSIVar _) _ = Curry_Prelude.C_True
  (<?=) (C_JSInt _) (C_JSIArrayIdx _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSInt _) (C_JSOp _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSInt _) (C_JSFCall _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSInt _) (C_JSApply _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSInt _) (C_JSLambda _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSBool x1) (C_JSBool y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_JSBool _) (C_JSIVar _) _ = Curry_Prelude.C_True
  (<?=) (C_JSBool _) (C_JSIArrayIdx _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSBool _) (C_JSOp _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSBool _) (C_JSFCall _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSBool _) (C_JSApply _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSBool _) (C_JSLambda _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSIVar x1) (C_JSIVar y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_JSIVar _) (C_JSIArrayIdx _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSIVar _) (C_JSOp _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSIVar _) (C_JSFCall _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSIVar _) (C_JSApply _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSIVar _) (C_JSLambda _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSIArrayIdx x1 x2) (C_JSIArrayIdx y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_JSIArrayIdx _ _) (C_JSOp _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSIArrayIdx _ _) (C_JSFCall _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSIArrayIdx _ _) (C_JSApply _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSIArrayIdx _ _) (C_JSLambda _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSOp x1 x2 x3) (C_JSOp y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs
  (<?=) (C_JSOp _ _ _) (C_JSFCall _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSOp _ _ _) (C_JSApply _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSOp _ _ _) (C_JSLambda _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSFCall x1 x2) (C_JSFCall y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_JSFCall _ _) (C_JSApply _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSFCall _ _) (C_JSLambda _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSApply x1 x2) (C_JSApply y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_JSApply _ _) (C_JSLambda _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSLambda x1 x2) (C_JSLambda y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_JSExp where
  cover (C_JSString x1) = C_JSString (cover x1)
  cover (C_JSInt x1) = C_JSInt (cover x1)
  cover (C_JSBool x1) = C_JSBool (cover x1)
  cover (C_JSIVar x1) = C_JSIVar (cover x1)
  cover (C_JSIArrayIdx x1 x2) = C_JSIArrayIdx (cover x1) (cover x2)
  cover (C_JSOp x1 x2 x3) = C_JSOp (cover x1) (cover x2) (cover x3)
  cover (C_JSFCall x1 x2) = C_JSFCall (cover x1) (cover x2)
  cover (C_JSApply x1 x2) = C_JSApply (cover x1) (cover x2)
  cover (C_JSLambda x1 x2) = C_JSLambda (cover x1) (cover x2)
  cover (Choice_C_JSExp cd i x y) = Choice_C_JSExp (incCover cd) i (cover x) (cover y)
  cover (Choices_C_JSExp cd i xs) = Choices_C_JSExp (incCover cd) i (map cover xs)
  cover (Fail_C_JSExp cd info) = Fail_C_JSExp (incCover cd) info
  cover (Guard_C_JSExp cd c e) = Guard_C_JSExp (incCover cd) c (cover e)


data C_JSStat
     = C_JSAssign C_JSExp C_JSExp
     | C_JSIf C_JSExp (Curry_Prelude.OP_List C_JSStat) (Curry_Prelude.OP_List C_JSStat)
     | C_JSSwitch C_JSExp (Curry_Prelude.OP_List C_JSBranch)
     | C_JSPCall (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List C_JSExp)
     | C_JSReturn C_JSExp
     | C_JSVarDecl Curry_Prelude.C_Int
     | Choice_C_JSStat Cover ID C_JSStat C_JSStat
     | Choices_C_JSStat Cover ID ([C_JSStat])
     | Fail_C_JSStat Cover FailInfo
     | Guard_C_JSStat Cover Constraints C_JSStat

instance Show C_JSStat where
  showsPrec d (Choice_C_JSStat cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_JSStat cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_JSStat cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_JSStat cd info) = showChar '!'
  showsPrec _ (C_JSAssign x1 x2) = (showString "(JSAssign") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_JSIf x1 x2 x3) = (showString "(JSIf") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_JSSwitch x1 x2) = (showString "(JSSwitch") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_JSPCall x1 x2) = (showString "(JSPCall") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_JSReturn x1) = (showString "(JSReturn") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_JSVarDecl x1) = (showString "(JSVarDecl") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_JSStat where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_JSAssign x1 x2,r2) | (_,r0) <- readQualified "JavaScript" "JSAssign" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_JSIf x1 x2 x3,r3) | (_,r0) <- readQualified "JavaScript" "JSIf" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ ((readParen (d > 10) (\r -> [ (C_JSSwitch x1 x2,r2) | (_,r0) <- readQualified "JavaScript" "JSSwitch" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_JSPCall x1 x2,r2) | (_,r0) <- readQualified "JavaScript" "JSPCall" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_JSReturn x1,r1) | (_,r0) <- readQualified "JavaScript" "JSReturn" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_JSVarDecl x1,r1) | (_,r0) <- readQualified "JavaScript" "JSVarDecl" r, (x1,r1) <- readsPrec 11 r0]) s)))))


instance NonDet C_JSStat where
  choiceCons = Choice_C_JSStat
  choicesCons = Choices_C_JSStat
  failCons = Fail_C_JSStat
  guardCons = Guard_C_JSStat
  try (Choice_C_JSStat cd i x y) = tryChoice cd i x y
  try (Choices_C_JSStat cd i xs) = tryChoices cd i xs
  try (Fail_C_JSStat cd info) = Fail cd info
  try (Guard_C_JSStat cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_JSStat cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_JSStat cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_JSStat cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_JSStat cd i _) = error ("JavaScript.JSStat.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_JSStat cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_JSStat cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_JSStat where
  generate s = Choices_C_JSStat defCover (freeID [2,3,2,2,1,1] s) [(C_JSAssign (generate (leftSupply s)) (generate (rightSupply s))),(C_JSIf (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s))),(C_JSSwitch (generate (leftSupply s)) (generate (rightSupply s))),(C_JSPCall (generate (leftSupply s)) (generate (rightSupply s))),(C_JSReturn (generate (leftSupply s))),(C_JSVarDecl (generate (leftSupply s)))]


instance NormalForm C_JSStat where
  ($!!) cont (C_JSAssign x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSAssign y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_JSIf x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_JSIf y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_JSSwitch x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSSwitch y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_JSPCall x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSPCall y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_JSReturn x1) cs = ((\y1 cs -> cont (C_JSReturn y1) cs) $!! x1) cs
  ($!!) cont (C_JSVarDecl x1) cs = ((\y1 cs -> cont (C_JSVarDecl y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_JSStat cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_JSStat cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_JSStat cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_JSStat cd info) _ = failCons cd info
  ($##) cont (C_JSAssign x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSAssign y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_JSIf x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_JSIf y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_JSSwitch x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSSwitch y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_JSPCall x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSPCall y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_JSReturn x1) cs = ((\y1 cs -> cont (C_JSReturn y1) cs) $## x1) cs
  ($##) cont (C_JSVarDecl x1) cs = ((\y1 cs -> cont (C_JSVarDecl y1) cs) $## x1) cs
  ($##) cont (Choice_C_JSStat cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_JSStat cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_JSStat cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_JSStat cd info) _ = failCons cd info
  searchNF search cont (C_JSAssign x1 x2) = search (\y1 -> search (\y2 -> cont (C_JSAssign y1 y2)) x2) x1
  searchNF search cont (C_JSIf x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_JSIf y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_JSSwitch x1 x2) = search (\y1 -> search (\y2 -> cont (C_JSSwitch y1 y2)) x2) x1
  searchNF search cont (C_JSPCall x1 x2) = search (\y1 -> search (\y2 -> cont (C_JSPCall y1 y2)) x2) x1
  searchNF search cont (C_JSReturn x1) = search (\y1 -> cont (C_JSReturn y1)) x1
  searchNF search cont (C_JSVarDecl x1) = search (\y1 -> cont (C_JSVarDecl y1)) x1
  searchNF _ _ x = error ("JavaScript.JSStat.searchNF: no constructor: " ++ (show x))


instance Unifiable C_JSStat where
  (=.=) (C_JSAssign x1 x2) (C_JSAssign y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_JSIf x1 x2 x3) (C_JSIf y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) (C_JSSwitch x1 x2) (C_JSSwitch y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_JSPCall x1 x2) (C_JSPCall y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_JSReturn x1) (C_JSReturn y1) cs = (x1 =:= y1) cs
  (=.=) (C_JSVarDecl x1) (C_JSVarDecl y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_JSAssign x1 x2) (C_JSAssign y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_JSIf x1 x2 x3) (C_JSIf y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) (C_JSSwitch x1 x2) (C_JSSwitch y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_JSPCall x1 x2) (C_JSPCall y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_JSReturn x1) (C_JSReturn y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_JSVarDecl x1) (C_JSVarDecl y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_JSAssign x2 x3) = ((i :=: (ChooseN 0 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_JSIf x2 x3 x4) = ((i :=: (ChooseN 1 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (C_JSSwitch x2 x3) = ((i :=: (ChooseN 2 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_JSPCall x2 x3) = ((i :=: (ChooseN 3 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_JSReturn x2) = ((i :=: (ChooseN 4 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_JSVarDecl x2) = ((i :=: (ChooseN 5 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_JSStat cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_JSStat cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_JSStat cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_JSStat cd i _) = error ("JavaScript.JSStat.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_JSStat cd info) = [(Unsolvable info)]
  bind i (Guard_C_JSStat cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_JSAssign x2 x3) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_JSIf x2 x3 x4) = [(i :=: (ChooseN 1 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (C_JSSwitch x2 x3) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_JSPCall x2 x3) = [(i :=: (ChooseN 3 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_JSReturn x2) = [(i :=: (ChooseN 4 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_JSVarDecl x2) = [(i :=: (ChooseN 5 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_JSStat cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_JSStat cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_JSStat cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_JSStat cd i _) = error ("JavaScript.JSStat.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_JSStat cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_JSStat cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_JSStat where
  (=?=) (Choice_C_JSStat cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_JSStat cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_JSStat cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_JSStat cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_JSStat cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_JSStat cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_JSStat cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_JSStat cd info) _ = failCons cd info
  (=?=) (C_JSAssign x1 x2) (C_JSAssign y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_JSIf x1 x2 x3) (C_JSIf y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (=?=) (C_JSSwitch x1 x2) (C_JSSwitch y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_JSPCall x1 x2) (C_JSPCall y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_JSReturn x1) (C_JSReturn y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_JSVarDecl x1) (C_JSVarDecl y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_JSStat cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_JSStat cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_JSStat cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_JSStat cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_JSStat cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_JSStat cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_JSStat cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_JSStat cd info) _ = failCons cd info
  (<?=) (C_JSAssign x1 x2) (C_JSAssign y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_JSAssign _ _) (C_JSIf _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSAssign _ _) (C_JSSwitch _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSAssign _ _) (C_JSPCall _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSAssign _ _) (C_JSReturn _) _ = Curry_Prelude.C_True
  (<?=) (C_JSAssign _ _) (C_JSVarDecl _) _ = Curry_Prelude.C_True
  (<?=) (C_JSIf x1 x2 x3) (C_JSIf y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs
  (<?=) (C_JSIf _ _ _) (C_JSSwitch _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSIf _ _ _) (C_JSPCall _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSIf _ _ _) (C_JSReturn _) _ = Curry_Prelude.C_True
  (<?=) (C_JSIf _ _ _) (C_JSVarDecl _) _ = Curry_Prelude.C_True
  (<?=) (C_JSSwitch x1 x2) (C_JSSwitch y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_JSSwitch _ _) (C_JSPCall _ _) _ = Curry_Prelude.C_True
  (<?=) (C_JSSwitch _ _) (C_JSReturn _) _ = Curry_Prelude.C_True
  (<?=) (C_JSSwitch _ _) (C_JSVarDecl _) _ = Curry_Prelude.C_True
  (<?=) (C_JSPCall x1 x2) (C_JSPCall y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_JSPCall _ _) (C_JSReturn _) _ = Curry_Prelude.C_True
  (<?=) (C_JSPCall _ _) (C_JSVarDecl _) _ = Curry_Prelude.C_True
  (<?=) (C_JSReturn x1) (C_JSReturn y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_JSReturn _) (C_JSVarDecl _) _ = Curry_Prelude.C_True
  (<?=) (C_JSVarDecl x1) (C_JSVarDecl y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_JSStat where
  cover (C_JSAssign x1 x2) = C_JSAssign (cover x1) (cover x2)
  cover (C_JSIf x1 x2 x3) = C_JSIf (cover x1) (cover x2) (cover x3)
  cover (C_JSSwitch x1 x2) = C_JSSwitch (cover x1) (cover x2)
  cover (C_JSPCall x1 x2) = C_JSPCall (cover x1) (cover x2)
  cover (C_JSReturn x1) = C_JSReturn (cover x1)
  cover (C_JSVarDecl x1) = C_JSVarDecl (cover x1)
  cover (Choice_C_JSStat cd i x y) = Choice_C_JSStat (incCover cd) i (cover x) (cover y)
  cover (Choices_C_JSStat cd i xs) = Choices_C_JSStat (incCover cd) i (map cover xs)
  cover (Fail_C_JSStat cd info) = Fail_C_JSStat (incCover cd) info
  cover (Guard_C_JSStat cd c e) = Guard_C_JSStat (incCover cd) c (cover e)


data C_JSBranch
     = C_JSCase (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List C_JSStat)
     | C_JSDefault (Curry_Prelude.OP_List C_JSStat)
     | Choice_C_JSBranch Cover ID C_JSBranch C_JSBranch
     | Choices_C_JSBranch Cover ID ([C_JSBranch])
     | Fail_C_JSBranch Cover FailInfo
     | Guard_C_JSBranch Cover Constraints C_JSBranch

instance Show C_JSBranch where
  showsPrec d (Choice_C_JSBranch cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_JSBranch cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_JSBranch cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_JSBranch cd info) = showChar '!'
  showsPrec _ (C_JSCase x1 x2) = (showString "(JSCase") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_JSDefault x1) = (showString "(JSDefault") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_JSBranch where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_JSCase x1 x2,r2) | (_,r0) <- readQualified "JavaScript" "JSCase" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_JSDefault x1,r1) | (_,r0) <- readQualified "JavaScript" "JSDefault" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet C_JSBranch where
  choiceCons = Choice_C_JSBranch
  choicesCons = Choices_C_JSBranch
  failCons = Fail_C_JSBranch
  guardCons = Guard_C_JSBranch
  try (Choice_C_JSBranch cd i x y) = tryChoice cd i x y
  try (Choices_C_JSBranch cd i xs) = tryChoices cd i xs
  try (Fail_C_JSBranch cd info) = Fail cd info
  try (Guard_C_JSBranch cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_JSBranch cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_JSBranch cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_JSBranch cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_JSBranch cd i _) = error ("JavaScript.JSBranch.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_JSBranch cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_JSBranch cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_JSBranch where
  generate s = Choices_C_JSBranch defCover (freeID [2,1] s) [(C_JSCase (generate (leftSupply s)) (generate (rightSupply s))),(C_JSDefault (generate (leftSupply s)))]


instance NormalForm C_JSBranch where
  ($!!) cont (C_JSCase x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSCase y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_JSDefault x1) cs = ((\y1 cs -> cont (C_JSDefault y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_JSBranch cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_JSBranch cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_JSBranch cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_JSBranch cd info) _ = failCons cd info
  ($##) cont (C_JSCase x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_JSCase y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_JSDefault x1) cs = ((\y1 cs -> cont (C_JSDefault y1) cs) $## x1) cs
  ($##) cont (Choice_C_JSBranch cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_JSBranch cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_JSBranch cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_JSBranch cd info) _ = failCons cd info
  searchNF search cont (C_JSCase x1 x2) = search (\y1 -> search (\y2 -> cont (C_JSCase y1 y2)) x2) x1
  searchNF search cont (C_JSDefault x1) = search (\y1 -> cont (C_JSDefault y1)) x1
  searchNF _ _ x = error ("JavaScript.JSBranch.searchNF: no constructor: " ++ (show x))


instance Unifiable C_JSBranch where
  (=.=) (C_JSCase x1 x2) (C_JSCase y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_JSDefault x1) (C_JSDefault y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_JSCase x1 x2) (C_JSCase y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_JSDefault x1) (C_JSDefault y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_JSCase x2 x3) = ((i :=: (ChooseN 0 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_JSDefault x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_JSBranch cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_JSBranch cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_JSBranch cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_JSBranch cd i _) = error ("JavaScript.JSBranch.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_JSBranch cd info) = [(Unsolvable info)]
  bind i (Guard_C_JSBranch cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_JSCase x2 x3) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_JSDefault x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_JSBranch cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_JSBranch cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_JSBranch cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_JSBranch cd i _) = error ("JavaScript.JSBranch.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_JSBranch cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_JSBranch cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_JSBranch where
  (=?=) (Choice_C_JSBranch cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_JSBranch cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_JSBranch cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_JSBranch cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_JSBranch cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_JSBranch cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_JSBranch cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_JSBranch cd info) _ = failCons cd info
  (=?=) (C_JSCase x1 x2) (C_JSCase y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_JSDefault x1) (C_JSDefault y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_JSBranch cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_JSBranch cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_JSBranch cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_JSBranch cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_JSBranch cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_JSBranch cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_JSBranch cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_JSBranch cd info) _ = failCons cd info
  (<?=) (C_JSCase x1 x2) (C_JSCase y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_JSCase _ _) (C_JSDefault _) _ = Curry_Prelude.C_True
  (<?=) (C_JSDefault x1) (C_JSDefault y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_JSBranch where
  cover (C_JSCase x1 x2) = C_JSCase (cover x1) (cover x2)
  cover (C_JSDefault x1) = C_JSDefault (cover x1)
  cover (Choice_C_JSBranch cd i x y) = Choice_C_JSBranch (incCover cd) i (cover x) (cover y)
  cover (Choices_C_JSBranch cd i xs) = Choices_C_JSBranch (incCover cd) i (map cover xs)
  cover (Fail_C_JSBranch cd info) = Fail_C_JSBranch (incCover cd) info
  cover (Guard_C_JSBranch cd c e) = Guard_C_JSBranch (incCover cd) c (cover e)


data C_JSFDecl
     = C_JSFDecl (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List C_JSStat)
     | Choice_C_JSFDecl Cover ID C_JSFDecl C_JSFDecl
     | Choices_C_JSFDecl Cover ID ([C_JSFDecl])
     | Fail_C_JSFDecl Cover FailInfo
     | Guard_C_JSFDecl Cover Constraints C_JSFDecl

instance Show C_JSFDecl where
  showsPrec d (Choice_C_JSFDecl cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_JSFDecl cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_JSFDecl cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_JSFDecl cd info) = showChar '!'
  showsPrec _ (C_JSFDecl x1 x2 x3) = (showString "(JSFDecl") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_JSFDecl where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_JSFDecl x1 x2 x3,r3) | (_,r0) <- readQualified "JavaScript" "JSFDecl" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_JSFDecl where
  choiceCons = Choice_C_JSFDecl
  choicesCons = Choices_C_JSFDecl
  failCons = Fail_C_JSFDecl
  guardCons = Guard_C_JSFDecl
  try (Choice_C_JSFDecl cd i x y) = tryChoice cd i x y
  try (Choices_C_JSFDecl cd i xs) = tryChoices cd i xs
  try (Fail_C_JSFDecl cd info) = Fail cd info
  try (Guard_C_JSFDecl cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_JSFDecl cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_JSFDecl cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_JSFDecl cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_JSFDecl cd i _) = error ("JavaScript.JSFDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_JSFDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_JSFDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_JSFDecl where
  generate s = Choices_C_JSFDecl defCover (freeID [3] s) [(C_JSFDecl (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s)))]


instance NormalForm C_JSFDecl where
  ($!!) cont (C_JSFDecl x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_JSFDecl y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_JSFDecl cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_JSFDecl cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_JSFDecl cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_JSFDecl cd info) _ = failCons cd info
  ($##) cont (C_JSFDecl x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_JSFDecl y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_JSFDecl cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_JSFDecl cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_JSFDecl cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_JSFDecl cd info) _ = failCons cd info
  searchNF search cont (C_JSFDecl x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_JSFDecl y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("JavaScript.JSFDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_JSFDecl where
  (=.=) (C_JSFDecl x1 x2 x3) (C_JSFDecl y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_JSFDecl x1 x2 x3) (C_JSFDecl y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_JSFDecl x2 x3 x4) = ((i :=: (ChooseN 0 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (Choice_C_JSFDecl cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_JSFDecl cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_JSFDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_JSFDecl cd i _) = error ("JavaScript.JSFDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_JSFDecl cd info) = [(Unsolvable info)]
  bind i (Guard_C_JSFDecl cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_JSFDecl x2 x3 x4) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (Choice_C_JSFDecl cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_JSFDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_JSFDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_JSFDecl cd i _) = error ("JavaScript.JSFDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_JSFDecl cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_JSFDecl cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_JSFDecl where
  (=?=) (Choice_C_JSFDecl cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_JSFDecl cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_JSFDecl cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_JSFDecl cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_JSFDecl cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_JSFDecl cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_JSFDecl cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_JSFDecl cd info) _ = failCons cd info
  (=?=) (C_JSFDecl x1 x2 x3) (C_JSFDecl y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (<?=) (Choice_C_JSFDecl cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_JSFDecl cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_JSFDecl cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_JSFDecl cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_JSFDecl cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_JSFDecl cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_JSFDecl cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_JSFDecl cd info) _ = failCons cd info
  (<?=) (C_JSFDecl x1 x2 x3) (C_JSFDecl y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs


instance Coverable C_JSFDecl where
  cover (C_JSFDecl x1 x2 x3) = C_JSFDecl (cover x1) (cover x2) (cover x3)
  cover (Choice_C_JSFDecl cd i x y) = Choice_C_JSFDecl (incCover cd) i (cover x) (cover y)
  cover (Choices_C_JSFDecl cd i xs) = Choices_C_JSFDecl (incCover cd) i (map cover xs)
  cover (Fail_C_JSFDecl cd info) = Fail_C_JSFDecl (incCover cd) info
  cover (Guard_C_JSFDecl cd c e) = Guard_C_JSFDecl (incCover cd) c (cover e)


d_C_showJSExp :: C_JSExp -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showJSExp x1 x3500 = case x1 of
     (C_JSString x2) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3500) x3500
     (C_JSInt x3) -> Curry_Prelude.d_C_show x3 x3500
     (C_JSBool x4) -> d_OP__case_1 x4 x3500
     (C_JSIVar x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_show x5 x3500) x3500
     (C_JSIArrayIdx x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x7 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500
     (C_JSOp x8 x9 x10) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x9 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x8 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x10 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500) x3500) x3500
     (C_JSFCall x11 x12) -> Curry_Prelude.d_OP_plus_plus x11 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map d_C_showJSExp x12 x3500) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (C_JSApply x13 x14) -> Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x13 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x14 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (C_JSLambda x15 x16) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot d_C_showJSExp (acceptCs id C_JSIVar) x3500) x15 x3500) x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_showJSStat (Curry_Prelude.C_Int 1#)) x3500) x16 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3500) x3500) x3500) x3500
     (Choice_C_JSExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showJSExp x1002 x3500) (d_C_showJSExp x1003 x3500)
     (Choices_C_JSExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showJSExp z x3500) x1002
     (Guard_C_JSExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showJSExp x1002) $! (addCs x1001 x3500))
     (Fail_C_JSExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showJSStat :: Curry_Prelude.C_Int -> C_JSStat -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showJSStat x1 x2 x3500 = case x2 of
     (C_JSAssign x3 x4) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x3 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x4 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500
     (C_JSIf x5 x6 x7) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x5 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (d_C_showJSStat (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3500)) x3500) x3500) x6 x3500) (d_OP__case_0 x1 x7 (Curry_Prelude.d_C_null x7 x3500) x3500) x3500) x3500) x3500) x3500) x3500
     (C_JSSwitch x8 x9) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x8 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_OP_showJSStat_dot_showJSBranch_dot_27 x1) x3500) x9 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500) x3500) x3500
     (C_JSPCall x10 x11) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.d_OP_plus_plus x10 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map d_C_showJSExp x11 x3500) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500
     (C_JSReturn x12) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x12 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (C_JSVarDecl x13) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x13 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Choice_C_JSStat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showJSStat x1 x1002 x3500) (d_C_showJSStat x1 x1003 x3500)
     (Choices_C_JSStat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showJSStat x1 z x3500) x1002
     (Guard_C_JSStat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showJSStat x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_JSStat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showJSStat_dot_showJSBranch_dot_27 :: Curry_Prelude.C_Int -> C_JSBranch -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showJSStat_dot_showJSBranch_dot_27 x1 x2 x3500 = case x2 of
     (C_JSCase x3 x4) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (d_C_showJSStat (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 4#) x3500)) x3500) x3500) x4 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_blanks (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 4#) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))) x3500) x3500) x3500) x3500) x3500) x3500
     (C_JSDefault x5) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (d_C_showJSStat (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 4#) x3500)) x3500) x3500) x5 x3500) x3500) x3500
     (Choice_C_JSBranch x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showJSStat_dot_showJSBranch_dot_27 x1 x1002 x3500) (d_OP_showJSStat_dot_showJSBranch_dot_27 x1 x1003 x3500)
     (Choices_C_JSBranch x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showJSStat_dot_showJSBranch_dot_27 x1 z x3500) x1002
     (Guard_C_JSBranch x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showJSStat_dot_showJSBranch_dot_27 x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_JSBranch x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_blanks :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_blanks x1 x3500 = Curry_Prelude.d_C_replicate x1 (Curry_Prelude.C_Char ' '#) x3500

d_C_showJSFDecl :: C_JSFDecl -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showJSFDecl x1 x3500 = case x1 of
     (C_JSFDecl x2 x3 x4) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map d_C_showJSExp (Curry_Prelude.d_C_map (acceptCs id C_JSIVar) x3 x3500) x3500) x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (d_C_showJSStat (Curry_Prelude.C_Int 2#)) x3500) x3500) x4 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3500) x3500) x3500) x3500) x3500) x3500
     (Choice_C_JSFDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showJSFDecl x1002 x3500) (d_C_showJSFDecl x1003 x3500)
     (Choices_C_JSFDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showJSFDecl z x3500) x1002
     (Guard_C_JSFDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showJSFDecl x1002) $! (addCs x1001 x3500))
     (Fail_C_JSFDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_jsConsTerm :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_JSExp -> ConstStore -> C_JSExp
d_C_jsConsTerm x1 x2 x3500 = C_JSFCall (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.OP_Cons (C_JSString x1) x2)

d_OP__case_0 x1 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (d_C_showJSStat (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3500)) x3500) x3500) x7 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x7 x1002 x3500) (d_OP__case_0 x1 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3500
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_concatMap (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_plus_plus)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) (wrapDX id (d_C_showJSStat (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3500))) x2000 x3500) x2001 x3500)))) x7 x2003 x3500)))) (Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x7 x1002 x3000 x3500) (nd_OP__case_0 x1 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1002 x3500) (d_OP__case_1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1002 x3000 x3500) (nd_OP__case_1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
