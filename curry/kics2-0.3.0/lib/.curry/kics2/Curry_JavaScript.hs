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
  generate s c = Choices_C_JSExp c (freeID [1,1,1,1,2,3,2,2,2] s) [(C_JSString (generate (leftSupply s) c)),(C_JSInt (generate (leftSupply s) c)),(C_JSBool (generate (leftSupply s) c)),(C_JSIVar (generate (leftSupply s) c)),(C_JSIArrayIdx (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_JSOp (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_JSFCall (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_JSApply (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_JSLambda (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_JSExp where
  ($!!) cont (C_JSString x1) d cs = (((\y1 d cs -> cont (C_JSString y1) d cs) $!! x1) d) cs
  ($!!) cont (C_JSInt x1) d cs = (((\y1 d cs -> cont (C_JSInt y1) d cs) $!! x1) d) cs
  ($!!) cont (C_JSBool x1) d cs = (((\y1 d cs -> cont (C_JSBool y1) d cs) $!! x1) d) cs
  ($!!) cont (C_JSIVar x1) d cs = (((\y1 d cs -> cont (C_JSIVar y1) d cs) $!! x1) d) cs
  ($!!) cont (C_JSIArrayIdx x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSIArrayIdx y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_JSOp x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_JSOp y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_JSFCall x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSFCall y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_JSApply x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSApply y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_JSLambda x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSLambda y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_JSExp cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_JSExp cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_JSExp cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_JSExp cd info) _ _ = failCons cd info
  ($##) cont (C_JSString x1) d cs = (((\y1 d cs -> cont (C_JSString y1) d cs) $## x1) d) cs
  ($##) cont (C_JSInt x1) d cs = (((\y1 d cs -> cont (C_JSInt y1) d cs) $## x1) d) cs
  ($##) cont (C_JSBool x1) d cs = (((\y1 d cs -> cont (C_JSBool y1) d cs) $## x1) d) cs
  ($##) cont (C_JSIVar x1) d cs = (((\y1 d cs -> cont (C_JSIVar y1) d cs) $## x1) d) cs
  ($##) cont (C_JSIArrayIdx x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSIArrayIdx y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_JSOp x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_JSOp y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_JSFCall x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSFCall y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_JSApply x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSApply y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_JSLambda x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSLambda y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_JSExp cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_JSExp cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_JSExp cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_JSExp cd info) _ _ = failCons cd info
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
  (=.=) (C_JSString x1) (C_JSString y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_JSInt x1) (C_JSInt y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_JSBool x1) (C_JSBool y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_JSIVar x1) (C_JSIVar y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_JSIArrayIdx x1 x2) (C_JSIArrayIdx y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_JSOp x1 x2 x3) (C_JSOp y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_JSFCall x1 x2) (C_JSFCall y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_JSApply x1 x2) (C_JSApply y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_JSLambda x1 x2) (C_JSLambda y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_JSString x1) (C_JSString y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_JSInt x1) (C_JSInt y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_JSBool x1) (C_JSBool y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_JSIVar x1) (C_JSIVar y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_JSIArrayIdx x1 x2) (C_JSIArrayIdx y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_JSOp x1 x2 x3) (C_JSOp y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_JSFCall x1 x2) (C_JSFCall y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_JSApply x1 x2) (C_JSApply y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_JSLambda x1 x2) (C_JSLambda y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_JSString x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_JSInt x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_JSBool x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_JSIVar x3) = ((i :=: (ChooseN 3 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_JSIArrayIdx x3 x4) = ((i :=: (ChooseN 4 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_JSOp x3 x4 x5) = ((i :=: (ChooseN 5 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_JSFCall x3 x4) = ((i :=: (ChooseN 6 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_JSApply x3 x4) = ((i :=: (ChooseN 7 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_JSLambda x3 x4) = ((i :=: (ChooseN 8 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_JSExp cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_JSExp cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_JSExp cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_JSExp cd i _) = error ("JavaScript.JSExp.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_JSExp cd info) = [(Unsolvable info)]
  bind d i (Guard_C_JSExp cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_JSString x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_JSInt x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_JSBool x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_JSIVar x3) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_JSIArrayIdx x3 x4) = [(i :=: (ChooseN 4 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_JSOp x3 x4 x5) = [(i :=: (ChooseN 5 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_JSFCall x3 x4) = [(i :=: (ChooseN 6 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_JSApply x3 x4) = [(i :=: (ChooseN 7 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_JSLambda x3 x4) = [(i :=: (ChooseN 8 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_JSExp cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_JSExp cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_JSExp cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_JSExp cd i _) = error ("JavaScript.JSExp.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_JSExp cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_JSExp cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_JSExp where
  (=?=) (Choice_C_JSExp cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_JSExp cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_JSExp cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_JSExp cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_JSExp cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_JSExp cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_JSExp cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_JSExp cd info) _ _ = failCons cd info
  (=?=) (C_JSString x1) (C_JSString y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_JSInt x1) (C_JSInt y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_JSBool x1) (C_JSBool y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_JSIVar x1) (C_JSIVar y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_JSIArrayIdx x1 x2) (C_JSIArrayIdx y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_JSOp x1 x2 x3) (C_JSOp y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_JSFCall x1 x2) (C_JSFCall y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_JSApply x1 x2) (C_JSApply y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_JSLambda x1 x2) (C_JSLambda y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_JSExp cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_JSExp cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_JSExp cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_JSExp cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_JSExp cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_JSExp cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_JSExp cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_JSExp cd info) _ _ = failCons cd info
  (<?=) (C_JSString x1) (C_JSString y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_JSString _) (C_JSInt _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSBool _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSIVar _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSIArrayIdx _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSOp _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSFCall _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSApply _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSString _) (C_JSLambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSInt x1) (C_JSInt y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_JSInt _) (C_JSBool _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSInt _) (C_JSIVar _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSInt _) (C_JSIArrayIdx _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSInt _) (C_JSOp _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSInt _) (C_JSFCall _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSInt _) (C_JSApply _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSInt _) (C_JSLambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSBool x1) (C_JSBool y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_JSBool _) (C_JSIVar _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSBool _) (C_JSIArrayIdx _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSBool _) (C_JSOp _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSBool _) (C_JSFCall _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSBool _) (C_JSApply _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSBool _) (C_JSLambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSIVar x1) (C_JSIVar y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_JSIVar _) (C_JSIArrayIdx _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSIVar _) (C_JSOp _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSIVar _) (C_JSFCall _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSIVar _) (C_JSApply _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSIVar _) (C_JSLambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSIArrayIdx x1 x2) (C_JSIArrayIdx y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_JSIArrayIdx _ _) (C_JSOp _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSIArrayIdx _ _) (C_JSFCall _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSIArrayIdx _ _) (C_JSApply _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSIArrayIdx _ _) (C_JSLambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSOp x1 x2 x3) (C_JSOp y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_JSOp _ _ _) (C_JSFCall _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSOp _ _ _) (C_JSApply _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSOp _ _ _) (C_JSLambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSFCall x1 x2) (C_JSFCall y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_JSFCall _ _) (C_JSApply _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSFCall _ _) (C_JSLambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSApply x1 x2) (C_JSApply y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_JSApply _ _) (C_JSLambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSLambda x1 x2) (C_JSLambda y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


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
  generate s c = Choices_C_JSStat c (freeID [2,3,2,2,1,1] s) [(C_JSAssign (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_JSIf (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_JSSwitch (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_JSPCall (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_JSReturn (generate (leftSupply s) c)),(C_JSVarDecl (generate (leftSupply s) c))]


instance NormalForm C_JSStat where
  ($!!) cont (C_JSAssign x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSAssign y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_JSIf x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_JSIf y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_JSSwitch x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSSwitch y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_JSPCall x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSPCall y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_JSReturn x1) d cs = (((\y1 d cs -> cont (C_JSReturn y1) d cs) $!! x1) d) cs
  ($!!) cont (C_JSVarDecl x1) d cs = (((\y1 d cs -> cont (C_JSVarDecl y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_JSStat cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_JSStat cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_JSStat cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_JSStat cd info) _ _ = failCons cd info
  ($##) cont (C_JSAssign x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSAssign y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_JSIf x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_JSIf y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_JSSwitch x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSSwitch y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_JSPCall x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSPCall y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_JSReturn x1) d cs = (((\y1 d cs -> cont (C_JSReturn y1) d cs) $## x1) d) cs
  ($##) cont (C_JSVarDecl x1) d cs = (((\y1 d cs -> cont (C_JSVarDecl y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_JSStat cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_JSStat cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_JSStat cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_JSStat cd info) _ _ = failCons cd info
  searchNF search cont (C_JSAssign x1 x2) = search (\y1 -> search (\y2 -> cont (C_JSAssign y1 y2)) x2) x1
  searchNF search cont (C_JSIf x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_JSIf y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_JSSwitch x1 x2) = search (\y1 -> search (\y2 -> cont (C_JSSwitch y1 y2)) x2) x1
  searchNF search cont (C_JSPCall x1 x2) = search (\y1 -> search (\y2 -> cont (C_JSPCall y1 y2)) x2) x1
  searchNF search cont (C_JSReturn x1) = search (\y1 -> cont (C_JSReturn y1)) x1
  searchNF search cont (C_JSVarDecl x1) = search (\y1 -> cont (C_JSVarDecl y1)) x1
  searchNF _ _ x = error ("JavaScript.JSStat.searchNF: no constructor: " ++ (show x))


instance Unifiable C_JSStat where
  (=.=) (C_JSAssign x1 x2) (C_JSAssign y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_JSIf x1 x2 x3) (C_JSIf y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_JSSwitch x1 x2) (C_JSSwitch y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_JSPCall x1 x2) (C_JSPCall y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_JSReturn x1) (C_JSReturn y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_JSVarDecl x1) (C_JSVarDecl y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_JSAssign x1 x2) (C_JSAssign y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_JSIf x1 x2 x3) (C_JSIf y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_JSSwitch x1 x2) (C_JSSwitch y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_JSPCall x1 x2) (C_JSPCall y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_JSReturn x1) (C_JSReturn y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_JSVarDecl x1) (C_JSVarDecl y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_JSAssign x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_JSIf x3 x4 x5) = ((i :=: (ChooseN 1 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_JSSwitch x3 x4) = ((i :=: (ChooseN 2 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_JSPCall x3 x4) = ((i :=: (ChooseN 3 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_JSReturn x3) = ((i :=: (ChooseN 4 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_JSVarDecl x3) = ((i :=: (ChooseN 5 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_JSStat cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_JSStat cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_JSStat cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_JSStat cd i _) = error ("JavaScript.JSStat.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_JSStat cd info) = [(Unsolvable info)]
  bind d i (Guard_C_JSStat cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_JSAssign x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_JSIf x3 x4 x5) = [(i :=: (ChooseN 1 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_JSSwitch x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_JSPCall x3 x4) = [(i :=: (ChooseN 3 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_JSReturn x3) = [(i :=: (ChooseN 4 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_JSVarDecl x3) = [(i :=: (ChooseN 5 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_JSStat cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_JSStat cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_JSStat cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_JSStat cd i _) = error ("JavaScript.JSStat.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_JSStat cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_JSStat cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_JSStat where
  (=?=) (Choice_C_JSStat cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_JSStat cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_JSStat cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_JSStat cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_JSStat cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_JSStat cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_JSStat cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_JSStat cd info) _ _ = failCons cd info
  (=?=) (C_JSAssign x1 x2) (C_JSAssign y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_JSIf x1 x2 x3) (C_JSIf y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_JSSwitch x1 x2) (C_JSSwitch y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_JSPCall x1 x2) (C_JSPCall y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_JSReturn x1) (C_JSReturn y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_JSVarDecl x1) (C_JSVarDecl y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_JSStat cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_JSStat cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_JSStat cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_JSStat cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_JSStat cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_JSStat cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_JSStat cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_JSStat cd info) _ _ = failCons cd info
  (<?=) (C_JSAssign x1 x2) (C_JSAssign y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_JSAssign _ _) (C_JSIf _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSAssign _ _) (C_JSSwitch _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSAssign _ _) (C_JSPCall _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSAssign _ _) (C_JSReturn _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSAssign _ _) (C_JSVarDecl _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSIf x1 x2 x3) (C_JSIf y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_JSIf _ _ _) (C_JSSwitch _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSIf _ _ _) (C_JSPCall _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSIf _ _ _) (C_JSReturn _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSIf _ _ _) (C_JSVarDecl _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSSwitch x1 x2) (C_JSSwitch y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_JSSwitch _ _) (C_JSPCall _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSSwitch _ _) (C_JSReturn _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSSwitch _ _) (C_JSVarDecl _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSPCall x1 x2) (C_JSPCall y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_JSPCall _ _) (C_JSReturn _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSPCall _ _) (C_JSVarDecl _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSReturn x1) (C_JSReturn y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_JSReturn _) (C_JSVarDecl _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSVarDecl x1) (C_JSVarDecl y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


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
  generate s c = Choices_C_JSBranch c (freeID [2,1] s) [(C_JSCase (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_JSDefault (generate (leftSupply s) c))]


instance NormalForm C_JSBranch where
  ($!!) cont (C_JSCase x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSCase y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_JSDefault x1) d cs = (((\y1 d cs -> cont (C_JSDefault y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_JSBranch cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_JSBranch cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_JSBranch cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_JSBranch cd info) _ _ = failCons cd info
  ($##) cont (C_JSCase x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_JSCase y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_JSDefault x1) d cs = (((\y1 d cs -> cont (C_JSDefault y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_JSBranch cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_JSBranch cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_JSBranch cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_JSBranch cd info) _ _ = failCons cd info
  searchNF search cont (C_JSCase x1 x2) = search (\y1 -> search (\y2 -> cont (C_JSCase y1 y2)) x2) x1
  searchNF search cont (C_JSDefault x1) = search (\y1 -> cont (C_JSDefault y1)) x1
  searchNF _ _ x = error ("JavaScript.JSBranch.searchNF: no constructor: " ++ (show x))


instance Unifiable C_JSBranch where
  (=.=) (C_JSCase x1 x2) (C_JSCase y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_JSDefault x1) (C_JSDefault y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_JSCase x1 x2) (C_JSCase y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_JSDefault x1) (C_JSDefault y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_JSCase x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_JSDefault x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_JSBranch cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_JSBranch cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_JSBranch cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_JSBranch cd i _) = error ("JavaScript.JSBranch.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_JSBranch cd info) = [(Unsolvable info)]
  bind d i (Guard_C_JSBranch cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_JSCase x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_JSDefault x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_JSBranch cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_JSBranch cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_JSBranch cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_JSBranch cd i _) = error ("JavaScript.JSBranch.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_JSBranch cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_JSBranch cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_JSBranch where
  (=?=) (Choice_C_JSBranch cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_JSBranch cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_JSBranch cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_JSBranch cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_JSBranch cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_JSBranch cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_JSBranch cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_JSBranch cd info) _ _ = failCons cd info
  (=?=) (C_JSCase x1 x2) (C_JSCase y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_JSDefault x1) (C_JSDefault y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_JSBranch cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_JSBranch cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_JSBranch cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_JSBranch cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_JSBranch cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_JSBranch cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_JSBranch cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_JSBranch cd info) _ _ = failCons cd info
  (<?=) (C_JSCase x1 x2) (C_JSCase y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_JSCase _ _) (C_JSDefault _) _ _ = Curry_Prelude.C_True
  (<?=) (C_JSDefault x1) (C_JSDefault y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


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
  generate s c = Choices_C_JSFDecl c (freeID [3] s) [(C_JSFDecl (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance NormalForm C_JSFDecl where
  ($!!) cont (C_JSFDecl x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_JSFDecl y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_JSFDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_JSFDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_JSFDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_JSFDecl cd info) _ _ = failCons cd info
  ($##) cont (C_JSFDecl x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_JSFDecl y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_JSFDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_JSFDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_JSFDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_JSFDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_JSFDecl x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_JSFDecl y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("JavaScript.JSFDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_JSFDecl where
  (=.=) (C_JSFDecl x1 x2 x3) (C_JSFDecl y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_JSFDecl x1 x2 x3) (C_JSFDecl y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_JSFDecl x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_JSFDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_JSFDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_JSFDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_JSFDecl cd i _) = error ("JavaScript.JSFDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_JSFDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_JSFDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_JSFDecl x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_JSFDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_JSFDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_JSFDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_JSFDecl cd i _) = error ("JavaScript.JSFDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_JSFDecl cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_JSFDecl cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_JSFDecl where
  (=?=) (Choice_C_JSFDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_JSFDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_JSFDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_JSFDecl cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_JSFDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_JSFDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_JSFDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_JSFDecl cd info) _ _ = failCons cd info
  (=?=) (C_JSFDecl x1 x2 x3) (C_JSFDecl y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (<?=) (Choice_C_JSFDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_JSFDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_JSFDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_JSFDecl cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_JSFDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_JSFDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_JSFDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_JSFDecl cd info) _ _ = failCons cd info
  (<?=) (C_JSFDecl x1 x2 x3) (C_JSFDecl y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs


d_C_showJSExp :: C_JSExp -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showJSExp x1 x3250 x3500 = case x1 of
     (C_JSString x2) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     (C_JSInt x3) -> Curry_Prelude.d_C_show x3 x3250 x3500
     (C_JSBool x4) -> d_OP__case_1 x4 x3250 x3500
     (C_JSIVar x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_show x5 x3250 x3500) x3250 x3500
     (C_JSIArrayIdx x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x7 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (C_JSOp x8 x9 x10) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x9 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x8 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x10 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (C_JSFCall x11 x12) -> Curry_Prelude.d_OP_plus_plus x11 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map d_C_showJSExp x12 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (C_JSApply x13 x14) -> Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x13 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x14 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (C_JSLambda x15 x16) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot d_C_showJSExp (acceptCs id C_JSIVar) x3250 x3500) x15 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_showJSStat (Curry_Prelude.C_Int 1#)) x3250 x3500) x16 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_JSExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showJSExp x1002 x3250 x3500) (d_C_showJSExp x1003 x3250 x3500)
     (Choices_C_JSExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showJSExp z x3250 x3500) x1002
     (Guard_C_JSExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showJSExp x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_JSExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showJSStat :: Curry_Prelude.C_Int -> C_JSStat -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showJSStat x1 x2 x3250 x3500 = case x2 of
     (C_JSAssign x3 x4) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (C_JSIf x5 x6 x7) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x5 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (d_C_showJSStat (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3250 x3500)) x3250 x3500) x3250 x3500) x6 x3250 x3500) (d_OP__case_0 x7 x1 (Curry_Prelude.d_C_null x7 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (C_JSSwitch x8 x9) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x8 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_OP_showJSStat_dot_showJSBranch_dot_27 x1) x3250 x3500) x9 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (C_JSPCall x10 x11) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus x10 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map d_C_showJSExp x11 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (C_JSReturn x12) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showJSExp x12 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (C_JSVarDecl x13) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x13 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_JSStat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showJSStat x1 x1002 x3250 x3500) (d_C_showJSStat x1 x1003 x3250 x3500)
     (Choices_C_JSStat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showJSStat x1 z x3250 x3500) x1002
     (Guard_C_JSStat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showJSStat x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_JSStat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showJSStat_dot_showJSBranch_dot_27 :: Curry_Prelude.C_Int -> C_JSBranch -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showJSStat_dot_showJSBranch_dot_27 x1 x2 x3250 x3500 = case x2 of
     (C_JSCase x3 x4) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (d_C_showJSStat (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 4#) x3250 x3500)) x3250 x3500) x3250 x3500) x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_blanks (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 4#) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (C_JSDefault x5) -> Curry_Prelude.d_OP_plus_plus (d_C_blanks (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (d_C_showJSStat (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 4#) x3250 x3500)) x3250 x3500) x3250 x3500) x5 x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_JSBranch x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showJSStat_dot_showJSBranch_dot_27 x1 x1002 x3250 x3500) (d_OP_showJSStat_dot_showJSBranch_dot_27 x1 x1003 x3250 x3500)
     (Choices_C_JSBranch x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showJSStat_dot_showJSBranch_dot_27 x1 z x3250 x3500) x1002
     (Guard_C_JSBranch x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showJSStat_dot_showJSBranch_dot_27 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_JSBranch x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_blanks :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_blanks x1 x3250 x3500 = Curry_Prelude.d_C_replicate x1 (Curry_Prelude.C_Char ' '#) x3250 x3500

d_C_showJSFDecl :: C_JSFDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showJSFDecl x1 x3250 x3500 = case x1 of
     (C_JSFDecl x2 x3 x4) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map d_C_showJSExp (Curry_Prelude.d_C_map (acceptCs id C_JSIVar) x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (d_C_showJSStat (Curry_Prelude.C_Int 2#)) x3250 x3500) x3250 x3500) x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_JSFDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showJSFDecl x1002 x3250 x3500) (d_C_showJSFDecl x1003 x3250 x3500)
     (Choices_C_JSFDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showJSFDecl z x3250 x3500) x1002
     (Guard_C_JSFDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showJSFDecl x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_JSFDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_jsConsTerm :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_JSExp -> Cover -> ConstStore -> C_JSExp
d_C_jsConsTerm x1 x2 x3250 x3500 = C_JSFCall (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.OP_Cons (C_JSString x1) x2)

d_OP__case_0 :: Curry_Prelude.OP_List C_JSStat -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x7 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (d_C_showJSStat (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3250 x3500)) x3250 x3500) x3250 x3500) x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_blanks x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x7 x1 x1002 x3250 x3500) (d_OP__case_0 x7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1002 x3250 x3500) (d_OP__case_1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
