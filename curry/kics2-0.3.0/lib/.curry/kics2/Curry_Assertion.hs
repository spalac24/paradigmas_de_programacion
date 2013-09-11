{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Assertion (C_ProtocolMsg (..), C_Assertion, d_C_assertTrue, nd_C_assertTrue, d_C_assertEqual, nd_C_assertEqual, d_C_assertValues, nd_C_assertValues, d_C_assertSolutions, nd_C_assertSolutions, d_C_assertIO, nd_C_assertIO, d_C_assertEqualIO, nd_C_assertEqualIO, d_C_seqStrActions, nd_C_checkAssertion, d_C_writeAssertResult, d_C_showTestMod, d_C_showTestCase, d_C_showTestEnd, d_C_showTestCompileError) where

import Basics
import qualified Curry_AllSolutions
import qualified Curry_IO
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_Socket
data C_Assertion t0
     = C_AssertTrue (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool
     | C_AssertEqual (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 t0
     | C_AssertValues (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 (Curry_Prelude.OP_List t0)
     | C_AssertSolutions (Curry_Prelude.OP_List Curry_Prelude.C_Char) (t0 -> Cover -> ConstStore -> Curry_Prelude.C_Success) (Curry_Prelude.OP_List t0)
     | HO_C_AssertSolutions (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func t0 Curry_Prelude.C_Success) (Curry_Prelude.OP_List t0)
     | C_AssertIO (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t0) t0
     | C_AssertEqualIO (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t0) (Curry_Prelude.C_IO t0)
     | Choice_C_Assertion Cover ID (C_Assertion t0) (C_Assertion t0)
     | Choices_C_Assertion Cover ID ([C_Assertion t0])
     | Fail_C_Assertion Cover FailInfo
     | Guard_C_Assertion Cover Constraints (C_Assertion t0)

instance Show t0 => Show (C_Assertion t0) where
  showsPrec d (Choice_C_Assertion cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Assertion cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Assertion cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Assertion cd info) = showChar '!'
  showsPrec _ (C_AssertTrue x1 x2) = (showString "(AssertTrue") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_AssertEqual x1 x2 x3) = (showString "(AssertEqual") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_AssertValues x1 x2 x3) = (showString "(AssertValues") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_AssertSolutions x1 x2 x3) = (showString "(AssertSolutions") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (HO_C_AssertSolutions x1 x2 x3) = (showString "(AssertSolutions") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_AssertIO x1 x2 x3) = (showString "(AssertIO") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_AssertEqualIO x1 x2 x3) = (showString "(AssertEqualIO") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read t0 => Read (C_Assertion t0) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_AssertTrue x1 x2,r2) | (_,r0) <- readQualified "Assertion" "AssertTrue" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_AssertEqual x1 x2 x3,r3) | (_,r0) <- readQualified "Assertion" "AssertEqual" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ ((readParen (d > 10) (\r -> [ (C_AssertValues x1 x2 x3,r3) | (_,r0) <- readQualified "Assertion" "AssertValues" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ ((readParen (d > 10) (\r -> [ (C_AssertSolutions x1 x2 x3,r3) | (_,r0) <- readQualified "Assertion" "AssertSolutions" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ ((readParen (d > 10) (\r -> [ (C_AssertIO x1 x2 x3,r3) | (_,r0) <- readQualified "Assertion" "AssertIO" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ (readParen (d > 10) (\r -> [ (C_AssertEqualIO x1 x2 x3,r3) | (_,r0) <- readQualified "Assertion" "AssertEqualIO" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s)))))


instance NonDet (C_Assertion t0) where
  choiceCons = Choice_C_Assertion
  choicesCons = Choices_C_Assertion
  failCons = Fail_C_Assertion
  guardCons = Guard_C_Assertion
  try (Choice_C_Assertion cd i x y) = tryChoice cd i x y
  try (Choices_C_Assertion cd i xs) = tryChoices cd i xs
  try (Fail_C_Assertion cd info) = Fail cd info
  try (Guard_C_Assertion cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Assertion cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Assertion cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Assertion cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Assertion cd i _) = error ("Assertion.Assertion.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Assertion cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Assertion cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_Assertion t0) where
  generate s c = Choices_C_Assertion c (freeID [2,3,3,3,3,3] s) [(C_AssertTrue (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_AssertEqual (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_AssertValues (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(HO_C_AssertSolutions (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_AssertIO (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_AssertEqualIO (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance NormalForm t0 => NormalForm (C_Assertion t0) where
  ($!!) cont (C_AssertTrue x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_AssertTrue y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_AssertEqual x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AssertEqual y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_AssertValues x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AssertValues y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_AssertSolutions x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AssertSolutions y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_AssertSolutions x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (HO_C_AssertSolutions y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_AssertIO x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AssertIO y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_AssertEqualIO x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AssertEqualIO y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Assertion cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Assertion cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Assertion cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Assertion cd info) _ _ = failCons cd info
  ($##) cont (C_AssertTrue x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_AssertTrue y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_AssertEqual x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AssertEqual y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_AssertValues x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AssertValues y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_AssertSolutions x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AssertSolutions y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_AssertSolutions x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (HO_C_AssertSolutions y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_AssertIO x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AssertIO y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_AssertEqualIO x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_AssertEqualIO y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Assertion cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Assertion cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Assertion cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Assertion cd info) _ _ = failCons cd info
  searchNF search cont (C_AssertTrue x1 x2) = search (\y1 -> search (\y2 -> cont (C_AssertTrue y1 y2)) x2) x1
  searchNF search cont (C_AssertEqual x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_AssertEqual y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_AssertValues x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_AssertValues y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_AssertSolutions x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_AssertSolutions y1 y2 y3)) x3) x2) x1
  searchNF search cont (HO_C_AssertSolutions x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (HO_C_AssertSolutions y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_AssertIO x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_AssertIO y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_AssertEqualIO x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_AssertEqualIO y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("Assertion.Assertion.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_Assertion t0) where
  (=.=) (C_AssertTrue x1 x2) (C_AssertTrue y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_AssertEqual x1 x2 x3) (C_AssertEqual y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_AssertValues x1 x2 x3) (C_AssertValues y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_AssertSolutions x1 x2 x3) (C_AssertSolutions y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (HO_C_AssertSolutions x1 x2 x3) (HO_C_AssertSolutions y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_AssertIO x1 x2 x3) (C_AssertIO y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_AssertEqualIO x1 x2 x3) (C_AssertEqualIO y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_AssertTrue x1 x2) (C_AssertTrue y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_AssertEqual x1 x2 x3) (C_AssertEqual y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_AssertValues x1 x2 x3) (C_AssertValues y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_AssertSolutions x1 x2 x3) (C_AssertSolutions y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (HO_C_AssertSolutions x1 x2 x3) (HO_C_AssertSolutions y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_AssertIO x1 x2 x3) (C_AssertIO y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_AssertEqualIO x1 x2 x3) (C_AssertEqualIO y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_AssertTrue x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_AssertEqual x3 x4 x5) = ((i :=: (ChooseN 1 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_AssertValues x3 x4 x5) = ((i :=: (ChooseN 2 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_AssertSolutions x3 x4 x5) = ((i :=: (ChooseN 3 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (HO_C_AssertSolutions x3 x4 x5) = ((i :=: (ChooseN 3 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_AssertIO x3 x4 x5) = ((i :=: (ChooseN 4 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_AssertEqualIO x3 x4 x5) = ((i :=: (ChooseN 5 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_Assertion cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Assertion cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Assertion cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Assertion cd i _) = error ("Assertion.Assertion.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Assertion cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Assertion cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_AssertTrue x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_AssertEqual x3 x4 x5) = [(i :=: (ChooseN 1 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_AssertValues x3 x4 x5) = [(i :=: (ChooseN 2 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_AssertSolutions x3 x4 x5) = [(i :=: (ChooseN 3 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (HO_C_AssertSolutions x3 x4 x5) = [(i :=: (ChooseN 3 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_AssertIO x3 x4 x5) = [(i :=: (ChooseN 4 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_AssertEqualIO x3 x4 x5) = [(i :=: (ChooseN 5 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_Assertion cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Assertion cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Assertion cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Assertion cd i _) = error ("Assertion.Assertion.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Assertion cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Assertion cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_Assertion t0) where
  (=?=) (Choice_C_Assertion cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Assertion cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Assertion cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Assertion cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Assertion cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Assertion cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Assertion cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Assertion cd info) _ _ = failCons cd info
  (=?=) (C_AssertTrue x1 x2) (C_AssertTrue y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_AssertEqual x1 x2 x3) (C_AssertEqual y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_AssertValues x1 x2 x3) (C_AssertValues y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_AssertSolutions x1 x2 x3) (C_AssertSolutions y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (HO_C_AssertSolutions x1 x2 x3) (HO_C_AssertSolutions y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_AssertIO x1 x2 x3) (C_AssertIO y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_AssertEqualIO x1 x2 x3) (C_AssertEqualIO y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Assertion cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Assertion cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Assertion cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Assertion cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Assertion cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Assertion cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Assertion cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Assertion cd info) _ _ = failCons cd info
  (<?=) (C_AssertTrue x1 x2) (C_AssertTrue y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_AssertTrue _ _) (C_AssertEqual _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertTrue _ _) (C_AssertValues _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertTrue _ _) (C_AssertSolutions _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertTrue _ _) (HO_C_AssertSolutions _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertTrue _ _) (C_AssertIO _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertTrue _ _) (C_AssertEqualIO _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertEqual x1 x2 x3) (C_AssertEqual y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_AssertEqual _ _ _) (C_AssertValues _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertEqual _ _ _) (C_AssertSolutions _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertEqual _ _ _) (HO_C_AssertSolutions _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertEqual _ _ _) (C_AssertIO _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertEqual _ _ _) (C_AssertEqualIO _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertValues x1 x2 x3) (C_AssertValues y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_AssertValues _ _ _) (C_AssertSolutions _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertValues _ _ _) (HO_C_AssertSolutions _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertValues _ _ _) (C_AssertIO _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertValues _ _ _) (C_AssertEqualIO _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertSolutions x1 x2 x3) (C_AssertSolutions y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_AssertSolutions _ _ _) (C_AssertIO _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertSolutions _ _ _) (C_AssertEqualIO _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_AssertSolutions x1 x2 x3) (HO_C_AssertSolutions y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (HO_C_AssertSolutions _ _ _) (C_AssertIO _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_AssertSolutions _ _ _) (C_AssertEqualIO _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertIO x1 x2 x3) (C_AssertIO y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_AssertIO _ _ _) (C_AssertEqualIO _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_AssertEqualIO x1 x2 x3) (C_AssertEqualIO y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_ProtocolMsg
     = C_TestModule (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_TestCase (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool
     | C_TestFinished
     | C_TestCompileError
     | Choice_C_ProtocolMsg Cover ID C_ProtocolMsg C_ProtocolMsg
     | Choices_C_ProtocolMsg Cover ID ([C_ProtocolMsg])
     | Fail_C_ProtocolMsg Cover FailInfo
     | Guard_C_ProtocolMsg Cover Constraints C_ProtocolMsg

instance Show C_ProtocolMsg where
  showsPrec d (Choice_C_ProtocolMsg cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ProtocolMsg cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ProtocolMsg cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ProtocolMsg cd info) = showChar '!'
  showsPrec _ (C_TestModule x1) = (showString "(TestModule") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_TestCase x1 x2) = (showString "(TestCase") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ C_TestFinished = showString "TestFinished"
  showsPrec _ C_TestCompileError = showString "TestCompileError"


instance Read C_ProtocolMsg where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_TestModule x1,r1) | (_,r0) <- readQualified "Assertion" "TestModule" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_TestCase x1 x2,r2) | (_,r0) <- readQualified "Assertion" "TestCase" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen False (\r -> [ (C_TestFinished,r0) | (_,r0) <- readQualified "Assertion" "TestFinished" r]) s) ++ (readParen False (\r -> [ (C_TestCompileError,r0) | (_,r0) <- readQualified "Assertion" "TestCompileError" r]) s)))


instance NonDet C_ProtocolMsg where
  choiceCons = Choice_C_ProtocolMsg
  choicesCons = Choices_C_ProtocolMsg
  failCons = Fail_C_ProtocolMsg
  guardCons = Guard_C_ProtocolMsg
  try (Choice_C_ProtocolMsg cd i x y) = tryChoice cd i x y
  try (Choices_C_ProtocolMsg cd i xs) = tryChoices cd i xs
  try (Fail_C_ProtocolMsg cd info) = Fail cd info
  try (Guard_C_ProtocolMsg cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ProtocolMsg cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ProtocolMsg cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ProtocolMsg cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ProtocolMsg cd i _) = error ("Assertion.ProtocolMsg.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ProtocolMsg cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ProtocolMsg cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_ProtocolMsg where
  generate s c = Choices_C_ProtocolMsg c (freeID [1,2,0,0] s) [(C_TestModule (generate (leftSupply s) c)),(C_TestCase (generate (leftSupply s) c) (generate (rightSupply s) c)),C_TestFinished,C_TestCompileError]


instance NormalForm C_ProtocolMsg where
  ($!!) cont (C_TestModule x1) d cs = (((\y1 d cs -> cont (C_TestModule y1) d cs) $!! x1) d) cs
  ($!!) cont (C_TestCase x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_TestCase y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont C_TestFinished d cs = cont C_TestFinished d cs
  ($!!) cont C_TestCompileError d cs = cont C_TestCompileError d cs
  ($!!) cont (Choice_C_ProtocolMsg cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_ProtocolMsg cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_ProtocolMsg cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_ProtocolMsg cd info) _ _ = failCons cd info
  ($##) cont (C_TestModule x1) d cs = (((\y1 d cs -> cont (C_TestModule y1) d cs) $## x1) d) cs
  ($##) cont (C_TestCase x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_TestCase y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont C_TestFinished d cs = cont C_TestFinished d cs
  ($##) cont C_TestCompileError d cs = cont C_TestCompileError d cs
  ($##) cont (Choice_C_ProtocolMsg cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_ProtocolMsg cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_ProtocolMsg cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_ProtocolMsg cd info) _ _ = failCons cd info
  searchNF search cont (C_TestModule x1) = search (\y1 -> cont (C_TestModule y1)) x1
  searchNF search cont (C_TestCase x1 x2) = search (\y1 -> search (\y2 -> cont (C_TestCase y1 y2)) x2) x1
  searchNF _ cont C_TestFinished = cont C_TestFinished
  searchNF _ cont C_TestCompileError = cont C_TestCompileError
  searchNF _ _ x = error ("Assertion.ProtocolMsg.searchNF: no constructor: " ++ (show x))


instance Unifiable C_ProtocolMsg where
  (=.=) (C_TestModule x1) (C_TestModule y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_TestCase x1 x2) (C_TestCase y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) C_TestFinished C_TestFinished d cs = C_Success
  (=.=) C_TestCompileError C_TestCompileError d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_TestModule x1) (C_TestModule y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_TestCase x1 x2) (C_TestCase y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) C_TestFinished C_TestFinished d cs = C_Success
  (=.<=) C_TestCompileError C_TestCompileError d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_TestModule x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_TestCase x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i C_TestFinished = ((i :=: (ChooseN 2 0)):(concat []))
  bind cd i C_TestCompileError = ((i :=: (ChooseN 3 0)):(concat []))
  bind d i (Choice_C_ProtocolMsg cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_ProtocolMsg cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_ProtocolMsg cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_ProtocolMsg cd i _) = error ("Assertion.ProtocolMsg.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_ProtocolMsg cd info) = [(Unsolvable info)]
  bind d i (Guard_C_ProtocolMsg cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_TestModule x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_TestCase x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i C_TestFinished = [(i :=: (ChooseN 2 0))]
  lazyBind cd i C_TestCompileError = [(i :=: (ChooseN 3 0))]
  lazyBind d i (Choice_C_ProtocolMsg cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_ProtocolMsg cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_ProtocolMsg cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_ProtocolMsg cd i _) = error ("Assertion.ProtocolMsg.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_ProtocolMsg cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_ProtocolMsg cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_ProtocolMsg where
  (=?=) (Choice_C_ProtocolMsg cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_ProtocolMsg cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_ProtocolMsg cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_ProtocolMsg cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_ProtocolMsg cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_ProtocolMsg cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_ProtocolMsg cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_ProtocolMsg cd info) _ _ = failCons cd info
  (=?=) (C_TestModule x1) (C_TestModule y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_TestCase x1 x2) (C_TestCase y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) C_TestFinished C_TestFinished d cs = Curry_Prelude.C_True
  (=?=) C_TestCompileError C_TestCompileError d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_ProtocolMsg cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_ProtocolMsg cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_ProtocolMsg cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_ProtocolMsg cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_ProtocolMsg cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_ProtocolMsg cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_ProtocolMsg cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_ProtocolMsg cd info) _ _ = failCons cd info
  (<?=) (C_TestModule x1) (C_TestModule y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_TestModule _) (C_TestCase _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_TestModule _) C_TestFinished _ _ = Curry_Prelude.C_True
  (<?=) (C_TestModule _) C_TestCompileError _ _ = Curry_Prelude.C_True
  (<?=) (C_TestCase x1 x2) (C_TestCase y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_TestCase _ _) C_TestFinished _ _ = Curry_Prelude.C_True
  (<?=) (C_TestCase _ _) C_TestCompileError _ _ = Curry_Prelude.C_True
  (<?=) C_TestFinished C_TestFinished d cs = Curry_Prelude.C_True
  (<?=) C_TestFinished C_TestCompileError _ _ = Curry_Prelude.C_True
  (<?=) C_TestCompileError C_TestCompileError d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_assertTrue :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Assertion Curry_Prelude.OP_Unit
d_C_assertTrue x1 x2 x3250 x3500 = C_AssertTrue x1 x2

nd_C_assertTrue :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_Assertion Curry_Prelude.OP_Unit
nd_C_assertTrue x1 x2 x3000 x3250 x3500 = C_AssertTrue x1 x2

d_C_assertEqual :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> t0 -> Cover -> ConstStore -> C_Assertion t0
d_C_assertEqual x1 x2 x3 x3250 x3500 = C_AssertEqual x1 x2 x3

nd_C_assertEqual :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> t0 -> IDSupply -> Cover -> ConstStore -> C_Assertion t0
nd_C_assertEqual x1 x2 x3 x3000 x3250 x3500 = C_AssertEqual x1 x2 x3

d_C_assertValues :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> C_Assertion t0
d_C_assertValues x1 x2 x3 x3250 x3500 = C_AssertValues x1 x2 x3

nd_C_assertValues :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> C_Assertion t0
nd_C_assertValues x1 x2 x3 x3000 x3250 x3500 = C_AssertValues x1 x2 x3

d_C_assertSolutions :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> (t0 -> Cover -> ConstStore -> Curry_Prelude.C_Success) -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> C_Assertion t0
d_C_assertSolutions x1 x2 x3 x3250 x3500 = C_AssertSolutions x1 x2 x3

nd_C_assertSolutions :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 Curry_Prelude.C_Success -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> C_Assertion t0
nd_C_assertSolutions x1 x2 x3 x3000 x3250 x3500 = HO_C_AssertSolutions x1 x2 x3

d_C_assertIO :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO t0 -> t0 -> Cover -> ConstStore -> C_Assertion t0
d_C_assertIO x1 x2 x3 x3250 x3500 = C_AssertIO x1 x2 x3

nd_C_assertIO :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO t0 -> t0 -> IDSupply -> Cover -> ConstStore -> C_Assertion t0
nd_C_assertIO x1 x2 x3 x3000 x3250 x3500 = C_AssertIO x1 x2 x3

d_C_assertEqualIO :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO t0 -> Curry_Prelude.C_IO t0 -> Cover -> ConstStore -> C_Assertion t0
d_C_assertEqualIO x1 x2 x3 x3250 x3500 = C_AssertEqualIO x1 x2 x3

nd_C_assertEqualIO :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO t0 -> Curry_Prelude.C_IO t0 -> IDSupply -> Cover -> ConstStore -> C_Assertion t0
nd_C_assertEqualIO x1 x2 x3 x3000 x3250 x3500 = C_AssertEqualIO x1 x2 x3

d_C_seqStrActions :: Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_C_seqStrActions x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq x1 (d_OP_seqStrActions_dot___hash_lambda1 x2) x3250 x3500

d_OP_seqStrActions_dot___hash_lambda1 :: Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP_seqStrActions_dot___hash_lambda1 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.d_OP_gt_gt_eq x1 (d_OP_seqStrActions_dot___hash_lambda1_dot___hash_lambda2 x4 x3) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_seqStrActions_dot___hash_lambda1 x1 x1002 x3250 x3500) (d_OP_seqStrActions_dot___hash_lambda1 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_seqStrActions_dot___hash_lambda1 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_seqStrActions_dot___hash_lambda1 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_seqStrActions_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP_seqStrActions_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus x2 x4 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand x1 x5 x3250 x3500)) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_seqStrActions_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x1002 x3250 x3500) (d_OP_seqStrActions_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_seqStrActions_dot___hash_lambda1_dot___hash_lambda2 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_seqStrActions_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_checkAssertion :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) (Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> C_Assertion t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
nd_C_checkAssertion x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_catchNDIO x1 x2 (nd_OP_checkAssertion_dot_execAsrt_dot_18 x2 x3 x2000 x3250 x3500) x2001 x3250 x3500)))))

d_OP_checkAssertion_dot_returnError_dot_18 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IOError -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP_checkAssertion_dot_returnError_dot_18 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_showError x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x3250 x3500

nd_OP_checkAssertion_dot_execAsrt_dot_18 :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) (Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> C_Assertion t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
nd_OP_checkAssertion_dot_execAsrt_dot_18 x1 x2 x3000 x3250 x3500 = case x2 of
     (C_AssertTrue x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.nd_C_catch (d_C_checkAssertTrue x3 x4 x3250 x3500) (wrapDX id (d_OP_checkAssertion_dot_returnError_dot_18 x3)) x2000 x3250 x3500) x1 x2001 x3250 x3500)))))
     (C_AssertEqual x5 x6 x7) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.nd_C_catch (d_C_checkAssertEqual x5 x6 x7 x3250 x3500) (wrapDX id (d_OP_checkAssertion_dot_returnError_dot_18 x5)) x2000 x3250 x3500) x1 x2001 x3250 x3500)))))
     (C_AssertValues x8 x9 x10) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.nd_C_catch (d_C_checkAssertValues x8 x9 x10 x3250 x3500) (wrapDX id (d_OP_checkAssertion_dot_returnError_dot_18 x8)) x2000 x3250 x3500) x1 x2001 x3250 x3500)))))
     (HO_C_AssertSolutions x11 x12 x13) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_catch (nd_C_checkAssertSolutions x11 x12 x13 x2000 x3250 x3500) (wrapDX id (d_OP_checkAssertion_dot_returnError_dot_18 x11)) x2001 x3250 x3500)))) x1 x2003 x3250 x3500)))))
     (C_AssertIO x14 x15 x16) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.nd_C_catch (d_C_checkAssertIO x14 x15 x16 x3250 x3500) (wrapDX id (d_OP_checkAssertion_dot_returnError_dot_18 x14)) x2000 x3250 x3500) x1 x2001 x3250 x3500)))))
     (C_AssertEqualIO x17 x18 x19) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.nd_C_catch (d_C_checkAssertEqualIO x17 x18 x19 x3250 x3500) (wrapDX id (d_OP_checkAssertion_dot_returnError_dot_18 x17)) x2000 x3250 x3500) x1 x2001 x3250 x3500)))))
     (Choice_C_Assertion x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_checkAssertion_dot_execAsrt_dot_18 x1 x1002 x3000 x3250 x3500) (nd_OP_checkAssertion_dot_execAsrt_dot_18 x1 x1003 x3000 x3250 x3500)
     (Choices_C_Assertion x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_checkAssertion_dot_execAsrt_dot_18 x1 z x3000 x3250 x3500) x1002
     (Guard_C_Assertion x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_checkAssertion_dot_execAsrt_dot_18 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Assertion x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_catchNDIO :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_C_catchNDIO x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_AllSolutions.d_C_getAllValues x3 x3250 x3500) (d_OP_catchNDIO_dot_checkIOActions_dot_34 x1 x2) x3250 x3500

nd_C_catchNDIO :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) (Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
nd_C_catchNDIO x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_AllSolutions.d_C_getAllValues x3 x3250 x3500) (wrapNX id (nd_OP_catchNDIO_dot_checkIOActions_dot_34 x1 x2)) x2000 x3250 x3500))

d_OP_catchNDIO_dot_checkIOActions_dot_34 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Curry_Prelude.OP_List (Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP_catchNDIO_dot_checkIOActions_dot_34 x1 x2 x3 x3250 x3500 = d_OP__case_15 x3 x1 x2 (Curry_Prelude.d_C_null x3 x3250 x3500) x3250 x3500

nd_OP_catchNDIO_dot_checkIOActions_dot_34 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) (Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Curry_Prelude.OP_List (Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
nd_OP_catchNDIO_dot_checkIOActions_dot_34 x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_15 x3 x1 x2 (Curry_Prelude.d_C_null x3 x3250 x3500) x2000 x3250 x3500))

d_C_checkAssertTrue :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_C_checkAssertTrue x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_AllSolutions.d_C_getAllValues x2 x3250 x3500) (d_OP_checkAssertTrue_dot_checkResults_dot_38 x1) x3250 x3500

d_OP_checkAssertTrue_dot_checkResults_dot_38 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP_checkAssertTrue_dot_checkResults_dot_38 x1 x2 x3250 x3500 = d_OP__case_12 x2 x1 (Curry_Prelude.d_C_null x2 x3250 x3500) x3250 x3500

d_C_checkAssertEqual :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_C_checkAssertEqual x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_AllSolutions.d_C_getAllValues (Curry_Prelude.d_OP_eq_eq x2 x3 x3250 x3500) x3250 x3500) (d_OP_checkAssertEqual_dot_checkResults_dot_42 x2 x1 x3) x3250 x3500

d_OP_checkAssertEqual_dot_checkResults_dot_42 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP_checkAssertEqual_dot_checkResults_dot_42 x1 x2 x3 x4 x3250 x3500 = d_OP__case_8 x4 x3 x1 x2 (Curry_Prelude.d_C_null x4 x3250 x3500) x3250 x3500

d_C_checkAssertValues :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_C_checkAssertValues x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_AllSolutions.d_C_getAllValues x2 x3250 x3500) (d_OP_checkAssertValues_dot___hash_lambda3 x1 x3) x3250 x3500

d_OP_checkAssertValues_dot___hash_lambda3 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP_checkAssertValues_dot___hash_lambda3 x1 x2 x3 x3250 x3500 = d_OP__case_4 x3 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_null (Curry_List.d_OP_backslash_backslash x3 x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_null (Curry_List.d_OP_backslash_backslash x2 x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

nd_C_checkAssertSolutions :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 Curry_Prelude.C_Success -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
nd_C_checkAssertSolutions x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_AllSolutions.nd_C_getAllSolutions x2 x2000 x3250 x3500) (wrapDX id (d_OP_checkAssertSolutions_dot___hash_lambda4 x1 x3)) x2001 x3250 x3500)))))

d_OP_checkAssertSolutions_dot___hash_lambda4 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP_checkAssertSolutions_dot___hash_lambda4 x1 x2 x3 x3250 x3500 = d_OP__case_3 x3 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_null (Curry_List.d_OP_backslash_backslash x3 x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_null (Curry_List.d_OP_backslash_backslash x2 x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_C_checkAssertIO :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO t0 -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_C_checkAssertIO x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq x2 (d_OP_checkAssertIO_dot___hash_lambda5 x1 x3) x3250 x3500

d_OP_checkAssertIO_dot___hash_lambda5 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP_checkAssertIO_dot___hash_lambda5 x1 x2 x3 x3250 x3500 = d_OP__case_2 x2 x3 x1 (Curry_Prelude.d_OP_eq_eq x3 x2 x3250 x3500) x3250 x3500

d_C_checkAssertEqualIO :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO t0 -> Curry_Prelude.C_IO t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_C_checkAssertEqualIO x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq x2 (d_OP_checkAssertEqualIO_dot___hash_lambda6 x3 x1) x3250 x3500

d_OP_checkAssertEqualIO_dot___hash_lambda6 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IO t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP_checkAssertEqualIO_dot___hash_lambda6 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq x1 (d_OP_checkAssertEqualIO_dot___hash_lambda6_dot___hash_lambda7 x2 x3) x3250 x3500

d_OP_checkAssertEqualIO_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP_checkAssertEqualIO_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3 x3250 x3500 = d_OP__case_1 x3 x2 x1 (Curry_Prelude.d_OP_eq_eq x2 x3 x3250 x3500) x3250 x3500

d_C_writeAssertResult :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_writeAssertResult x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_0 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_writeAssertResult x1002 x3250 x3500) (d_C_writeAssertResult x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_writeAssertResult z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_writeAssertResult x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showTestMod :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_showTestMod x1 x2 x3250 x3500 = d_C_sendToLocalSocket x1 (C_TestModule x2) x3250 x3500

d_C_showTestCase :: Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_C_showTestCase x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.d_OP_gt_gt (d_C_sendToLocalSocket x1 (C_TestCase x3 x4) x3250 x3500) (Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x3 x4) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showTestCase x1 x1002 x3250 x3500) (d_C_showTestCase x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showTestCase x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showTestCase x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showTestEnd :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_showTestEnd x1 x3250 x3500 = d_C_sendToLocalSocket x1 C_TestFinished x3250 x3500

d_C_showTestCompileError :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_showTestCompileError x1 x3250 x3500 = d_C_sendToLocalSocket x1 C_TestCompileError x3250 x3500

d_C_sendToLocalSocket :: Curry_Prelude.C_Int -> C_ProtocolMsg -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_sendToLocalSocket x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Socket.d_C_connectToSocket (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))) x1 x3250 x3500) (d_OP_sendToLocalSocket_dot___hash_lambda8 x2) x3250 x3500

d_OP_sendToLocalSocket_dot___hash_lambda8 :: C_ProtocolMsg -> Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_sendToLocalSocket_dot___hash_lambda8 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_IO.d_C_hPutStrLn x2 (Curry_Prelude.d_C_show x1 x3250 x3500) x3250 x3500) (Curry_IO.d_C_hClose x2 x3250 x3500) x3250 x3500

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_OP__case_0 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x1002 x3250 x3500) (d_OP__case_0 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_1 x3 x2 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'K'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) Curry_Prelude.C_True) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x3 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_1 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_2 x2 x3 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'K'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) Curry_Prelude.C_True) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x2 x3 x1 x1002 x3250 x3500) (d_OP__case_2 x2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_3 x3 x2 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'K'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) Curry_Prelude.C_True) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_3 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_4 x3 x2 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'K'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) Curry_Prelude.C_True) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_4 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Bool -> t0 -> t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_8 x4 x3 x1 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_7 x4 x3 x1 x2 (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null (Curry_Prelude.d_C_tail x4 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x4 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_8 x4 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x4 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x4 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Bool -> t0 -> t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_7 x4 x3 x1 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_6 x4 x3 x1 x2 (Curry_Prelude.d_C_head x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x4 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_7 x4 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x4 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x4 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Bool -> t0 -> t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_6 x4 x3 x1 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'K'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) Curry_Prelude.C_True) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_5 x3 x1 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x4 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_6 x4 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x4 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x4 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_5 x3 x1 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x3 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_5 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.OP_List Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_12 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_11 x2 x1 (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null (Curry_Prelude.d_C_tail x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x2 x1 x1002 x3250 x3500) (d_OP__case_12 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.OP_List Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_11 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_10 x2 x1 (Curry_Prelude.d_C_head x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x2 x1 x1002 x3250 x3500) (d_OP__case_11 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.OP_List Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_10 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'K'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) Curry_Prelude.C_True) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_9 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x2 x1 x1002 x3250 x3500) (d_OP__case_10 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_9 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x1002 x3250 x3500) (d_OP__case_9 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.OP_List (Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_15 x3 x1 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply x2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_14 x3 x1 x2 (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null (Curry_Prelude.d_C_tail x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_15 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_15 :: Curry_Prelude.OP_List (Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) (Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
nd_OP__case_15 x3 x1 x2 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_apply x2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x3 x1 x2 (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null (Curry_Prelude.d_C_tail x3 x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x3 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_15 x3 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x3 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x3 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.OP_List (Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_14 x3 x1 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply x2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_13 x3 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_14 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_14 :: Curry_Prelude.OP_List (Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) (Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
nd_OP__case_14 x3 x1 x2 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_apply x2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500) Curry_Prelude.C_False) x2000 x3250 x3500))
     Curry_Prelude.C_False -> d_OP__case_13 x3 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x3 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_14 x3 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x3 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x3 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.OP_List (Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
d_OP__case_13 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_head x3 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x3 x1002 x3250 x3500) (d_OP__case_13 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
