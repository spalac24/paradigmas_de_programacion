{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_TotallyDefined (C_Completeness (..), d_C_showSibling, nd_C_showSibling, d_C_siblingCons, nd_C_siblingCons, d_C_totalAnalysis, nd_C_totalAnalysis, d_C_showTotally, d_C_patCompAnalysis, nd_C_patCompAnalysis, d_C_showComplete) where

import Basics
import qualified Curry_Analysis
import qualified Curry_FlatCurry
import qualified Curry_FlatCurryGoodies
import qualified Curry_GenericProgInfo
import qualified Curry_List
import qualified Curry_Prelude
data C_Completeness
     = C_Complete
     | C_InComplete
     | C_InCompleteOr
     | Choice_C_Completeness Cover ID C_Completeness C_Completeness
     | Choices_C_Completeness Cover ID ([C_Completeness])
     | Fail_C_Completeness Cover FailInfo
     | Guard_C_Completeness Cover Constraints C_Completeness

instance Show C_Completeness where
  showsPrec d (Choice_C_Completeness cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Completeness cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Completeness cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Completeness cd info) = showChar '!'
  showsPrec _ C_Complete = showString "Complete"
  showsPrec _ C_InComplete = showString "InComplete"
  showsPrec _ C_InCompleteOr = showString "InCompleteOr"


instance Read C_Completeness where
  readsPrec _ s = (readParen False (\r -> [ (C_Complete,r0) | (_,r0) <- readQualified "TotallyDefined" "Complete" r]) s) ++ ((readParen False (\r -> [ (C_InComplete,r0) | (_,r0) <- readQualified "TotallyDefined" "InComplete" r]) s) ++ (readParen False (\r -> [ (C_InCompleteOr,r0) | (_,r0) <- readQualified "TotallyDefined" "InCompleteOr" r]) s))


instance NonDet C_Completeness where
  choiceCons = Choice_C_Completeness
  choicesCons = Choices_C_Completeness
  failCons = Fail_C_Completeness
  guardCons = Guard_C_Completeness
  try (Choice_C_Completeness cd i x y) = tryChoice cd i x y
  try (Choices_C_Completeness cd i xs) = tryChoices cd i xs
  try (Fail_C_Completeness cd info) = Fail cd info
  try (Guard_C_Completeness cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Completeness cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Completeness cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Completeness cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Completeness cd i _) = error ("TotallyDefined.Completeness.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Completeness cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Completeness cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Completeness where
  generate s c = Choices_C_Completeness c (freeID [0,0,0] s) [C_Complete,C_InComplete,C_InCompleteOr]


instance NormalForm C_Completeness where
  ($!!) cont C_Complete d cs = cont C_Complete d cs
  ($!!) cont C_InComplete d cs = cont C_InComplete d cs
  ($!!) cont C_InCompleteOr d cs = cont C_InCompleteOr d cs
  ($!!) cont (Choice_C_Completeness cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Completeness cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Completeness cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Completeness cd info) _ _ = failCons cd info
  ($##) cont C_Complete d cs = cont C_Complete d cs
  ($##) cont C_InComplete d cs = cont C_InComplete d cs
  ($##) cont C_InCompleteOr d cs = cont C_InCompleteOr d cs
  ($##) cont (Choice_C_Completeness cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Completeness cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Completeness cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Completeness cd info) _ _ = failCons cd info
  searchNF _ cont C_Complete = cont C_Complete
  searchNF _ cont C_InComplete = cont C_InComplete
  searchNF _ cont C_InCompleteOr = cont C_InCompleteOr
  searchNF _ _ x = error ("TotallyDefined.Completeness.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Completeness where
  (=.=) C_Complete C_Complete d cs = C_Success
  (=.=) C_InComplete C_InComplete d cs = C_Success
  (=.=) C_InCompleteOr C_InCompleteOr d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_Complete C_Complete d cs = C_Success
  (=.<=) C_InComplete C_InComplete d cs = C_Success
  (=.<=) C_InCompleteOr C_InCompleteOr d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_Complete = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_InComplete = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_InCompleteOr = ((i :=: (ChooseN 2 0)):(concat []))
  bind d i (Choice_C_Completeness cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Completeness cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Completeness cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Completeness cd i _) = error ("TotallyDefined.Completeness.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Completeness cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Completeness cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Complete = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_InComplete = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_InCompleteOr = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_Completeness cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Completeness cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Completeness cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Completeness cd i _) = error ("TotallyDefined.Completeness.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Completeness cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Completeness cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Completeness where
  (=?=) (Choice_C_Completeness cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Completeness cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Completeness cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Completeness cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Completeness cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Completeness cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Completeness cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Completeness cd info) _ _ = failCons cd info
  (=?=) C_Complete C_Complete d cs = Curry_Prelude.C_True
  (=?=) C_InComplete C_InComplete d cs = Curry_Prelude.C_True
  (=?=) C_InCompleteOr C_InCompleteOr d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Completeness cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Completeness cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Completeness cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Completeness cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Completeness cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Completeness cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Completeness cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Completeness cd info) _ _ = failCons cd info
  (<?=) C_Complete C_Complete d cs = Curry_Prelude.C_True
  (<?=) C_Complete C_InComplete _ _ = Curry_Prelude.C_True
  (<?=) C_Complete C_InCompleteOr _ _ = Curry_Prelude.C_True
  (<?=) C_InComplete C_InComplete d cs = Curry_Prelude.C_True
  (<?=) C_InComplete C_InCompleteOr _ _ = Curry_Prelude.C_True
  (<?=) C_InCompleteOr C_InCompleteOr d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_showSibling :: Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSibling x1 x3250 x3500 = Curry_Prelude.d_C_show

nd_C_showSibling :: Curry_Analysis.C_AOutFormat -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_showSibling x1 x3000 x3250 x3500 = wrapDX id Curry_Prelude.d_C_show

d_C_siblingCons :: Cover -> ConstStore -> Curry_Analysis.C_Analysis (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_siblingCons x3250 x3500 = Curry_Analysis.d_C_simpleConstructorAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))) (acceptCs id d_OP_siblingCons_dot_consNamesOfType_dot_5) x3250 x3500

nd_C_siblingCons :: IDSupply -> Cover -> ConstStore -> Curry_Analysis.C_Analysis (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_C_siblingCons x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Analysis.nd_C_simpleConstructorAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))) (wrapDX (wrapDX id) (acceptCs id d_OP_siblingCons_dot_consNamesOfType_dot_5)) x2000 x3250 x3500))

d_OP_siblingCons_dot_consNamesOfType_dot_5 :: Curry_FlatCurry.C_ConsDecl -> Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_siblingCons_dot_consNamesOfType_dot_5 x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Type x3 x4 x5 x6) -> Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_consName x3250 x3500) x1 x3250 x3500)) (Curry_Prelude.d_C_map (Curry_FlatCurryGoodies.d_C_consName x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_TypeSyn x7 x8 x9 x10) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_siblingCons_dot_consNamesOfType_dot_5 x1 x1002 x3250 x3500) (d_OP_siblingCons_dot_consNamesOfType_dot_5 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_siblingCons_dot_consNamesOfType_dot_5 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_siblingCons_dot_consNamesOfType_dot_5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_totalAnalysis :: Cover -> ConstStore -> Curry_Analysis.C_Analysis Curry_Prelude.C_Bool
d_C_totalAnalysis x3250 x3500 = Curry_Analysis.d_C_combinedDependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))) (d_C_patCompAnalysis x3250 x3500) Curry_Prelude.C_True (acceptCs (acceptCs id) d_C_analyseTotally) x3250 x3500

nd_C_totalAnalysis :: IDSupply -> Cover -> ConstStore -> Curry_Analysis.C_Analysis Curry_Prelude.C_Bool
nd_C_totalAnalysis x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Analysis.nd_C_combinedDependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))) (nd_C_patCompAnalysis x2000 x3250 x3500) Curry_Prelude.C_True (wrapDX (wrapDX (wrapNX id)) (acceptCs (acceptCs id) nd_C_analyseTotally)) x2001 x3250 x3500)))))

d_C_analyseTotally :: Curry_GenericProgInfo.C_ProgInfo C_Completeness -> Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_analyseTotally x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_maybe Curry_Prelude.C_False d_OP_analyseTotally_dot___hash_lambda1 (Curry_GenericProgInfo.d_C_lookupProgInfo (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3250 x3500) x2 x3250 x3500) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Prelude.d_C_snd x3250 x3500) x3 x3250 x3500) x3250 x3500

nd_C_analyseTotally :: Curry_GenericProgInfo.C_ProgInfo C_Completeness -> Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_C_analyseTotally x1 x2 x3 x3000 x3250 x3500 = let
     x2010 = x3000
      in (seq x2010 (let
          x2006 = leftSupply x2010
          x2009 = rightSupply x2010
           in (seq x2006 (seq x2009 (Curry_Prelude.d_OP_ampersand_ampersand (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_maybe Curry_Prelude.C_False (wrapDX id d_OP_analyseTotally_dot___hash_lambda1) (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_GenericProgInfo.nd_C_lookupProgInfo (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_funcName x2000 x3250 x3500) x2 x2001 x3250 x3500)))) x1 x2003 x3250 x3500)))) x2005 x3250 x3500)))) (let
               x2008 = leftSupply x2009
               x2007 = rightSupply x2009
                in (seq x2008 (seq x2007 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_all (wrapDX id Curry_Prelude.d_C_snd) x2007 x3250 x3500) x3 x2008 x3250 x3500)))) x3250 x3500)))))

d_OP_analyseTotally_dot___hash_lambda1 :: C_Completeness -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_analyseTotally_dot___hash_lambda1 x1 x3250 x3500 = Curry_Prelude.d_OP_eq_eq x1 C_Complete x3250 x3500

d_C_showTotally :: Curry_Analysis.C_AOutFormat -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTotally x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> d_OP__case_13 x1 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showTotally x1 x1002 x3250 x3500) (d_C_showTotally x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showTotally x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showTotally x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_patCompAnalysis :: Cover -> ConstStore -> Curry_Analysis.C_Analysis C_Completeness
d_C_patCompAnalysis x3250 x3500 = Curry_Analysis.d_C_combinedSimpleFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))) (d_C_siblingCons x3250 x3500) (acceptCs id d_C_analysePatComplete) x3250 x3500

nd_C_patCompAnalysis :: IDSupply -> Cover -> ConstStore -> Curry_Analysis.C_Analysis C_Completeness
nd_C_patCompAnalysis x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Analysis.nd_C_combinedSimpleFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))) (nd_C_siblingCons x2000 x3250 x3500) (wrapDX (wrapNX id) (acceptCs id nd_C_analysePatComplete)) x2001 x3250 x3500)))))

d_C_showComplete :: Curry_Analysis.C_AOutFormat -> C_Completeness -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showComplete x1 x2 x3250 x3500 = case x2 of
     C_Complete -> d_OP__case_12 x1 x3250 x3500
     C_InComplete -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))
     C_InCompleteOr -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'j'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))
     (Choice_C_Completeness x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showComplete x1 x1002 x3250 x3500) (d_C_showComplete x1 x1003 x3250 x3500)
     (Choices_C_Completeness x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showComplete x1 z x3250 x3500) x1002
     (Guard_C_Completeness x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showComplete x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Completeness x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_analysePatComplete :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> C_Completeness
d_C_analysePatComplete x1 x2 x3250 x3500 = d_OP_analysePatComplete_dot_anaFun_dot_43 x1 x2 x3250 x3500

nd_C_analysePatComplete :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_FlatCurry.C_FuncDecl -> IDSupply -> Cover -> ConstStore -> C_Completeness
nd_C_analysePatComplete x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_analysePatComplete_dot_anaFun_dot_43 x1 x2 x2000 x3250 x3500))

d_OP_analysePatComplete_dot_anaFun_dot_43 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> C_Completeness
d_OP_analysePatComplete_dot_anaFun_dot_43 x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Func x3 x4 x5 x6 x7) -> d_OP__case_11 x1 x7 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_analysePatComplete_dot_anaFun_dot_43 x1 x1002 x3250 x3500) (d_OP_analysePatComplete_dot_anaFun_dot_43 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_analysePatComplete_dot_anaFun_dot_43 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_analysePatComplete_dot_anaFun_dot_43 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_analysePatComplete_dot_anaFun_dot_43 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_FlatCurry.C_FuncDecl -> IDSupply -> Cover -> ConstStore -> C_Completeness
nd_OP_analysePatComplete_dot_anaFun_dot_43 x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Func x3 x4 x5 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_11 x1 x7 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_analysePatComplete_dot_anaFun_dot_43 x1 x1002 x3000 x3250 x3500) (nd_OP_analysePatComplete_dot_anaFun_dot_43 x1 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_analysePatComplete_dot_anaFun_dot_43 x1 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_analysePatComplete_dot_anaFun_dot_43 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isComplete :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> C_Completeness
d_C_isComplete x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Var x3) -> C_Complete
     (Curry_FlatCurry.C_Lit x4) -> C_Complete
     (Curry_FlatCurry.C_Comb x5 x6 x7) -> d_OP__case_10 x7 x6 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))) x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x7 x3250 x3500) (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Free x8 x9) -> C_Complete
     (Curry_FlatCurry.C_Let x10 x11) -> C_Complete
     (Curry_FlatCurry.C_Or x12 x13) -> d_C_combineOrResults (d_C_isComplete x1 x12 x3250 x3500) (d_C_isComplete x1 x13 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Case x14 x15 x16) -> d_OP__case_9 x1 x16 x3250 x3500
     (Curry_FlatCurry.C_Typed x24 x25) -> d_C_isComplete x1 x24 x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isComplete x1 x1002 x3250 x3500) (d_C_isComplete x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isComplete x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isComplete x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_isComplete :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_FlatCurry.C_Expr -> IDSupply -> Cover -> ConstStore -> C_Completeness
nd_C_isComplete x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Var x3) -> C_Complete
     (Curry_FlatCurry.C_Lit x4) -> C_Complete
     (Curry_FlatCurry.C_Comb x5 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x7 x6 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))) x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x7 x3250 x3500) (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_FlatCurry.C_Free x8 x9) -> C_Complete
     (Curry_FlatCurry.C_Let x10 x11) -> C_Complete
     (Curry_FlatCurry.C_Or x12 x13) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (d_C_combineOrResults (nd_C_isComplete x1 x12 x2000 x3250 x3500) (nd_C_isComplete x1 x13 x2001 x3250 x3500) x3250 x3500)))))
     (Curry_FlatCurry.C_Case x14 x15 x16) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x1 x16 x2000 x3250 x3500))
     (Curry_FlatCurry.C_Typed x24 x25) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_isComplete x1 x24 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_isComplete x1 x1002 x3000 x3250 x3500) (nd_C_isComplete x1 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_isComplete x1 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_isComplete x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_isComplete_dot_checkAllCons_dot_99 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> C_Completeness
d_OP_isComplete_dot_checkAllCons_dot_99 x1 x2 x3 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> C_Complete
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_6 x1 x5 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isComplete_dot_checkAllCons_dot_99 x1 x1002 x3 x3250 x3500) (d_OP_isComplete_dot_checkAllCons_dot_99 x1 x1003 x3 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isComplete_dot_checkAllCons_dot_99 x1 z x3 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isComplete_dot_checkAllCons_dot_99 x1 x1002 x3 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_isComplete_dot_checkAllCons_dot_99 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> IDSupply -> Cover -> ConstStore -> C_Completeness
nd_OP_isComplete_dot_checkAllCons_dot_99 x1 x2 x3 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> C_Complete
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x1 x5 x4 x3 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_isComplete_dot_checkAllCons_dot_99 x1 x1002 x3 x3000 x3250 x3500) (nd_OP_isComplete_dot_checkAllCons_dot_99 x1 x1003 x3 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_isComplete_dot_checkAllCons_dot_99 x1 z x3 x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_isComplete_dot_checkAllCons_dot_99 x1 x1002 x3 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_combineOrResults :: C_Completeness -> C_Completeness -> Cover -> ConstStore -> C_Completeness
d_C_combineOrResults x1 x2 x3250 x3500 = case x1 of
     C_Complete -> C_Complete
     C_InComplete -> d_OP__case_3 x2 x3250 x3500
     C_InCompleteOr -> d_OP__case_2 x2 x3250 x3500
     (Choice_C_Completeness x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_combineOrResults x1002 x2 x3250 x3500) (d_C_combineOrResults x1003 x2 x3250 x3500)
     (Choices_C_Completeness x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_combineOrResults z x2 x3250 x3500) x1002
     (Guard_C_Completeness x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_combineOrResults x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_Completeness x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_combineAndResults :: C_Completeness -> C_Completeness -> Cover -> ConstStore -> C_Completeness
d_C_combineAndResults x1 x2 x3250 x3500 = case x1 of
     C_InComplete -> C_InComplete
     C_Complete -> d_OP__case_1 x2 x3250 x3500
     C_InCompleteOr -> d_OP__case_0 x2 x3250 x3500
     (Choice_C_Completeness x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_combineAndResults x1002 x2 x3250 x3500) (d_C_combineAndResults x1003 x2 x3250 x3500)
     (Choices_C_Completeness x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_combineAndResults z x2 x3250 x3500) x1002
     (Guard_C_Completeness x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_combineAndResults x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_Completeness x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: C_Completeness -> Cover -> ConstStore -> C_Completeness
d_OP__case_0 x2 x3250 x3500 = case x2 of
     C_Complete -> C_InCompleteOr
     C_InComplete -> C_InComplete
     C_InCompleteOr -> C_InCompleteOr
     (Choice_C_Completeness x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1002 x3250 x3500) (d_OP__case_0 x1003 x3250 x3500)
     (Choices_C_Completeness x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 z x3250 x3500) x1002
     (Guard_C_Completeness x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Completeness x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: C_Completeness -> Cover -> ConstStore -> C_Completeness
d_OP__case_1 x2 x3250 x3500 = case x2 of
     C_Complete -> C_Complete
     C_InComplete -> C_InComplete
     C_InCompleteOr -> C_InCompleteOr
     (Choice_C_Completeness x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1002 x3250 x3500) (d_OP__case_1 x1003 x3250 x3500)
     (Choices_C_Completeness x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 z x3250 x3500) x1002
     (Guard_C_Completeness x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Completeness x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: C_Completeness -> Cover -> ConstStore -> C_Completeness
d_OP__case_2 x2 x3250 x3500 = case x2 of
     C_Complete -> C_Complete
     C_InComplete -> C_InCompleteOr
     C_InCompleteOr -> C_InCompleteOr
     (Choice_C_Completeness x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1002 x3250 x3500) (d_OP__case_2 x1003 x3250 x3500)
     (Choices_C_Completeness x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 z x3250 x3500) x1002
     (Guard_C_Completeness x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Completeness x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: C_Completeness -> Cover -> ConstStore -> C_Completeness
d_OP__case_3 x2 x3250 x3500 = case x2 of
     C_Complete -> C_Complete
     C_InComplete -> C_InCompleteOr
     C_InCompleteOr -> C_InCompleteOr
     (Choice_C_Completeness x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1002 x3250 x3500) (d_OP__case_3 x1003 x3250 x3500)
     (Choices_C_Completeness x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 z x3250 x3500) x1002
     (Guard_C_Completeness x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Completeness x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> C_Completeness
d_OP__case_6 x1 x5 x4 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> C_InComplete
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_5 x1 x7 x5 x4 x6 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x5 x4 x1002 x3250 x3500) (d_OP__case_6 x1 x5 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x5 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_6 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> IDSupply -> Cover -> ConstStore -> C_Completeness
nd_OP__case_6 x1 x5 x4 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> C_InComplete
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x1 x7 x5 x4 x6 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1 x5 x4 x1002 x3000 x3250 x3500) (nd_OP__case_6 x1 x5 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x1 x5 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1 x5 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> C_Completeness
d_OP__case_5 x1 x7 x5 x4 x6 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_Branch x8 x9) -> d_OP__case_4 x9 x1 x7 x5 x4 x8 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x7 x5 x4 x1002 x3250 x3500) (d_OP__case_5 x1 x7 x5 x4 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x7 x5 x4 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x7 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_5 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_BranchExpr -> IDSupply -> Cover -> ConstStore -> C_Completeness
nd_OP__case_5 x1 x7 x5 x4 x6 x3000 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_Branch x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x9 x1 x7 x5 x4 x8 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1 x7 x5 x4 x1002 x3000 x3250 x3500) (nd_OP__case_5 x1 x7 x5 x4 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x1 x7 x5 x4 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1 x7 x5 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_FlatCurry.C_Expr -> Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Pattern -> Cover -> ConstStore -> C_Completeness
d_OP__case_4 x9 x1 x7 x5 x4 x8 x3250 x3500 = case x8 of
     (Curry_FlatCurry.C_LPattern x10) -> C_InComplete
     (Curry_FlatCurry.C_Pattern x11 x12) -> d_C_combineAndResults (d_OP_isComplete_dot_checkAllCons_dot_99 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_List.d_C_delete x3250 x3500) x11 x3250 x3500) (Curry_Prelude.OP_Cons x4 x5) x3250 x3500) x7 x3250 x3500) (d_C_isComplete x1 x9 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x9 x1 x7 x5 x4 x1002 x3250 x3500) (d_OP__case_4 x9 x1 x7 x5 x4 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x9 x1 x7 x5 x4 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x9 x1 x7 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_4 :: Curry_FlatCurry.C_Expr -> Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Pattern -> IDSupply -> Cover -> ConstStore -> C_Completeness
nd_OP__case_4 x9 x1 x7 x5 x4 x8 x3000 x3250 x3500 = case x8 of
     (Curry_FlatCurry.C_LPattern x10) -> C_InComplete
     (Curry_FlatCurry.C_Pattern x11 x12) -> let
          x2008 = x3000
           in (seq x2008 (let
               x2006 = leftSupply x2008
               x2007 = rightSupply x2008
                in (seq x2006 (seq x2007 (d_C_combineAndResults (let
                    x2005 = leftSupply x2006
                    x2004 = rightSupply x2006
                     in (seq x2005 (seq x2004 (nd_OP_isComplete_dot_checkAllCons_dot_99 x1 (let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_List.nd_C_delete x2000 x3250 x3500) x11 x2001 x3250 x3500)))) (Curry_Prelude.OP_Cons x4 x5) x2003 x3250 x3500)))) x7 x2005 x3250 x3500)))) (nd_C_isComplete x1 x9 x2007 x3250 x3500) x3250 x3500)))))
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x9 x1 x7 x5 x4 x1002 x3000 x3250 x3500) (nd_OP__case_4 x9 x1 x7 x5 x4 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x9 x1 x7 x5 x4 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x9 x1 x7 x5 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> C_Completeness
d_OP__case_9 x1 x16 x3250 x3500 = case x16 of
     Curry_Prelude.OP_List -> C_InComplete
     (Curry_Prelude.OP_Cons x17 x18) -> d_OP__case_8 x1 x18 x17 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x1002 x3250 x3500) (d_OP__case_9 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_9 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> IDSupply -> Cover -> ConstStore -> C_Completeness
nd_OP__case_9 x1 x16 x3000 x3250 x3500 = case x16 of
     Curry_Prelude.OP_List -> C_InComplete
     (Curry_Prelude.OP_Cons x17 x18) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x1 x18 x17 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x1002 x3000 x3250 x3500) (nd_OP__case_9 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> C_Completeness
d_OP__case_8 x1 x18 x17 x3250 x3500 = case x17 of
     (Curry_FlatCurry.C_Branch x19 x20) -> d_OP__case_7 x20 x1 x18 x19 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x18 x1002 x3250 x3500) (d_OP__case_8 x1 x18 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 x18 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x18 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_8 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_FlatCurry.C_BranchExpr -> IDSupply -> Cover -> ConstStore -> C_Completeness
nd_OP__case_8 x1 x18 x17 x3000 x3250 x3500 = case x17 of
     (Curry_FlatCurry.C_Branch x19 x20) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x20 x1 x18 x19 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1 x18 x1002 x3000 x3250 x3500) (nd_OP__case_8 x1 x18 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x1 x18 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1 x18 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_FlatCurry.C_Expr -> Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_FlatCurry.C_Pattern -> Cover -> ConstStore -> C_Completeness
d_OP__case_7 x20 x1 x18 x19 x3250 x3500 = case x19 of
     (Curry_FlatCurry.C_LPattern x21) -> C_InComplete
     (Curry_FlatCurry.C_Pattern x22 x23) -> d_C_combineAndResults (d_OP_isComplete_dot_checkAllCons_dot_99 x1 (Curry_Prelude.d_C_maybe Curry_Prelude.OP_List Curry_Prelude.d_C_id (Curry_GenericProgInfo.d_C_lookupProgInfo x22 x1 x3250 x3500) x3250 x3500) x18 x3250 x3500) (d_C_isComplete x1 x20 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x20 x1 x18 x1002 x3250 x3500) (d_OP__case_7 x20 x1 x18 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x20 x1 x18 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x20 x1 x18 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_7 :: Curry_FlatCurry.C_Expr -> Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_FlatCurry.C_Pattern -> IDSupply -> Cover -> ConstStore -> C_Completeness
nd_OP__case_7 x20 x1 x18 x19 x3000 x3250 x3500 = case x19 of
     (Curry_FlatCurry.C_LPattern x21) -> C_InComplete
     (Curry_FlatCurry.C_Pattern x22 x23) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2004 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2004 (seq x2005 (d_C_combineAndResults (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (nd_OP_isComplete_dot_checkAllCons_dot_99 x1 (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_maybe Curry_Prelude.OP_List (wrapDX id Curry_Prelude.d_C_id) (Curry_GenericProgInfo.nd_C_lookupProgInfo x22 x1 x2000 x3250 x3500) x2001 x3250 x3500)))) x18 x2003 x3250 x3500)))) (nd_C_isComplete x1 x20 x2005 x3250 x3500) x3250 x3500)))))
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x20 x1 x18 x1002 x3000 x3250 x3500) (nd_OP__case_7 x20 x1 x18 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x20 x1 x18 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x20 x1 x18 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Completeness
d_OP__case_10 x7 x6 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_isComplete x1 (Curry_Prelude.d_C_head x7 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> C_Complete
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x7 x6 x1 x1002 x3250 x3500) (d_OP__case_10 x7 x6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x7 x6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x7 x6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_10 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_Completeness
nd_OP__case_10 x7 x6 x1 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_isComplete x1 (Curry_Prelude.d_C_head x7 x3250 x3500) x2000 x3250 x3500))
     Curry_Prelude.C_False -> C_Complete
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x7 x6 x1 x1002 x3000 x3250 x3500) (nd_OP__case_10 x7 x6 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x7 x6 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x7 x6 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> C_Completeness
d_OP__case_11 x1 x7 x3250 x3500 = case x7 of
     (Curry_FlatCurry.C_Rule x8 x9) -> d_C_isComplete x1 x9 x3250 x3500
     (Curry_FlatCurry.C_External x10) -> C_Complete
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1 x1002 x3250 x3500) (d_OP__case_11 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_11 :: Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_FlatCurry.C_Rule -> IDSupply -> Cover -> ConstStore -> C_Completeness
nd_OP__case_11 x1 x7 x3000 x3250 x3500 = case x7 of
     (Curry_FlatCurry.C_Rule x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_isComplete x1 x9 x2000 x3250 x3500))
     (Curry_FlatCurry.C_External x10) -> C_Complete
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1 x1002 x3000 x3250 x3500) (nd_OP__case_11 x1 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x1 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_12 x1 x3250 x3500 = case x1 of
     Curry_Analysis.C_AText -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))
     Curry_Analysis.C_ANote -> Curry_Prelude.OP_List
     (Curry_Analysis.Choice_C_AOutFormat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1002 x3250 x3500) (d_OP__case_12 x1003 x3250 x3500)
     (Curry_Analysis.Choices_C_AOutFormat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 z x3250 x3500) x1002
     (Curry_Analysis.Guard_C_AOutFormat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Analysis.Fail_C_AOutFormat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_13 x1 x3250 x3500 = case x1 of
     Curry_Analysis.C_AText -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))))
     Curry_Analysis.C_ANote -> Curry_Prelude.OP_List
     (Curry_Analysis.Choice_C_AOutFormat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1002 x3250 x3500) (d_OP__case_13 x1003 x3250 x3500)
     (Curry_Analysis.Choices_C_AOutFormat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 z x3250 x3500) x1002
     (Curry_Analysis.Guard_C_AOutFormat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Analysis.Fail_C_AOutFormat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
