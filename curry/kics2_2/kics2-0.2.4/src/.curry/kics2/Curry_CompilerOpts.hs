{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_CompilerOpts (OP___hash_Rec_colon_Options (..), C_Verbosity (..), C_DumpFormat (..), C_OptimLevel (..), C_Extension (..), C_Options, d_OP___hash_selR_at_Options_dot_optHelp, d_OP___hash_updR_at_Options_dot_optHelp, d_OP___hash_selR_at_Options_dot_optVersion, d_OP___hash_updR_at_Options_dot_optVersion, d_OP___hash_selR_at_Options_dot_optVerbosity, d_OP___hash_updR_at_Options_dot_optVerbosity, d_OP___hash_selR_at_Options_dot_optForce, d_OP___hash_updR_at_Options_dot_optForce, d_OP___hash_selR_at_Options_dot_optImportPaths, d_OP___hash_updR_at_Options_dot_optImportPaths, d_OP___hash_selR_at_Options_dot_optOutputSubdir, d_OP___hash_updR_at_Options_dot_optOutputSubdir, d_OP___hash_selR_at_Options_dot_optOptimization, d_OP___hash_updR_at_Options_dot_optOptimization, d_OP___hash_selR_at_Options_dot_optDump, d_OP___hash_updR_at_Options_dot_optDump, d_OP___hash_selR_at_Options_dot_optExtensions, d_OP___hash_updR_at_Options_dot_optExtensions, d_C_defaultOptions, d_C_debugOptions, d_C_compilerOpts) where

import Basics
import qualified Curry_Char
import qualified Curry_FileGoodies
import qualified Curry_GetOpt
import qualified Curry_IO
import qualified Curry_Installation
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_System
import qualified Curry_Maybe
data OP___hash_Rec_colon_Options
     = OP___hash_Lab_colon_optHelp Curry_Prelude.C_Bool
     | OP___hash_Lab_colon_optVersion Curry_Prelude.C_Bool
     | OP___hash_Lab_colon_optVerbosity C_Verbosity
     | OP___hash_Lab_colon_optForce Curry_Prelude.C_Bool
     | OP___hash_Lab_colon_optImportPaths (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | OP___hash_Lab_colon_optOutputSubdir (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | OP___hash_Lab_colon_optOptimization C_OptimLevel
     | OP___hash_Lab_colon_optDump (Curry_Prelude.OP_List C_DumpFormat)
     | OP___hash_Lab_colon_optExtensions (Curry_Prelude.OP_List C_Extension)
     | Choice_OP___hash_Rec_colon_Options Cover ID OP___hash_Rec_colon_Options OP___hash_Rec_colon_Options
     | Choices_OP___hash_Rec_colon_Options Cover ID ([OP___hash_Rec_colon_Options])
     | Fail_OP___hash_Rec_colon_Options Cover FailInfo
     | Guard_OP___hash_Rec_colon_Options Cover Constraints OP___hash_Rec_colon_Options

instance Show OP___hash_Rec_colon_Options where
  showsPrec d (Choice_OP___hash_Rec_colon_Options cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP___hash_Rec_colon_Options cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP___hash_Rec_colon_Options cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP___hash_Rec_colon_Options cd info) = showChar '!'
  showsPrec _ (OP___hash_Lab_colon_optHelp x1) = (showString "(OP___hash_Lab_colon_optHelp") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_optVersion x1) = (showString "(OP___hash_Lab_colon_optVersion") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_optVerbosity x1) = (showString "(OP___hash_Lab_colon_optVerbosity") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_optForce x1) = (showString "(OP___hash_Lab_colon_optForce") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_optImportPaths x1) = (showString "(OP___hash_Lab_colon_optImportPaths") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_optOutputSubdir x1) = (showString "(OP___hash_Lab_colon_optOutputSubdir") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_optOptimization x1) = (showString "(OP___hash_Lab_colon_optOptimization") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_optDump x1) = (showString "(OP___hash_Lab_colon_optDump") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_optExtensions x1) = (showString "(OP___hash_Lab_colon_optExtensions") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read OP___hash_Rec_colon_Options where
  readsPrec d s = (readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_optHelp x1,r1) | (_,r0) <- readQualified "CompilerOpts" "OP___hash_Lab_colon_optHelp" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_optVersion x1,r1) | (_,r0) <- readQualified "CompilerOpts" "OP___hash_Lab_colon_optVersion" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_optVerbosity x1,r1) | (_,r0) <- readQualified "CompilerOpts" "OP___hash_Lab_colon_optVerbosity" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_optForce x1,r1) | (_,r0) <- readQualified "CompilerOpts" "OP___hash_Lab_colon_optForce" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_optImportPaths x1,r1) | (_,r0) <- readQualified "CompilerOpts" "OP___hash_Lab_colon_optImportPaths" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_optOutputSubdir x1,r1) | (_,r0) <- readQualified "CompilerOpts" "OP___hash_Lab_colon_optOutputSubdir" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_optOptimization x1,r1) | (_,r0) <- readQualified "CompilerOpts" "OP___hash_Lab_colon_optOptimization" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_optDump x1,r1) | (_,r0) <- readQualified "CompilerOpts" "OP___hash_Lab_colon_optDump" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_optExtensions x1,r1) | (_,r0) <- readQualified "CompilerOpts" "OP___hash_Lab_colon_optExtensions" r, (x1,r1) <- readsPrec 11 r0]) s))))))))


instance NonDet OP___hash_Rec_colon_Options where
  choiceCons = Choice_OP___hash_Rec_colon_Options
  choicesCons = Choices_OP___hash_Rec_colon_Options
  failCons = Fail_OP___hash_Rec_colon_Options
  guardCons = Guard_OP___hash_Rec_colon_Options
  try (Choice_OP___hash_Rec_colon_Options cd i x y) = tryChoice cd i x y
  try (Choices_OP___hash_Rec_colon_Options cd i xs) = tryChoices cd i xs
  try (Fail_OP___hash_Rec_colon_Options cd info) = Fail cd info
  try (Guard_OP___hash_Rec_colon_Options cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP___hash_Rec_colon_Options cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP___hash_Rec_colon_Options cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP___hash_Rec_colon_Options cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP___hash_Rec_colon_Options cd i _) = error ("CompilerOpts.OP___hash_Rec_colon_Options.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP___hash_Rec_colon_Options cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP___hash_Rec_colon_Options cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable OP___hash_Rec_colon_Options where
  generate s = Choices_OP___hash_Rec_colon_Options defCover (freeID [1,1,1,1,1,1,1,1,1] s) [(OP___hash_Lab_colon_optHelp (generate (leftSupply s))),(OP___hash_Lab_colon_optVersion (generate (leftSupply s))),(OP___hash_Lab_colon_optVerbosity (generate (leftSupply s))),(OP___hash_Lab_colon_optForce (generate (leftSupply s))),(OP___hash_Lab_colon_optImportPaths (generate (leftSupply s))),(OP___hash_Lab_colon_optOutputSubdir (generate (leftSupply s))),(OP___hash_Lab_colon_optOptimization (generate (leftSupply s))),(OP___hash_Lab_colon_optDump (generate (leftSupply s))),(OP___hash_Lab_colon_optExtensions (generate (leftSupply s)))]


instance NormalForm OP___hash_Rec_colon_Options where
  ($!!) cont (OP___hash_Lab_colon_optHelp x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optHelp y1) cs) $!! x1) cs
  ($!!) cont (OP___hash_Lab_colon_optVersion x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optVersion y1) cs) $!! x1) cs
  ($!!) cont (OP___hash_Lab_colon_optVerbosity x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optVerbosity y1) cs) $!! x1) cs
  ($!!) cont (OP___hash_Lab_colon_optForce x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optForce y1) cs) $!! x1) cs
  ($!!) cont (OP___hash_Lab_colon_optImportPaths x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optImportPaths y1) cs) $!! x1) cs
  ($!!) cont (OP___hash_Lab_colon_optOutputSubdir x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optOutputSubdir y1) cs) $!! x1) cs
  ($!!) cont (OP___hash_Lab_colon_optOptimization x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optOptimization y1) cs) $!! x1) cs
  ($!!) cont (OP___hash_Lab_colon_optDump x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optDump y1) cs) $!! x1) cs
  ($!!) cont (OP___hash_Lab_colon_optExtensions x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optExtensions y1) cs) $!! x1) cs
  ($!!) cont (Choice_OP___hash_Rec_colon_Options cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP___hash_Rec_colon_Options cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP___hash_Rec_colon_Options cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP___hash_Rec_colon_Options cd info) _ = failCons cd info
  ($##) cont (OP___hash_Lab_colon_optHelp x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optHelp y1) cs) $## x1) cs
  ($##) cont (OP___hash_Lab_colon_optVersion x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optVersion y1) cs) $## x1) cs
  ($##) cont (OP___hash_Lab_colon_optVerbosity x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optVerbosity y1) cs) $## x1) cs
  ($##) cont (OP___hash_Lab_colon_optForce x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optForce y1) cs) $## x1) cs
  ($##) cont (OP___hash_Lab_colon_optImportPaths x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optImportPaths y1) cs) $## x1) cs
  ($##) cont (OP___hash_Lab_colon_optOutputSubdir x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optOutputSubdir y1) cs) $## x1) cs
  ($##) cont (OP___hash_Lab_colon_optOptimization x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optOptimization y1) cs) $## x1) cs
  ($##) cont (OP___hash_Lab_colon_optDump x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optDump y1) cs) $## x1) cs
  ($##) cont (OP___hash_Lab_colon_optExtensions x1) cs = ((\y1 cs -> cont (OP___hash_Lab_colon_optExtensions y1) cs) $## x1) cs
  ($##) cont (Choice_OP___hash_Rec_colon_Options cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP___hash_Rec_colon_Options cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP___hash_Rec_colon_Options cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP___hash_Rec_colon_Options cd info) _ = failCons cd info
  searchNF search cont (OP___hash_Lab_colon_optHelp x1) = search (\y1 -> cont (OP___hash_Lab_colon_optHelp y1)) x1
  searchNF search cont (OP___hash_Lab_colon_optVersion x1) = search (\y1 -> cont (OP___hash_Lab_colon_optVersion y1)) x1
  searchNF search cont (OP___hash_Lab_colon_optVerbosity x1) = search (\y1 -> cont (OP___hash_Lab_colon_optVerbosity y1)) x1
  searchNF search cont (OP___hash_Lab_colon_optForce x1) = search (\y1 -> cont (OP___hash_Lab_colon_optForce y1)) x1
  searchNF search cont (OP___hash_Lab_colon_optImportPaths x1) = search (\y1 -> cont (OP___hash_Lab_colon_optImportPaths y1)) x1
  searchNF search cont (OP___hash_Lab_colon_optOutputSubdir x1) = search (\y1 -> cont (OP___hash_Lab_colon_optOutputSubdir y1)) x1
  searchNF search cont (OP___hash_Lab_colon_optOptimization x1) = search (\y1 -> cont (OP___hash_Lab_colon_optOptimization y1)) x1
  searchNF search cont (OP___hash_Lab_colon_optDump x1) = search (\y1 -> cont (OP___hash_Lab_colon_optDump y1)) x1
  searchNF search cont (OP___hash_Lab_colon_optExtensions x1) = search (\y1 -> cont (OP___hash_Lab_colon_optExtensions y1)) x1
  searchNF _ _ x = error ("CompilerOpts.OP___hash_Rec_colon_Options.searchNF: no constructor: " ++ (show x))


instance Unifiable OP___hash_Rec_colon_Options where
  (=.=) (OP___hash_Lab_colon_optHelp x1) (OP___hash_Lab_colon_optHelp y1) cs = (x1 =:= y1) cs
  (=.=) (OP___hash_Lab_colon_optVersion x1) (OP___hash_Lab_colon_optVersion y1) cs = (x1 =:= y1) cs
  (=.=) (OP___hash_Lab_colon_optVerbosity x1) (OP___hash_Lab_colon_optVerbosity y1) cs = (x1 =:= y1) cs
  (=.=) (OP___hash_Lab_colon_optForce x1) (OP___hash_Lab_colon_optForce y1) cs = (x1 =:= y1) cs
  (=.=) (OP___hash_Lab_colon_optImportPaths x1) (OP___hash_Lab_colon_optImportPaths y1) cs = (x1 =:= y1) cs
  (=.=) (OP___hash_Lab_colon_optOutputSubdir x1) (OP___hash_Lab_colon_optOutputSubdir y1) cs = (x1 =:= y1) cs
  (=.=) (OP___hash_Lab_colon_optOptimization x1) (OP___hash_Lab_colon_optOptimization y1) cs = (x1 =:= y1) cs
  (=.=) (OP___hash_Lab_colon_optDump x1) (OP___hash_Lab_colon_optDump y1) cs = (x1 =:= y1) cs
  (=.=) (OP___hash_Lab_colon_optExtensions x1) (OP___hash_Lab_colon_optExtensions y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP___hash_Lab_colon_optHelp x1) (OP___hash_Lab_colon_optHelp y1) cs = (x1 =:<= y1) cs
  (=.<=) (OP___hash_Lab_colon_optVersion x1) (OP___hash_Lab_colon_optVersion y1) cs = (x1 =:<= y1) cs
  (=.<=) (OP___hash_Lab_colon_optVerbosity x1) (OP___hash_Lab_colon_optVerbosity y1) cs = (x1 =:<= y1) cs
  (=.<=) (OP___hash_Lab_colon_optForce x1) (OP___hash_Lab_colon_optForce y1) cs = (x1 =:<= y1) cs
  (=.<=) (OP___hash_Lab_colon_optImportPaths x1) (OP___hash_Lab_colon_optImportPaths y1) cs = (x1 =:<= y1) cs
  (=.<=) (OP___hash_Lab_colon_optOutputSubdir x1) (OP___hash_Lab_colon_optOutputSubdir y1) cs = (x1 =:<= y1) cs
  (=.<=) (OP___hash_Lab_colon_optOptimization x1) (OP___hash_Lab_colon_optOptimization y1) cs = (x1 =:<= y1) cs
  (=.<=) (OP___hash_Lab_colon_optDump x1) (OP___hash_Lab_colon_optDump y1) cs = (x1 =:<= y1) cs
  (=.<=) (OP___hash_Lab_colon_optExtensions x1) (OP___hash_Lab_colon_optExtensions y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP___hash_Lab_colon_optHelp x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (OP___hash_Lab_colon_optVersion x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (OP___hash_Lab_colon_optVerbosity x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i (OP___hash_Lab_colon_optForce x2) = ((i :=: (ChooseN 3 1)):(concat [(bind (leftID i) x2)]))
  bind i (OP___hash_Lab_colon_optImportPaths x2) = ((i :=: (ChooseN 4 1)):(concat [(bind (leftID i) x2)]))
  bind i (OP___hash_Lab_colon_optOutputSubdir x2) = ((i :=: (ChooseN 5 1)):(concat [(bind (leftID i) x2)]))
  bind i (OP___hash_Lab_colon_optOptimization x2) = ((i :=: (ChooseN 6 1)):(concat [(bind (leftID i) x2)]))
  bind i (OP___hash_Lab_colon_optDump x2) = ((i :=: (ChooseN 7 1)):(concat [(bind (leftID i) x2)]))
  bind i (OP___hash_Lab_colon_optExtensions x2) = ((i :=: (ChooseN 8 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_OP___hash_Rec_colon_Options cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP___hash_Rec_colon_Options cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP___hash_Rec_colon_Options cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP___hash_Rec_colon_Options cd i _) = error ("CompilerOpts.OP___hash_Rec_colon_Options.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP___hash_Rec_colon_Options cd info) = [(Unsolvable info)]
  bind i (Guard_OP___hash_Rec_colon_Options cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP___hash_Lab_colon_optHelp x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (OP___hash_Lab_colon_optVersion x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (OP___hash_Lab_colon_optVerbosity x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (OP___hash_Lab_colon_optForce x2) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (OP___hash_Lab_colon_optImportPaths x2) = [(i :=: (ChooseN 4 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (OP___hash_Lab_colon_optOutputSubdir x2) = [(i :=: (ChooseN 5 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (OP___hash_Lab_colon_optOptimization x2) = [(i :=: (ChooseN 6 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (OP___hash_Lab_colon_optDump x2) = [(i :=: (ChooseN 7 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (OP___hash_Lab_colon_optExtensions x2) = [(i :=: (ChooseN 8 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_OP___hash_Rec_colon_Options cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP___hash_Rec_colon_Options cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP___hash_Rec_colon_Options cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP___hash_Rec_colon_Options cd i _) = error ("CompilerOpts.OP___hash_Rec_colon_Options.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP___hash_Rec_colon_Options cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP___hash_Rec_colon_Options cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry OP___hash_Rec_colon_Options where
  (=?=) (Choice_OP___hash_Rec_colon_Options cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_OP___hash_Rec_colon_Options cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_OP___hash_Rec_colon_Options cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_OP___hash_Rec_colon_Options cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP___hash_Rec_colon_Options cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_OP___hash_Rec_colon_Options cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_OP___hash_Rec_colon_Options cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_OP___hash_Rec_colon_Options cd info) _ = failCons cd info
  (=?=) (OP___hash_Lab_colon_optHelp x1) (OP___hash_Lab_colon_optHelp y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (OP___hash_Lab_colon_optVersion x1) (OP___hash_Lab_colon_optVersion y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (OP___hash_Lab_colon_optVerbosity x1) (OP___hash_Lab_colon_optVerbosity y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (OP___hash_Lab_colon_optForce x1) (OP___hash_Lab_colon_optForce y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (OP___hash_Lab_colon_optImportPaths x1) (OP___hash_Lab_colon_optImportPaths y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (OP___hash_Lab_colon_optOutputSubdir x1) (OP___hash_Lab_colon_optOutputSubdir y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (OP___hash_Lab_colon_optOptimization x1) (OP___hash_Lab_colon_optOptimization y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (OP___hash_Lab_colon_optDump x1) (OP___hash_Lab_colon_optDump y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (OP___hash_Lab_colon_optExtensions x1) (OP___hash_Lab_colon_optExtensions y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_OP___hash_Rec_colon_Options cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_OP___hash_Rec_colon_Options cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_OP___hash_Rec_colon_Options cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_OP___hash_Rec_colon_Options cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP___hash_Rec_colon_Options cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_OP___hash_Rec_colon_Options cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_OP___hash_Rec_colon_Options cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_OP___hash_Rec_colon_Options cd info) _ = failCons cd info
  (<?=) (OP___hash_Lab_colon_optHelp x1) (OP___hash_Lab_colon_optHelp y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (OP___hash_Lab_colon_optHelp _) (OP___hash_Lab_colon_optVersion _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optHelp _) (OP___hash_Lab_colon_optVerbosity _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optHelp _) (OP___hash_Lab_colon_optForce _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optHelp _) (OP___hash_Lab_colon_optImportPaths _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optHelp _) (OP___hash_Lab_colon_optOutputSubdir _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optHelp _) (OP___hash_Lab_colon_optOptimization _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optHelp _) (OP___hash_Lab_colon_optDump _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optHelp _) (OP___hash_Lab_colon_optExtensions _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optVersion x1) (OP___hash_Lab_colon_optVersion y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (OP___hash_Lab_colon_optVersion _) (OP___hash_Lab_colon_optVerbosity _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optVersion _) (OP___hash_Lab_colon_optForce _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optVersion _) (OP___hash_Lab_colon_optImportPaths _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optVersion _) (OP___hash_Lab_colon_optOutputSubdir _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optVersion _) (OP___hash_Lab_colon_optOptimization _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optVersion _) (OP___hash_Lab_colon_optDump _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optVersion _) (OP___hash_Lab_colon_optExtensions _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optVerbosity x1) (OP___hash_Lab_colon_optVerbosity y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (OP___hash_Lab_colon_optVerbosity _) (OP___hash_Lab_colon_optForce _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optVerbosity _) (OP___hash_Lab_colon_optImportPaths _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optVerbosity _) (OP___hash_Lab_colon_optOutputSubdir _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optVerbosity _) (OP___hash_Lab_colon_optOptimization _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optVerbosity _) (OP___hash_Lab_colon_optDump _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optVerbosity _) (OP___hash_Lab_colon_optExtensions _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optForce x1) (OP___hash_Lab_colon_optForce y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (OP___hash_Lab_colon_optForce _) (OP___hash_Lab_colon_optImportPaths _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optForce _) (OP___hash_Lab_colon_optOutputSubdir _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optForce _) (OP___hash_Lab_colon_optOptimization _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optForce _) (OP___hash_Lab_colon_optDump _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optForce _) (OP___hash_Lab_colon_optExtensions _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optImportPaths x1) (OP___hash_Lab_colon_optImportPaths y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (OP___hash_Lab_colon_optImportPaths _) (OP___hash_Lab_colon_optOutputSubdir _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optImportPaths _) (OP___hash_Lab_colon_optOptimization _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optImportPaths _) (OP___hash_Lab_colon_optDump _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optImportPaths _) (OP___hash_Lab_colon_optExtensions _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optOutputSubdir x1) (OP___hash_Lab_colon_optOutputSubdir y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (OP___hash_Lab_colon_optOutputSubdir _) (OP___hash_Lab_colon_optOptimization _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optOutputSubdir _) (OP___hash_Lab_colon_optDump _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optOutputSubdir _) (OP___hash_Lab_colon_optExtensions _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optOptimization x1) (OP___hash_Lab_colon_optOptimization y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (OP___hash_Lab_colon_optOptimization _) (OP___hash_Lab_colon_optDump _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optOptimization _) (OP___hash_Lab_colon_optExtensions _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optDump x1) (OP___hash_Lab_colon_optDump y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (OP___hash_Lab_colon_optDump _) (OP___hash_Lab_colon_optExtensions _) _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optExtensions x1) (OP___hash_Lab_colon_optExtensions y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable OP___hash_Rec_colon_Options where
  cover (OP___hash_Lab_colon_optHelp x1) = OP___hash_Lab_colon_optHelp (cover x1)
  cover (OP___hash_Lab_colon_optVersion x1) = OP___hash_Lab_colon_optVersion (cover x1)
  cover (OP___hash_Lab_colon_optVerbosity x1) = OP___hash_Lab_colon_optVerbosity (cover x1)
  cover (OP___hash_Lab_colon_optForce x1) = OP___hash_Lab_colon_optForce (cover x1)
  cover (OP___hash_Lab_colon_optImportPaths x1) = OP___hash_Lab_colon_optImportPaths (cover x1)
  cover (OP___hash_Lab_colon_optOutputSubdir x1) = OP___hash_Lab_colon_optOutputSubdir (cover x1)
  cover (OP___hash_Lab_colon_optOptimization x1) = OP___hash_Lab_colon_optOptimization (cover x1)
  cover (OP___hash_Lab_colon_optDump x1) = OP___hash_Lab_colon_optDump (cover x1)
  cover (OP___hash_Lab_colon_optExtensions x1) = OP___hash_Lab_colon_optExtensions (cover x1)
  cover (Choice_OP___hash_Rec_colon_Options cd i x y) = Choice_OP___hash_Rec_colon_Options (incCover cd) i (cover x) (cover y)
  cover (Choices_OP___hash_Rec_colon_Options cd i xs) = Choices_OP___hash_Rec_colon_Options (incCover cd) i (map cover xs)
  cover (Fail_OP___hash_Rec_colon_Options cd info) = Fail_OP___hash_Rec_colon_Options (incCover cd) info
  cover (Guard_OP___hash_Rec_colon_Options cd c e) = Guard_OP___hash_Rec_colon_Options (incCover cd) c (cover e)


data C_Options
     = C_Options Curry_Prelude.C_Bool Curry_Prelude.C_Bool C_Verbosity Curry_Prelude.C_Bool (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_OptimLevel (Curry_Prelude.OP_List C_DumpFormat) (Curry_Prelude.OP_List C_Extension)
     | Choice_C_Options Cover ID C_Options C_Options
     | Choices_C_Options Cover ID ([C_Options])
     | Fail_C_Options Cover FailInfo
     | Guard_C_Options Cover Constraints C_Options

instance Show C_Options where
  showsPrec d (Choice_C_Options cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Options cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Options cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Options cd info) = showChar '!'
  showsPrec _ (C_Options x1 x2 x3 x4 x5 x6 x7 x8 x9) = (showString "(Options") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . ((showChar ' ') . ((shows x6) . ((showChar ' ') . ((shows x7) . ((showChar ' ') . ((shows x8) . ((showChar ' ') . ((shows x9) . (showChar ')')))))))))))))))))))


instance Read C_Options where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Options x1 x2 x3 x4 x5 x6 x7 x8 x9,r9) | (_,r0) <- readQualified "CompilerOpts" "Options" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4, (x6,r6) <- readsPrec 11 r5, (x7,r7) <- readsPrec 11 r6, (x8,r8) <- readsPrec 11 r7, (x9,r9) <- readsPrec 11 r8]) s


instance NonDet C_Options where
  choiceCons = Choice_C_Options
  choicesCons = Choices_C_Options
  failCons = Fail_C_Options
  guardCons = Guard_C_Options
  try (Choice_C_Options cd i x y) = tryChoice cd i x y
  try (Choices_C_Options cd i xs) = tryChoices cd i xs
  try (Fail_C_Options cd info) = Fail cd info
  try (Guard_C_Options cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Options cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Options cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Options cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Options cd i _) = error ("CompilerOpts.Options.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Options cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Options cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Options where
  generate s = Choices_C_Options defCover (freeID [9] s) [(C_Options (generate (leftSupply (leftSupply (leftSupply (leftSupply s))))) (generate (rightSupply (leftSupply (leftSupply (leftSupply s))))) (generate (rightSupply (leftSupply (leftSupply s)))) (generate (leftSupply (rightSupply (leftSupply s)))) (generate (rightSupply (rightSupply (leftSupply s)))) (generate (leftSupply (leftSupply (rightSupply s)))) (generate (rightSupply (leftSupply (rightSupply s)))) (generate (leftSupply (rightSupply (rightSupply s)))) (generate (rightSupply (rightSupply (rightSupply s)))))]


instance NormalForm C_Options where
  ($!!) cont (C_Options x1 x2 x3 x4 x5 x6 x7 x8 x9) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> cont (C_Options y1 y2 y3 y4 y5 y6 y7 y8 y9) cs) $!! x9) cs) $!! x8) cs) $!! x7) cs) $!! x6) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_Options cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Options cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Options cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Options cd info) _ = failCons cd info
  ($##) cont (C_Options x1 x2 x3 x4 x5 x6 x7 x8 x9) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> cont (C_Options y1 y2 y3 y4 y5 y6 y7 y8 y9) cs) $## x9) cs) $## x8) cs) $## x7) cs) $## x6) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_Options cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Options cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Options cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Options cd info) _ = failCons cd info
  searchNF search cont (C_Options x1 x2 x3 x4 x5 x6 x7 x8 x9) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> cont (C_Options y1 y2 y3 y4 y5 y6 y7 y8 y9)) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("CompilerOpts.Options.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Options where
  (=.=) (C_Options x1 x2 x3 x4 x5 x6 x7 x8 x9) (C_Options y1 y2 y3 y4 y5 y6 y7 y8 y9) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((((x5 =:= y5) cs) & ((((x6 =:= y6) cs) & ((((x7 =:= y7) cs) & ((((x8 =:= y8) cs) & ((x9 =:= y9) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Options x1 x2 x3 x4 x5 x6 x7 x8 x9) (C_Options y1 y2 y3 y4 y5 y6 y7 y8 y9) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((((x5 =:<= y5) cs) & ((((x6 =:<= y6) cs) & ((((x7 =:<= y7) cs) & ((((x8 =:<= y8) cs) & ((x9 =:<= y9) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Options x2 x3 x4 x5 x6 x7 x8 x9 x10) = ((i :=: (ChooseN 0 9)):(concat [(bind (leftID (leftID (leftID (leftID i)))) x2),(bind (rightID (leftID (leftID (leftID i)))) x3),(bind (rightID (leftID (leftID i))) x4),(bind (leftID (rightID (leftID i))) x5),(bind (rightID (rightID (leftID i))) x6),(bind (leftID (leftID (rightID i))) x7),(bind (rightID (leftID (rightID i))) x8),(bind (leftID (rightID (rightID i))) x9),(bind (rightID (rightID (rightID i))) x10)]))
  bind i (Choice_C_Options cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Options cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Options cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Options cd i _) = error ("CompilerOpts.Options.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Options cd info) = [(Unsolvable info)]
  bind i (Guard_C_Options cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Options x2 x3 x4 x5 x6 x7 x8 x9 x10) = [(i :=: (ChooseN 0 9)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (leftID i)))) x2))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (leftID i)))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind (rightID (leftID (leftID i))) x4))),((leftID (rightID (leftID i))) :=: (LazyBind (lazyBind (leftID (rightID (leftID i))) x5))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind (rightID (rightID (leftID i))) x6))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind (leftID (leftID (rightID i))) x7))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind (rightID (leftID (rightID i))) x8))),((leftID (rightID (rightID i))) :=: (LazyBind (lazyBind (leftID (rightID (rightID i))) x9))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind (rightID (rightID (rightID i))) x10)))]
  lazyBind i (Choice_C_Options cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Options cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Options cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Options cd i _) = error ("CompilerOpts.Options.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Options cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Options cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Options where
  (=?=) (Choice_C_Options cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Options cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Options cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Options cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Options cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Options cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Options cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Options cd info) _ = failCons cd info
  (=?=) (C_Options x1 x2 x3 x4 x5 x6 x7 x8 x9) (C_Options y1 y2 y3 y4 y5 y6 y7 y8 y9) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x4 Curry_Prelude.=?= y4) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x5 Curry_Prelude.=?= y5) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x6 Curry_Prelude.=?= y6) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x7 Curry_Prelude.=?= y7) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x8 Curry_Prelude.=?= y8) cs) ((x9 Curry_Prelude.=?= y9) cs) cs) cs) cs) cs) cs) cs) cs) cs
  (<?=) (Choice_C_Options cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Options cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Options cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Options cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Options cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Options cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Options cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Options cd info) _ = failCons cd info
  (<?=) (C_Options x1 x2 x3 x4 x5 x6 x7 x8 x9) (C_Options y1 y2 y3 y4 y5 y6 y7 y8 y9) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x4 Curry_Prelude.=?= y4) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x5 y5 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x5 Curry_Prelude.=?= y5) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x6 y6 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x6 Curry_Prelude.=?= y6) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x7 y7 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x7 Curry_Prelude.=?= y7) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x8 y8 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x8 Curry_Prelude.=?= y8) cs) ((x9 Curry_Prelude.<?= y9) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs


instance Coverable C_Options where
  cover (C_Options x1 x2 x3 x4 x5 x6 x7 x8 x9) = C_Options (cover x1) (cover x2) (cover x3) (cover x4) (cover x5) (cover x6) (cover x7) (cover x8) (cover x9)
  cover (Choice_C_Options cd i x y) = Choice_C_Options (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Options cd i xs) = Choices_C_Options (incCover cd) i (map cover xs)
  cover (Fail_C_Options cd info) = Fail_C_Options (incCover cd) info
  cover (Guard_C_Options cd c e) = Guard_C_Options (incCover cd) c (cover e)


data C_Verbosity
     = C_VerbQuiet
     | C_VerbStatus
     | C_VerbFrontend
     | C_VerbAnalysis
     | C_VerbDetails
     | Choice_C_Verbosity Cover ID C_Verbosity C_Verbosity
     | Choices_C_Verbosity Cover ID ([C_Verbosity])
     | Fail_C_Verbosity Cover FailInfo
     | Guard_C_Verbosity Cover Constraints C_Verbosity

instance Show C_Verbosity where
  showsPrec d (Choice_C_Verbosity cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Verbosity cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Verbosity cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Verbosity cd info) = showChar '!'
  showsPrec _ C_VerbQuiet = showString "VerbQuiet"
  showsPrec _ C_VerbStatus = showString "VerbStatus"
  showsPrec _ C_VerbFrontend = showString "VerbFrontend"
  showsPrec _ C_VerbAnalysis = showString "VerbAnalysis"
  showsPrec _ C_VerbDetails = showString "VerbDetails"


instance Read C_Verbosity where
  readsPrec _ s = (readParen False (\r -> [ (C_VerbQuiet,r0) | (_,r0) <- readQualified "CompilerOpts" "VerbQuiet" r]) s) ++ ((readParen False (\r -> [ (C_VerbStatus,r0) | (_,r0) <- readQualified "CompilerOpts" "VerbStatus" r]) s) ++ ((readParen False (\r -> [ (C_VerbFrontend,r0) | (_,r0) <- readQualified "CompilerOpts" "VerbFrontend" r]) s) ++ ((readParen False (\r -> [ (C_VerbAnalysis,r0) | (_,r0) <- readQualified "CompilerOpts" "VerbAnalysis" r]) s) ++ (readParen False (\r -> [ (C_VerbDetails,r0) | (_,r0) <- readQualified "CompilerOpts" "VerbDetails" r]) s))))


instance NonDet C_Verbosity where
  choiceCons = Choice_C_Verbosity
  choicesCons = Choices_C_Verbosity
  failCons = Fail_C_Verbosity
  guardCons = Guard_C_Verbosity
  try (Choice_C_Verbosity cd i x y) = tryChoice cd i x y
  try (Choices_C_Verbosity cd i xs) = tryChoices cd i xs
  try (Fail_C_Verbosity cd info) = Fail cd info
  try (Guard_C_Verbosity cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Verbosity cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Verbosity cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Verbosity cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Verbosity cd i _) = error ("CompilerOpts.Verbosity.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Verbosity cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Verbosity cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Verbosity where
  generate s = Choices_C_Verbosity defCover (freeID [0,0,0,0,0] s) [C_VerbQuiet,C_VerbStatus,C_VerbFrontend,C_VerbAnalysis,C_VerbDetails]


instance NormalForm C_Verbosity where
  ($!!) cont C_VerbQuiet cs = cont C_VerbQuiet cs
  ($!!) cont C_VerbStatus cs = cont C_VerbStatus cs
  ($!!) cont C_VerbFrontend cs = cont C_VerbFrontend cs
  ($!!) cont C_VerbAnalysis cs = cont C_VerbAnalysis cs
  ($!!) cont C_VerbDetails cs = cont C_VerbDetails cs
  ($!!) cont (Choice_C_Verbosity cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Verbosity cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Verbosity cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Verbosity cd info) _ = failCons cd info
  ($##) cont C_VerbQuiet cs = cont C_VerbQuiet cs
  ($##) cont C_VerbStatus cs = cont C_VerbStatus cs
  ($##) cont C_VerbFrontend cs = cont C_VerbFrontend cs
  ($##) cont C_VerbAnalysis cs = cont C_VerbAnalysis cs
  ($##) cont C_VerbDetails cs = cont C_VerbDetails cs
  ($##) cont (Choice_C_Verbosity cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Verbosity cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Verbosity cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Verbosity cd info) _ = failCons cd info
  searchNF _ cont C_VerbQuiet = cont C_VerbQuiet
  searchNF _ cont C_VerbStatus = cont C_VerbStatus
  searchNF _ cont C_VerbFrontend = cont C_VerbFrontend
  searchNF _ cont C_VerbAnalysis = cont C_VerbAnalysis
  searchNF _ cont C_VerbDetails = cont C_VerbDetails
  searchNF _ _ x = error ("CompilerOpts.Verbosity.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Verbosity where
  (=.=) C_VerbQuiet C_VerbQuiet cs = C_Success
  (=.=) C_VerbStatus C_VerbStatus cs = C_Success
  (=.=) C_VerbFrontend C_VerbFrontend cs = C_Success
  (=.=) C_VerbAnalysis C_VerbAnalysis cs = C_Success
  (=.=) C_VerbDetails C_VerbDetails cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_VerbQuiet C_VerbQuiet cs = C_Success
  (=.<=) C_VerbStatus C_VerbStatus cs = C_Success
  (=.<=) C_VerbFrontend C_VerbFrontend cs = C_Success
  (=.<=) C_VerbAnalysis C_VerbAnalysis cs = C_Success
  (=.<=) C_VerbDetails C_VerbDetails cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_VerbQuiet = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_VerbStatus = ((i :=: (ChooseN 1 0)):(concat []))
  bind i C_VerbFrontend = ((i :=: (ChooseN 2 0)):(concat []))
  bind i C_VerbAnalysis = ((i :=: (ChooseN 3 0)):(concat []))
  bind i C_VerbDetails = ((i :=: (ChooseN 4 0)):(concat []))
  bind i (Choice_C_Verbosity cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Verbosity cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Verbosity cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Verbosity cd i _) = error ("CompilerOpts.Verbosity.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Verbosity cd info) = [(Unsolvable info)]
  bind i (Guard_C_Verbosity cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_VerbQuiet = [(i :=: (ChooseN 0 0))]
  lazyBind i C_VerbStatus = [(i :=: (ChooseN 1 0))]
  lazyBind i C_VerbFrontend = [(i :=: (ChooseN 2 0))]
  lazyBind i C_VerbAnalysis = [(i :=: (ChooseN 3 0))]
  lazyBind i C_VerbDetails = [(i :=: (ChooseN 4 0))]
  lazyBind i (Choice_C_Verbosity cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Verbosity cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Verbosity cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Verbosity cd i _) = error ("CompilerOpts.Verbosity.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Verbosity cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Verbosity cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Verbosity where
  (=?=) (Choice_C_Verbosity cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Verbosity cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Verbosity cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Verbosity cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Verbosity cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Verbosity cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Verbosity cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Verbosity cd info) _ = failCons cd info
  (=?=) C_VerbQuiet C_VerbQuiet cs = Curry_Prelude.C_True
  (=?=) C_VerbStatus C_VerbStatus cs = Curry_Prelude.C_True
  (=?=) C_VerbFrontend C_VerbFrontend cs = Curry_Prelude.C_True
  (=?=) C_VerbAnalysis C_VerbAnalysis cs = Curry_Prelude.C_True
  (=?=) C_VerbDetails C_VerbDetails cs = Curry_Prelude.C_True
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Verbosity cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Verbosity cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Verbosity cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Verbosity cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Verbosity cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Verbosity cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Verbosity cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Verbosity cd info) _ = failCons cd info
  (<?=) C_VerbQuiet C_VerbQuiet cs = Curry_Prelude.C_True
  (<?=) C_VerbQuiet C_VerbStatus _ = Curry_Prelude.C_True
  (<?=) C_VerbQuiet C_VerbFrontend _ = Curry_Prelude.C_True
  (<?=) C_VerbQuiet C_VerbAnalysis _ = Curry_Prelude.C_True
  (<?=) C_VerbQuiet C_VerbDetails _ = Curry_Prelude.C_True
  (<?=) C_VerbStatus C_VerbStatus cs = Curry_Prelude.C_True
  (<?=) C_VerbStatus C_VerbFrontend _ = Curry_Prelude.C_True
  (<?=) C_VerbStatus C_VerbAnalysis _ = Curry_Prelude.C_True
  (<?=) C_VerbStatus C_VerbDetails _ = Curry_Prelude.C_True
  (<?=) C_VerbFrontend C_VerbFrontend cs = Curry_Prelude.C_True
  (<?=) C_VerbFrontend C_VerbAnalysis _ = Curry_Prelude.C_True
  (<?=) C_VerbFrontend C_VerbDetails _ = Curry_Prelude.C_True
  (<?=) C_VerbAnalysis C_VerbAnalysis cs = Curry_Prelude.C_True
  (<?=) C_VerbAnalysis C_VerbDetails _ = Curry_Prelude.C_True
  (<?=) C_VerbDetails C_VerbDetails cs = Curry_Prelude.C_True
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_Verbosity where
  cover C_VerbQuiet = C_VerbQuiet
  cover C_VerbStatus = C_VerbStatus
  cover C_VerbFrontend = C_VerbFrontend
  cover C_VerbAnalysis = C_VerbAnalysis
  cover C_VerbDetails = C_VerbDetails
  cover (Choice_C_Verbosity cd i x y) = Choice_C_Verbosity (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Verbosity cd i xs) = Choices_C_Verbosity (incCover cd) i (map cover xs)
  cover (Fail_C_Verbosity cd info) = Fail_C_Verbosity (incCover cd) info
  cover (Guard_C_Verbosity cd c e) = Guard_C_Verbosity (incCover cd) c (cover e)


data C_DumpFormat
     = C_DumpFlat
     | C_DumpLifted
     | C_DumpEliminated
     | C_DumpRenamed
     | C_DumpFunDecls
     | C_DumpTypeDecls
     | C_DumpAbstractHs
     | Choice_C_DumpFormat Cover ID C_DumpFormat C_DumpFormat
     | Choices_C_DumpFormat Cover ID ([C_DumpFormat])
     | Fail_C_DumpFormat Cover FailInfo
     | Guard_C_DumpFormat Cover Constraints C_DumpFormat

instance Show C_DumpFormat where
  showsPrec d (Choice_C_DumpFormat cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_DumpFormat cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_DumpFormat cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_DumpFormat cd info) = showChar '!'
  showsPrec _ C_DumpFlat = showString "DumpFlat"
  showsPrec _ C_DumpLifted = showString "DumpLifted"
  showsPrec _ C_DumpEliminated = showString "DumpEliminated"
  showsPrec _ C_DumpRenamed = showString "DumpRenamed"
  showsPrec _ C_DumpFunDecls = showString "DumpFunDecls"
  showsPrec _ C_DumpTypeDecls = showString "DumpTypeDecls"
  showsPrec _ C_DumpAbstractHs = showString "DumpAbstractHs"


instance Read C_DumpFormat where
  readsPrec _ s = (readParen False (\r -> [ (C_DumpFlat,r0) | (_,r0) <- readQualified "CompilerOpts" "DumpFlat" r]) s) ++ ((readParen False (\r -> [ (C_DumpLifted,r0) | (_,r0) <- readQualified "CompilerOpts" "DumpLifted" r]) s) ++ ((readParen False (\r -> [ (C_DumpEliminated,r0) | (_,r0) <- readQualified "CompilerOpts" "DumpEliminated" r]) s) ++ ((readParen False (\r -> [ (C_DumpRenamed,r0) | (_,r0) <- readQualified "CompilerOpts" "DumpRenamed" r]) s) ++ ((readParen False (\r -> [ (C_DumpFunDecls,r0) | (_,r0) <- readQualified "CompilerOpts" "DumpFunDecls" r]) s) ++ ((readParen False (\r -> [ (C_DumpTypeDecls,r0) | (_,r0) <- readQualified "CompilerOpts" "DumpTypeDecls" r]) s) ++ (readParen False (\r -> [ (C_DumpAbstractHs,r0) | (_,r0) <- readQualified "CompilerOpts" "DumpAbstractHs" r]) s))))))


instance NonDet C_DumpFormat where
  choiceCons = Choice_C_DumpFormat
  choicesCons = Choices_C_DumpFormat
  failCons = Fail_C_DumpFormat
  guardCons = Guard_C_DumpFormat
  try (Choice_C_DumpFormat cd i x y) = tryChoice cd i x y
  try (Choices_C_DumpFormat cd i xs) = tryChoices cd i xs
  try (Fail_C_DumpFormat cd info) = Fail cd info
  try (Guard_C_DumpFormat cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_DumpFormat cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_DumpFormat cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_DumpFormat cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_DumpFormat cd i _) = error ("CompilerOpts.DumpFormat.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_DumpFormat cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_DumpFormat cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_DumpFormat where
  generate s = Choices_C_DumpFormat defCover (freeID [0,0,0,0,0,0,0] s) [C_DumpFlat,C_DumpLifted,C_DumpEliminated,C_DumpRenamed,C_DumpFunDecls,C_DumpTypeDecls,C_DumpAbstractHs]


instance NormalForm C_DumpFormat where
  ($!!) cont C_DumpFlat cs = cont C_DumpFlat cs
  ($!!) cont C_DumpLifted cs = cont C_DumpLifted cs
  ($!!) cont C_DumpEliminated cs = cont C_DumpEliminated cs
  ($!!) cont C_DumpRenamed cs = cont C_DumpRenamed cs
  ($!!) cont C_DumpFunDecls cs = cont C_DumpFunDecls cs
  ($!!) cont C_DumpTypeDecls cs = cont C_DumpTypeDecls cs
  ($!!) cont C_DumpAbstractHs cs = cont C_DumpAbstractHs cs
  ($!!) cont (Choice_C_DumpFormat cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_DumpFormat cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_DumpFormat cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_DumpFormat cd info) _ = failCons cd info
  ($##) cont C_DumpFlat cs = cont C_DumpFlat cs
  ($##) cont C_DumpLifted cs = cont C_DumpLifted cs
  ($##) cont C_DumpEliminated cs = cont C_DumpEliminated cs
  ($##) cont C_DumpRenamed cs = cont C_DumpRenamed cs
  ($##) cont C_DumpFunDecls cs = cont C_DumpFunDecls cs
  ($##) cont C_DumpTypeDecls cs = cont C_DumpTypeDecls cs
  ($##) cont C_DumpAbstractHs cs = cont C_DumpAbstractHs cs
  ($##) cont (Choice_C_DumpFormat cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_DumpFormat cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_DumpFormat cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_DumpFormat cd info) _ = failCons cd info
  searchNF _ cont C_DumpFlat = cont C_DumpFlat
  searchNF _ cont C_DumpLifted = cont C_DumpLifted
  searchNF _ cont C_DumpEliminated = cont C_DumpEliminated
  searchNF _ cont C_DumpRenamed = cont C_DumpRenamed
  searchNF _ cont C_DumpFunDecls = cont C_DumpFunDecls
  searchNF _ cont C_DumpTypeDecls = cont C_DumpTypeDecls
  searchNF _ cont C_DumpAbstractHs = cont C_DumpAbstractHs
  searchNF _ _ x = error ("CompilerOpts.DumpFormat.searchNF: no constructor: " ++ (show x))


instance Unifiable C_DumpFormat where
  (=.=) C_DumpFlat C_DumpFlat cs = C_Success
  (=.=) C_DumpLifted C_DumpLifted cs = C_Success
  (=.=) C_DumpEliminated C_DumpEliminated cs = C_Success
  (=.=) C_DumpRenamed C_DumpRenamed cs = C_Success
  (=.=) C_DumpFunDecls C_DumpFunDecls cs = C_Success
  (=.=) C_DumpTypeDecls C_DumpTypeDecls cs = C_Success
  (=.=) C_DumpAbstractHs C_DumpAbstractHs cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_DumpFlat C_DumpFlat cs = C_Success
  (=.<=) C_DumpLifted C_DumpLifted cs = C_Success
  (=.<=) C_DumpEliminated C_DumpEliminated cs = C_Success
  (=.<=) C_DumpRenamed C_DumpRenamed cs = C_Success
  (=.<=) C_DumpFunDecls C_DumpFunDecls cs = C_Success
  (=.<=) C_DumpTypeDecls C_DumpTypeDecls cs = C_Success
  (=.<=) C_DumpAbstractHs C_DumpAbstractHs cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_DumpFlat = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_DumpLifted = ((i :=: (ChooseN 1 0)):(concat []))
  bind i C_DumpEliminated = ((i :=: (ChooseN 2 0)):(concat []))
  bind i C_DumpRenamed = ((i :=: (ChooseN 3 0)):(concat []))
  bind i C_DumpFunDecls = ((i :=: (ChooseN 4 0)):(concat []))
  bind i C_DumpTypeDecls = ((i :=: (ChooseN 5 0)):(concat []))
  bind i C_DumpAbstractHs = ((i :=: (ChooseN 6 0)):(concat []))
  bind i (Choice_C_DumpFormat cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_DumpFormat cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_DumpFormat cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_DumpFormat cd i _) = error ("CompilerOpts.DumpFormat.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_DumpFormat cd info) = [(Unsolvable info)]
  bind i (Guard_C_DumpFormat cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_DumpFlat = [(i :=: (ChooseN 0 0))]
  lazyBind i C_DumpLifted = [(i :=: (ChooseN 1 0))]
  lazyBind i C_DumpEliminated = [(i :=: (ChooseN 2 0))]
  lazyBind i C_DumpRenamed = [(i :=: (ChooseN 3 0))]
  lazyBind i C_DumpFunDecls = [(i :=: (ChooseN 4 0))]
  lazyBind i C_DumpTypeDecls = [(i :=: (ChooseN 5 0))]
  lazyBind i C_DumpAbstractHs = [(i :=: (ChooseN 6 0))]
  lazyBind i (Choice_C_DumpFormat cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_DumpFormat cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_DumpFormat cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_DumpFormat cd i _) = error ("CompilerOpts.DumpFormat.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_DumpFormat cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_DumpFormat cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_DumpFormat where
  (=?=) (Choice_C_DumpFormat cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_DumpFormat cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_DumpFormat cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_DumpFormat cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_DumpFormat cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_DumpFormat cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_DumpFormat cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_DumpFormat cd info) _ = failCons cd info
  (=?=) C_DumpFlat C_DumpFlat cs = Curry_Prelude.C_True
  (=?=) C_DumpLifted C_DumpLifted cs = Curry_Prelude.C_True
  (=?=) C_DumpEliminated C_DumpEliminated cs = Curry_Prelude.C_True
  (=?=) C_DumpRenamed C_DumpRenamed cs = Curry_Prelude.C_True
  (=?=) C_DumpFunDecls C_DumpFunDecls cs = Curry_Prelude.C_True
  (=?=) C_DumpTypeDecls C_DumpTypeDecls cs = Curry_Prelude.C_True
  (=?=) C_DumpAbstractHs C_DumpAbstractHs cs = Curry_Prelude.C_True
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_DumpFormat cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_DumpFormat cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_DumpFormat cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_DumpFormat cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_DumpFormat cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_DumpFormat cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_DumpFormat cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_DumpFormat cd info) _ = failCons cd info
  (<?=) C_DumpFlat C_DumpFlat cs = Curry_Prelude.C_True
  (<?=) C_DumpFlat C_DumpLifted _ = Curry_Prelude.C_True
  (<?=) C_DumpFlat C_DumpEliminated _ = Curry_Prelude.C_True
  (<?=) C_DumpFlat C_DumpRenamed _ = Curry_Prelude.C_True
  (<?=) C_DumpFlat C_DumpFunDecls _ = Curry_Prelude.C_True
  (<?=) C_DumpFlat C_DumpTypeDecls _ = Curry_Prelude.C_True
  (<?=) C_DumpFlat C_DumpAbstractHs _ = Curry_Prelude.C_True
  (<?=) C_DumpLifted C_DumpLifted cs = Curry_Prelude.C_True
  (<?=) C_DumpLifted C_DumpEliminated _ = Curry_Prelude.C_True
  (<?=) C_DumpLifted C_DumpRenamed _ = Curry_Prelude.C_True
  (<?=) C_DumpLifted C_DumpFunDecls _ = Curry_Prelude.C_True
  (<?=) C_DumpLifted C_DumpTypeDecls _ = Curry_Prelude.C_True
  (<?=) C_DumpLifted C_DumpAbstractHs _ = Curry_Prelude.C_True
  (<?=) C_DumpEliminated C_DumpEliminated cs = Curry_Prelude.C_True
  (<?=) C_DumpEliminated C_DumpRenamed _ = Curry_Prelude.C_True
  (<?=) C_DumpEliminated C_DumpFunDecls _ = Curry_Prelude.C_True
  (<?=) C_DumpEliminated C_DumpTypeDecls _ = Curry_Prelude.C_True
  (<?=) C_DumpEliminated C_DumpAbstractHs _ = Curry_Prelude.C_True
  (<?=) C_DumpRenamed C_DumpRenamed cs = Curry_Prelude.C_True
  (<?=) C_DumpRenamed C_DumpFunDecls _ = Curry_Prelude.C_True
  (<?=) C_DumpRenamed C_DumpTypeDecls _ = Curry_Prelude.C_True
  (<?=) C_DumpRenamed C_DumpAbstractHs _ = Curry_Prelude.C_True
  (<?=) C_DumpFunDecls C_DumpFunDecls cs = Curry_Prelude.C_True
  (<?=) C_DumpFunDecls C_DumpTypeDecls _ = Curry_Prelude.C_True
  (<?=) C_DumpFunDecls C_DumpAbstractHs _ = Curry_Prelude.C_True
  (<?=) C_DumpTypeDecls C_DumpTypeDecls cs = Curry_Prelude.C_True
  (<?=) C_DumpTypeDecls C_DumpAbstractHs _ = Curry_Prelude.C_True
  (<?=) C_DumpAbstractHs C_DumpAbstractHs cs = Curry_Prelude.C_True
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_DumpFormat where
  cover C_DumpFlat = C_DumpFlat
  cover C_DumpLifted = C_DumpLifted
  cover C_DumpEliminated = C_DumpEliminated
  cover C_DumpRenamed = C_DumpRenamed
  cover C_DumpFunDecls = C_DumpFunDecls
  cover C_DumpTypeDecls = C_DumpTypeDecls
  cover C_DumpAbstractHs = C_DumpAbstractHs
  cover (Choice_C_DumpFormat cd i x y) = Choice_C_DumpFormat (incCover cd) i (cover x) (cover y)
  cover (Choices_C_DumpFormat cd i xs) = Choices_C_DumpFormat (incCover cd) i (map cover xs)
  cover (Fail_C_DumpFormat cd info) = Fail_C_DumpFormat (incCover cd) info
  cover (Guard_C_DumpFormat cd c e) = Guard_C_DumpFormat (incCover cd) c (cover e)


data C_OptimLevel
     = C_OptimNone
     | C_OptimHigherOrder
     | C_OptimStrictSupply
     | Choice_C_OptimLevel Cover ID C_OptimLevel C_OptimLevel
     | Choices_C_OptimLevel Cover ID ([C_OptimLevel])
     | Fail_C_OptimLevel Cover FailInfo
     | Guard_C_OptimLevel Cover Constraints C_OptimLevel

instance Show C_OptimLevel where
  showsPrec d (Choice_C_OptimLevel cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_OptimLevel cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_OptimLevel cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_OptimLevel cd info) = showChar '!'
  showsPrec _ C_OptimNone = showString "OptimNone"
  showsPrec _ C_OptimHigherOrder = showString "OptimHigherOrder"
  showsPrec _ C_OptimStrictSupply = showString "OptimStrictSupply"


instance Read C_OptimLevel where
  readsPrec _ s = (readParen False (\r -> [ (C_OptimNone,r0) | (_,r0) <- readQualified "CompilerOpts" "OptimNone" r]) s) ++ ((readParen False (\r -> [ (C_OptimHigherOrder,r0) | (_,r0) <- readQualified "CompilerOpts" "OptimHigherOrder" r]) s) ++ (readParen False (\r -> [ (C_OptimStrictSupply,r0) | (_,r0) <- readQualified "CompilerOpts" "OptimStrictSupply" r]) s))


instance NonDet C_OptimLevel where
  choiceCons = Choice_C_OptimLevel
  choicesCons = Choices_C_OptimLevel
  failCons = Fail_C_OptimLevel
  guardCons = Guard_C_OptimLevel
  try (Choice_C_OptimLevel cd i x y) = tryChoice cd i x y
  try (Choices_C_OptimLevel cd i xs) = tryChoices cd i xs
  try (Fail_C_OptimLevel cd info) = Fail cd info
  try (Guard_C_OptimLevel cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_OptimLevel cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_OptimLevel cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_OptimLevel cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_OptimLevel cd i _) = error ("CompilerOpts.OptimLevel.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_OptimLevel cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_OptimLevel cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_OptimLevel where
  generate s = Choices_C_OptimLevel defCover (freeID [0,0,0] s) [C_OptimNone,C_OptimHigherOrder,C_OptimStrictSupply]


instance NormalForm C_OptimLevel where
  ($!!) cont C_OptimNone cs = cont C_OptimNone cs
  ($!!) cont C_OptimHigherOrder cs = cont C_OptimHigherOrder cs
  ($!!) cont C_OptimStrictSupply cs = cont C_OptimStrictSupply cs
  ($!!) cont (Choice_C_OptimLevel cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_OptimLevel cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_OptimLevel cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_OptimLevel cd info) _ = failCons cd info
  ($##) cont C_OptimNone cs = cont C_OptimNone cs
  ($##) cont C_OptimHigherOrder cs = cont C_OptimHigherOrder cs
  ($##) cont C_OptimStrictSupply cs = cont C_OptimStrictSupply cs
  ($##) cont (Choice_C_OptimLevel cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_OptimLevel cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_OptimLevel cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_OptimLevel cd info) _ = failCons cd info
  searchNF _ cont C_OptimNone = cont C_OptimNone
  searchNF _ cont C_OptimHigherOrder = cont C_OptimHigherOrder
  searchNF _ cont C_OptimStrictSupply = cont C_OptimStrictSupply
  searchNF _ _ x = error ("CompilerOpts.OptimLevel.searchNF: no constructor: " ++ (show x))


instance Unifiable C_OptimLevel where
  (=.=) C_OptimNone C_OptimNone cs = C_Success
  (=.=) C_OptimHigherOrder C_OptimHigherOrder cs = C_Success
  (=.=) C_OptimStrictSupply C_OptimStrictSupply cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_OptimNone C_OptimNone cs = C_Success
  (=.<=) C_OptimHigherOrder C_OptimHigherOrder cs = C_Success
  (=.<=) C_OptimStrictSupply C_OptimStrictSupply cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_OptimNone = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_OptimHigherOrder = ((i :=: (ChooseN 1 0)):(concat []))
  bind i C_OptimStrictSupply = ((i :=: (ChooseN 2 0)):(concat []))
  bind i (Choice_C_OptimLevel cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_OptimLevel cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_OptimLevel cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_OptimLevel cd i _) = error ("CompilerOpts.OptimLevel.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_OptimLevel cd info) = [(Unsolvable info)]
  bind i (Guard_C_OptimLevel cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_OptimNone = [(i :=: (ChooseN 0 0))]
  lazyBind i C_OptimHigherOrder = [(i :=: (ChooseN 1 0))]
  lazyBind i C_OptimStrictSupply = [(i :=: (ChooseN 2 0))]
  lazyBind i (Choice_C_OptimLevel cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_OptimLevel cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_OptimLevel cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_OptimLevel cd i _) = error ("CompilerOpts.OptimLevel.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_OptimLevel cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_OptimLevel cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_OptimLevel where
  (=?=) (Choice_C_OptimLevel cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_OptimLevel cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_OptimLevel cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_OptimLevel cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_OptimLevel cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_OptimLevel cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_OptimLevel cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_OptimLevel cd info) _ = failCons cd info
  (=?=) C_OptimNone C_OptimNone cs = Curry_Prelude.C_True
  (=?=) C_OptimHigherOrder C_OptimHigherOrder cs = Curry_Prelude.C_True
  (=?=) C_OptimStrictSupply C_OptimStrictSupply cs = Curry_Prelude.C_True
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_OptimLevel cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_OptimLevel cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_OptimLevel cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_OptimLevel cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_OptimLevel cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_OptimLevel cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_OptimLevel cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_OptimLevel cd info) _ = failCons cd info
  (<?=) C_OptimNone C_OptimNone cs = Curry_Prelude.C_True
  (<?=) C_OptimNone C_OptimHigherOrder _ = Curry_Prelude.C_True
  (<?=) C_OptimNone C_OptimStrictSupply _ = Curry_Prelude.C_True
  (<?=) C_OptimHigherOrder C_OptimHigherOrder cs = Curry_Prelude.C_True
  (<?=) C_OptimHigherOrder C_OptimStrictSupply _ = Curry_Prelude.C_True
  (<?=) C_OptimStrictSupply C_OptimStrictSupply cs = Curry_Prelude.C_True
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_OptimLevel where
  cover C_OptimNone = C_OptimNone
  cover C_OptimHigherOrder = C_OptimHigherOrder
  cover C_OptimStrictSupply = C_OptimStrictSupply
  cover (Choice_C_OptimLevel cd i x y) = Choice_C_OptimLevel (incCover cd) i (cover x) (cover y)
  cover (Choices_C_OptimLevel cd i xs) = Choices_C_OptimLevel (incCover cd) i (map cover xs)
  cover (Fail_C_OptimLevel cd info) = Fail_C_OptimLevel (incCover cd) info
  cover (Guard_C_OptimLevel cd c e) = Guard_C_OptimLevel (incCover cd) c (cover e)


data C_Extension
     = C_ExtNoImplicitPrelude
     | C_ExtUnknown (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_Extension Cover ID C_Extension C_Extension
     | Choices_C_Extension Cover ID ([C_Extension])
     | Fail_C_Extension Cover FailInfo
     | Guard_C_Extension Cover Constraints C_Extension

instance Show C_Extension where
  showsPrec d (Choice_C_Extension cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Extension cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Extension cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Extension cd info) = showChar '!'
  showsPrec _ C_ExtNoImplicitPrelude = showString "ExtNoImplicitPrelude"
  showsPrec _ (C_ExtUnknown x1) = (showString "(ExtUnknown") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Extension where
  readsPrec d s = (readParen False (\r -> [ (C_ExtNoImplicitPrelude,r0) | (_,r0) <- readQualified "CompilerOpts" "ExtNoImplicitPrelude" r]) s) ++ (readParen (d > 10) (\r -> [ (C_ExtUnknown x1,r1) | (_,r0) <- readQualified "CompilerOpts" "ExtUnknown" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet C_Extension where
  choiceCons = Choice_C_Extension
  choicesCons = Choices_C_Extension
  failCons = Fail_C_Extension
  guardCons = Guard_C_Extension
  try (Choice_C_Extension cd i x y) = tryChoice cd i x y
  try (Choices_C_Extension cd i xs) = tryChoices cd i xs
  try (Fail_C_Extension cd info) = Fail cd info
  try (Guard_C_Extension cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Extension cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Extension cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Extension cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Extension cd i _) = error ("CompilerOpts.Extension.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Extension cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Extension cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Extension where
  generate s = Choices_C_Extension defCover (freeID [0,1] s) [C_ExtNoImplicitPrelude,(C_ExtUnknown (generate (leftSupply s)))]


instance NormalForm C_Extension where
  ($!!) cont C_ExtNoImplicitPrelude cs = cont C_ExtNoImplicitPrelude cs
  ($!!) cont (C_ExtUnknown x1) cs = ((\y1 cs -> cont (C_ExtUnknown y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_Extension cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Extension cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Extension cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Extension cd info) _ = failCons cd info
  ($##) cont C_ExtNoImplicitPrelude cs = cont C_ExtNoImplicitPrelude cs
  ($##) cont (C_ExtUnknown x1) cs = ((\y1 cs -> cont (C_ExtUnknown y1) cs) $## x1) cs
  ($##) cont (Choice_C_Extension cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Extension cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Extension cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Extension cd info) _ = failCons cd info
  searchNF _ cont C_ExtNoImplicitPrelude = cont C_ExtNoImplicitPrelude
  searchNF search cont (C_ExtUnknown x1) = search (\y1 -> cont (C_ExtUnknown y1)) x1
  searchNF _ _ x = error ("CompilerOpts.Extension.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Extension where
  (=.=) C_ExtNoImplicitPrelude C_ExtNoImplicitPrelude cs = C_Success
  (=.=) (C_ExtUnknown x1) (C_ExtUnknown y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_ExtNoImplicitPrelude C_ExtNoImplicitPrelude cs = C_Success
  (=.<=) (C_ExtUnknown x1) (C_ExtUnknown y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_ExtNoImplicitPrelude = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (C_ExtUnknown x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_Extension cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Extension cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Extension cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Extension cd i _) = error ("CompilerOpts.Extension.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Extension cd info) = [(Unsolvable info)]
  bind i (Guard_C_Extension cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_ExtNoImplicitPrelude = [(i :=: (ChooseN 0 0))]
  lazyBind i (C_ExtUnknown x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_Extension cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Extension cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Extension cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Extension cd i _) = error ("CompilerOpts.Extension.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Extension cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Extension cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Extension where
  (=?=) (Choice_C_Extension cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Extension cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Extension cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Extension cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Extension cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Extension cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Extension cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Extension cd info) _ = failCons cd info
  (=?=) C_ExtNoImplicitPrelude C_ExtNoImplicitPrelude cs = Curry_Prelude.C_True
  (=?=) (C_ExtUnknown x1) (C_ExtUnknown y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Extension cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Extension cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Extension cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Extension cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Extension cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Extension cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Extension cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Extension cd info) _ = failCons cd info
  (<?=) C_ExtNoImplicitPrelude C_ExtNoImplicitPrelude cs = Curry_Prelude.C_True
  (<?=) C_ExtNoImplicitPrelude (C_ExtUnknown _) _ = Curry_Prelude.C_True
  (<?=) (C_ExtUnknown x1) (C_ExtUnknown y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_Extension where
  cover C_ExtNoImplicitPrelude = C_ExtNoImplicitPrelude
  cover (C_ExtUnknown x1) = C_ExtUnknown (cover x1)
  cover (Choice_C_Extension cd i x y) = Choice_C_Extension (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Extension cd i xs) = Choices_C_Extension (incCover cd) i (map cover xs)
  cover (Fail_C_Extension cd info) = Fail_C_Extension (incCover cd) info
  cover (Guard_C_Extension cd c e) = Guard_C_Extension (incCover cd) c (cover e)


d_OP___hash_selR_at_Options_dot_optHelp :: C_Options -> ConstStore -> Curry_Prelude.C_Bool
d_OP___hash_selR_at_Options_dot_optHelp x1 x3500 = case x1 of
     (C_Options x2 x3 x4 x5 x6 x7 x8 x9 x10) -> x2
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_Options_dot_optHelp x1002 x3500) (d_OP___hash_selR_at_Options_dot_optHelp x1003 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_Options_dot_optHelp z x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_Options_dot_optHelp x1002) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_updR_at_Options_dot_optHelp :: C_Options -> Curry_Prelude.C_Bool -> ConstStore -> C_Options
d_OP___hash_updR_at_Options_dot_optHelp x1 x2 x3500 = case x1 of
     (C_Options x3 x4 x5 x6 x7 x8 x9 x10 x11) -> C_Options x2 x4 x5 x6 x7 x8 x9 x10 x11
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_Options_dot_optHelp x1002 x2 x3500) (d_OP___hash_updR_at_Options_dot_optHelp x1003 x2 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_Options_dot_optHelp z x2 x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_Options_dot_optHelp x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_selR_at_Options_dot_optVersion :: C_Options -> ConstStore -> Curry_Prelude.C_Bool
d_OP___hash_selR_at_Options_dot_optVersion x1 x3500 = case x1 of
     (C_Options x2 x3 x4 x5 x6 x7 x8 x9 x10) -> x3
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_Options_dot_optVersion x1002 x3500) (d_OP___hash_selR_at_Options_dot_optVersion x1003 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_Options_dot_optVersion z x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_Options_dot_optVersion x1002) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_updR_at_Options_dot_optVersion :: C_Options -> Curry_Prelude.C_Bool -> ConstStore -> C_Options
d_OP___hash_updR_at_Options_dot_optVersion x1 x2 x3500 = case x1 of
     (C_Options x3 x4 x5 x6 x7 x8 x9 x10 x11) -> C_Options x3 x2 x5 x6 x7 x8 x9 x10 x11
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_Options_dot_optVersion x1002 x2 x3500) (d_OP___hash_updR_at_Options_dot_optVersion x1003 x2 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_Options_dot_optVersion z x2 x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_Options_dot_optVersion x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_selR_at_Options_dot_optVerbosity :: C_Options -> ConstStore -> C_Verbosity
d_OP___hash_selR_at_Options_dot_optVerbosity x1 x3500 = case x1 of
     (C_Options x2 x3 x4 x5 x6 x7 x8 x9 x10) -> x4
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_Options_dot_optVerbosity x1002 x3500) (d_OP___hash_selR_at_Options_dot_optVerbosity x1003 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_Options_dot_optVerbosity z x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_Options_dot_optVerbosity x1002) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_updR_at_Options_dot_optVerbosity :: C_Options -> C_Verbosity -> ConstStore -> C_Options
d_OP___hash_updR_at_Options_dot_optVerbosity x1 x2 x3500 = case x1 of
     (C_Options x3 x4 x5 x6 x7 x8 x9 x10 x11) -> C_Options x3 x4 x2 x6 x7 x8 x9 x10 x11
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_Options_dot_optVerbosity x1002 x2 x3500) (d_OP___hash_updR_at_Options_dot_optVerbosity x1003 x2 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_Options_dot_optVerbosity z x2 x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_Options_dot_optVerbosity x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_selR_at_Options_dot_optForce :: C_Options -> ConstStore -> Curry_Prelude.C_Bool
d_OP___hash_selR_at_Options_dot_optForce x1 x3500 = case x1 of
     (C_Options x2 x3 x4 x5 x6 x7 x8 x9 x10) -> x5
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_Options_dot_optForce x1002 x3500) (d_OP___hash_selR_at_Options_dot_optForce x1003 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_Options_dot_optForce z x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_Options_dot_optForce x1002) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_updR_at_Options_dot_optForce :: C_Options -> Curry_Prelude.C_Bool -> ConstStore -> C_Options
d_OP___hash_updR_at_Options_dot_optForce x1 x2 x3500 = case x1 of
     (C_Options x3 x4 x5 x6 x7 x8 x9 x10 x11) -> C_Options x3 x4 x5 x2 x7 x8 x9 x10 x11
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_Options_dot_optForce x1002 x2 x3500) (d_OP___hash_updR_at_Options_dot_optForce x1003 x2 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_Options_dot_optForce z x2 x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_Options_dot_optForce x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_selR_at_Options_dot_optImportPaths :: C_Options -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP___hash_selR_at_Options_dot_optImportPaths x1 x3500 = case x1 of
     (C_Options x2 x3 x4 x5 x6 x7 x8 x9 x10) -> x6
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_Options_dot_optImportPaths x1002 x3500) (d_OP___hash_selR_at_Options_dot_optImportPaths x1003 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_Options_dot_optImportPaths z x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_Options_dot_optImportPaths x1002) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_updR_at_Options_dot_optImportPaths :: C_Options -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> C_Options
d_OP___hash_updR_at_Options_dot_optImportPaths x1 x2 x3500 = case x1 of
     (C_Options x3 x4 x5 x6 x7 x8 x9 x10 x11) -> C_Options x3 x4 x5 x6 x2 x8 x9 x10 x11
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_Options_dot_optImportPaths x1002 x2 x3500) (d_OP___hash_updR_at_Options_dot_optImportPaths x1003 x2 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_Options_dot_optImportPaths z x2 x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_Options_dot_optImportPaths x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_selR_at_Options_dot_optOutputSubdir :: C_Options -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP___hash_selR_at_Options_dot_optOutputSubdir x1 x3500 = case x1 of
     (C_Options x2 x3 x4 x5 x6 x7 x8 x9 x10) -> x7
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_Options_dot_optOutputSubdir x1002 x3500) (d_OP___hash_selR_at_Options_dot_optOutputSubdir x1003 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_Options_dot_optOutputSubdir z x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_Options_dot_optOutputSubdir x1002) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_updR_at_Options_dot_optOutputSubdir :: C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_Options
d_OP___hash_updR_at_Options_dot_optOutputSubdir x1 x2 x3500 = case x1 of
     (C_Options x3 x4 x5 x6 x7 x8 x9 x10 x11) -> C_Options x3 x4 x5 x6 x7 x2 x9 x10 x11
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_Options_dot_optOutputSubdir x1002 x2 x3500) (d_OP___hash_updR_at_Options_dot_optOutputSubdir x1003 x2 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_Options_dot_optOutputSubdir z x2 x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_Options_dot_optOutputSubdir x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_selR_at_Options_dot_optOptimization :: C_Options -> ConstStore -> C_OptimLevel
d_OP___hash_selR_at_Options_dot_optOptimization x1 x3500 = case x1 of
     (C_Options x2 x3 x4 x5 x6 x7 x8 x9 x10) -> x8
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_Options_dot_optOptimization x1002 x3500) (d_OP___hash_selR_at_Options_dot_optOptimization x1003 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_Options_dot_optOptimization z x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_Options_dot_optOptimization x1002) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_updR_at_Options_dot_optOptimization :: C_Options -> C_OptimLevel -> ConstStore -> C_Options
d_OP___hash_updR_at_Options_dot_optOptimization x1 x2 x3500 = case x1 of
     (C_Options x3 x4 x5 x6 x7 x8 x9 x10 x11) -> C_Options x3 x4 x5 x6 x7 x8 x2 x10 x11
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_Options_dot_optOptimization x1002 x2 x3500) (d_OP___hash_updR_at_Options_dot_optOptimization x1003 x2 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_Options_dot_optOptimization z x2 x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_Options_dot_optOptimization x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_selR_at_Options_dot_optDump :: C_Options -> ConstStore -> Curry_Prelude.OP_List C_DumpFormat
d_OP___hash_selR_at_Options_dot_optDump x1 x3500 = case x1 of
     (C_Options x2 x3 x4 x5 x6 x7 x8 x9 x10) -> x9
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_Options_dot_optDump x1002 x3500) (d_OP___hash_selR_at_Options_dot_optDump x1003 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_Options_dot_optDump z x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_Options_dot_optDump x1002) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_updR_at_Options_dot_optDump :: C_Options -> Curry_Prelude.OP_List C_DumpFormat -> ConstStore -> C_Options
d_OP___hash_updR_at_Options_dot_optDump x1 x2 x3500 = case x1 of
     (C_Options x3 x4 x5 x6 x7 x8 x9 x10 x11) -> C_Options x3 x4 x5 x6 x7 x8 x9 x2 x11
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_Options_dot_optDump x1002 x2 x3500) (d_OP___hash_updR_at_Options_dot_optDump x1003 x2 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_Options_dot_optDump z x2 x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_Options_dot_optDump x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_selR_at_Options_dot_optExtensions :: C_Options -> ConstStore -> Curry_Prelude.OP_List C_Extension
d_OP___hash_selR_at_Options_dot_optExtensions x1 x3500 = case x1 of
     (C_Options x2 x3 x4 x5 x6 x7 x8 x9 x10) -> x10
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_Options_dot_optExtensions x1002 x3500) (d_OP___hash_selR_at_Options_dot_optExtensions x1003 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_Options_dot_optExtensions z x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_Options_dot_optExtensions x1002) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___hash_updR_at_Options_dot_optExtensions :: C_Options -> Curry_Prelude.OP_List C_Extension -> ConstStore -> C_Options
d_OP___hash_updR_at_Options_dot_optExtensions x1 x2 x3500 = case x1 of
     (C_Options x3 x4 x5 x6 x7 x8 x9 x10 x11) -> C_Options x3 x4 x5 x6 x7 x8 x9 x10 x2
     (Choice_C_Options x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_Options_dot_optExtensions x1002 x2 x3500) (d_OP___hash_updR_at_Options_dot_optExtensions x1003 x2 x3500)
     (Choices_C_Options x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_Options_dot_optExtensions z x2 x3500) x1002
     (Guard_C_Options x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_Options_dot_optExtensions x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Options x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_allDumps :: ConstStore -> Curry_Prelude.OP_List C_DumpFormat
d_C_allDumps x3500 = Curry_Prelude.OP_Cons C_DumpFlat (Curry_Prelude.OP_Cons C_DumpLifted (Curry_Prelude.OP_Cons C_DumpEliminated (Curry_Prelude.OP_Cons C_DumpRenamed (Curry_Prelude.OP_Cons C_DumpFunDecls (Curry_Prelude.OP_Cons C_DumpTypeDecls (Curry_Prelude.OP_Cons C_DumpAbstractHs Curry_Prelude.OP_List))))))

d_C_defaultOptions :: ConstStore -> C_Options
d_C_defaultOptions x3500 = C_Options Curry_Prelude.C_False Curry_Prelude.C_False C_VerbStatus Curry_Prelude.C_False Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List))))))))))))) C_OptimStrictSupply Curry_Prelude.OP_List Curry_Prelude.OP_List

d_C_debugOptions :: ConstStore -> C_Options
d_C_debugOptions x3500 = d_OP___hash_updR_at_Options_dot_optForce (d_OP___hash_updR_at_Options_dot_optVerbosity (d_C_defaultOptions x3500) C_VerbDetails x3500) Curry_Prelude.C_True x3500

d_C_parseVerbosity :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Verbosity -> ConstStore -> C_Verbosity
d_C_parseVerbosity x1 x2 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = x3
           in (d_OP__case_54 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '0'#) x3500) x3500)
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_parseVerbosity x1002 x2 x3500) (d_C_parseVerbosity x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_parseVerbosity z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_parseVerbosity x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_parseOptimization :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_OptimLevel -> ConstStore -> C_OptimLevel
d_C_parseOptimization x1 x2 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = x3
           in (d_OP__case_44 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '0'#) x3500) x3500)
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_parseOptimization x1002 x2 x3500) (d_C_parseOptimization x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_parseOptimization z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_parseOptimization x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_parseExtension :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_Extension
d_C_parseExtension x1 x3500 = d_OP__case_38 x1 (Curry_Prelude.d_C_map Curry_Char.d_C_toLower x1 x3500) x3500

d_C_options :: ConstStore -> Curry_Prelude.OP_List (Curry_GetOpt.C_OptDescr (C_Options -> ConstStore -> C_Options))
d_C_options x3500 = Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg d_OP_options_dot___hash_lambda4) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'V'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg d_OP_options_dot___hash_lambda5) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_ReqArg (acceptCs id d_OP_options_dot___hash_lambda6) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '3'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '4'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))) x3500)) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg d_OP_options_dot___hash_lambda7) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg d_OP_options_dot___hash_lambda8) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_ReqArg (acceptCs id d_OP_options_dot___hash_lambda9) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_ReqArg (acceptCs id d_OP_options_dot___hash_lambda10) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_ReqArg (acceptCs id d_OP_options_dot___hash_lambda11) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg d_OP_options_dot___hash_lambda12) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg d_OP_options_dot___hash_lambda13) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg d_OP_options_dot___hash_lambda14) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg d_OP_options_dot___hash_lambda15) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg d_OP_options_dot___hash_lambda16) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg d_OP_options_dot___hash_lambda17) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg d_OP_options_dot___hash_lambda18) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg d_OP_options_dot___hash_lambda19) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg d_OP_options_dot___hash_lambda20) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) Curry_Prelude.OP_List) Curry_Prelude.OP_List (Curry_GetOpt.C_ReqArg (acceptCs id d_OP_options_dot___hash_lambda21) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))) Curry_Prelude.OP_List)))))))))))))))))

nd_C_options :: IDSupply -> ConstStore -> Curry_Prelude.OP_List (Curry_GetOpt.C_OptDescr (Func C_Options C_Options))
nd_C_options x3000 x3500 = Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg (wrapDX id d_OP_options_dot___hash_lambda4)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'V'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg (wrapDX id d_OP_options_dot___hash_lambda5)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.HO_C_ReqArg (wrapDX (wrapDX id) (acceptCs id d_OP_options_dot___hash_lambda6)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '3'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '4'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))) x3500)) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg (wrapDX id d_OP_options_dot___hash_lambda7)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg (wrapDX id d_OP_options_dot___hash_lambda8)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.HO_C_ReqArg (wrapDX (wrapDX id) (acceptCs id d_OP_options_dot___hash_lambda9)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.HO_C_ReqArg (wrapDX (wrapDX id) (acceptCs id d_OP_options_dot___hash_lambda10)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.HO_C_ReqArg (wrapDX (wrapDX id) (acceptCs id d_OP_options_dot___hash_lambda11)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg (wrapDX id d_OP_options_dot___hash_lambda12)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg (wrapDX id d_OP_options_dot___hash_lambda13)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg (wrapDX id d_OP_options_dot___hash_lambda14)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg (wrapDX id d_OP_options_dot___hash_lambda15)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg (wrapDX id d_OP_options_dot___hash_lambda16)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg (wrapDX id d_OP_options_dot___hash_lambda17)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg (wrapDX id d_OP_options_dot___hash_lambda18)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg (wrapDX id d_OP_options_dot___hash_lambda19)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))))) Curry_Prelude.OP_List) (Curry_GetOpt.C_NoArg (wrapDX id d_OP_options_dot___hash_lambda20)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_GetOpt.C_Option (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) Curry_Prelude.OP_List) Curry_Prelude.OP_List (Curry_GetOpt.HO_C_ReqArg (wrapDX (wrapDX id) (acceptCs id d_OP_options_dot___hash_lambda21)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))) Curry_Prelude.OP_List)))))))))))))))))

d_OP_options_dot___hash_lambda4 :: C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda4 x1 x3500 = d_OP___hash_updR_at_Options_dot_optHelp x1 Curry_Prelude.C_True x3500

d_OP_options_dot___hash_lambda5 :: C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda5 x1 x3500 = d_OP___hash_updR_at_Options_dot_optVersion x1 Curry_Prelude.C_True x3500

d_OP_options_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda6 x1 x2 x3500 = d_OP___hash_updR_at_Options_dot_optVerbosity x2 (d_C_parseVerbosity x1 (d_OP___hash_selR_at_Options_dot_optVerbosity x2 x3500) x3500) x3500

d_OP_options_dot___hash_lambda7 :: C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda7 x1 x3500 = d_OP___hash_updR_at_Options_dot_optVerbosity x1 C_VerbQuiet x3500

d_OP_options_dot___hash_lambda8 :: C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda8 x1 x3500 = d_OP___hash_updR_at_Options_dot_optForce x1 Curry_Prelude.C_True x3500

d_OP_options_dot___hash_lambda9 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda9 x1 x2 x3500 = d_OP___hash_updR_at_Options_dot_optImportPaths x2 (Curry_List.d_C_nub (Curry_Prelude.d_OP_plus_plus (d_OP___hash_selR_at_Options_dot_optImportPaths x2 x3500) (Curry_FileGoodies.d_C_splitPath x1 x3500) x3500) x3500) x3500

d_OP_options_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda10 x1 x2 x3500 = d_OP___hash_updR_at_Options_dot_optOutputSubdir x2 x1 x3500

d_OP_options_dot___hash_lambda11 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda11 x1 x2 x3500 = d_OP___hash_updR_at_Options_dot_optOptimization x2 (d_C_parseOptimization x1 (d_OP___hash_selR_at_Options_dot_optOptimization x2 x3500) x3500) x3500

d_OP_options_dot___hash_lambda12 :: C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda12 x1 x3500 = d_OP___hash_updR_at_Options_dot_optOptimization x1 C_OptimNone x3500

d_OP_options_dot___hash_lambda13 :: C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda13 x1 x3500 = d_OP___hash_updR_at_Options_dot_optDump x1 (Curry_List.d_C_nub (Curry_Prelude.OP_Cons C_DumpFlat (d_OP___hash_selR_at_Options_dot_optDump x1 x3500)) x3500) x3500

d_OP_options_dot___hash_lambda14 :: C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda14 x1 x3500 = d_OP___hash_updR_at_Options_dot_optDump x1 (Curry_List.d_C_nub (Curry_Prelude.OP_Cons C_DumpLifted (d_OP___hash_selR_at_Options_dot_optDump x1 x3500)) x3500) x3500

d_OP_options_dot___hash_lambda15 :: C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda15 x1 x3500 = d_OP___hash_updR_at_Options_dot_optDump x1 (Curry_List.d_C_nub (Curry_Prelude.OP_Cons C_DumpEliminated (d_OP___hash_selR_at_Options_dot_optDump x1 x3500)) x3500) x3500

d_OP_options_dot___hash_lambda16 :: C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda16 x1 x3500 = d_OP___hash_updR_at_Options_dot_optDump x1 (Curry_List.d_C_nub (Curry_Prelude.OP_Cons C_DumpAbstractHs (d_OP___hash_selR_at_Options_dot_optDump x1 x3500)) x3500) x3500

d_OP_options_dot___hash_lambda17 :: C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda17 x1 x3500 = d_OP___hash_updR_at_Options_dot_optDump x1 (Curry_List.d_C_nub (Curry_Prelude.OP_Cons C_DumpFunDecls (d_OP___hash_selR_at_Options_dot_optDump x1 x3500)) x3500) x3500

d_OP_options_dot___hash_lambda18 :: C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda18 x1 x3500 = d_OP___hash_updR_at_Options_dot_optDump x1 (Curry_List.d_C_nub (Curry_Prelude.OP_Cons C_DumpTypeDecls (d_OP___hash_selR_at_Options_dot_optDump x1 x3500)) x3500) x3500

d_OP_options_dot___hash_lambda19 :: C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda19 x1 x3500 = d_OP___hash_updR_at_Options_dot_optDump x1 (Curry_List.d_C_nub (Curry_Prelude.OP_Cons C_DumpRenamed (d_OP___hash_selR_at_Options_dot_optDump x1 x3500)) x3500) x3500

d_OP_options_dot___hash_lambda20 :: C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda20 x1 x3500 = d_OP___hash_updR_at_Options_dot_optDump x1 (d_C_allDumps x3500) x3500

d_OP_options_dot___hash_lambda21 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Options -> ConstStore -> C_Options
d_OP_options_dot___hash_lambda21 x1 x2 x3500 = d_OP___hash_updR_at_Options_dot_optExtensions x2 (Curry_List.d_C_nub (Curry_Prelude.OP_Cons (d_C_parseExtension x1 x3500) (d_OP___hash_selR_at_Options_dot_optExtensions x2 x3500)) x3500) x3500

d_C_versionString :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_versionString x3500 = Curry_Prelude.d_C_concat (Curry_Prelude.OP_Cons (Curry_Installation.d_C_compilerName x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'V'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show (Curry_Installation.d_C_majorVersion x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.d_C_show (Curry_Installation.d_C_minorVersion x3500) x3500)) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_Installation.d_C_compilerDate x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500) Curry_Prelude.OP_List))) x3500

d_C_parseOpts :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple3 C_Options (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_parseOpts x1 x3500 = let
     x2 = Curry_GetOpt.d_C_getOpt Curry_GetOpt.C_Permute (d_C_options x3500) x1 x3500
     x3 = d_OP_parseOpts_dot___hash_selFP2_hash_opts x2 x3500
     x4 = d_OP_parseOpts_dot___hash_selFP3_hash_files x2 x3500
     x5 = d_OP_parseOpts_dot___hash_selFP4_hash_errs x2 x3500
      in (Curry_Prelude.OP_Tuple3 (Curry_Prelude.d_C_foldl (acceptCs id (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_dollar))) (d_C_defaultOptions x3500) x3 x3500) x4 x5)

d_OP_parseOpts_dot___hash_selFP2_hash_opts :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (C_Options -> ConstStore -> C_Options)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (C_Options -> ConstStore -> C_Options)
d_OP_parseOpts_dot___hash_selFP2_hash_opts x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseOpts_dot___hash_selFP2_hash_opts x1002 x3500) (d_OP_parseOpts_dot___hash_selFP2_hash_opts x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseOpts_dot___hash_selFP2_hash_opts z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseOpts_dot___hash_selFP2_hash_opts x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_parseOpts_dot___hash_selFP2_hash_opts :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Func C_Options C_Options)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (Func C_Options C_Options)
nd_OP_parseOpts_dot___hash_selFP2_hash_opts x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_parseOpts_dot___hash_selFP2_hash_opts x1002 x3000 x3500) (nd_OP_parseOpts_dot___hash_selFP2_hash_opts x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_parseOpts_dot___hash_selFP2_hash_opts z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_parseOpts_dot___hash_selFP2_hash_opts x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseOpts_dot___hash_selFP3_hash_files :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (C_Options -> ConstStore -> C_Options)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_parseOpts_dot___hash_selFP3_hash_files x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseOpts_dot___hash_selFP3_hash_files x1002 x3500) (d_OP_parseOpts_dot___hash_selFP3_hash_files x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseOpts_dot___hash_selFP3_hash_files z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseOpts_dot___hash_selFP3_hash_files x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_parseOpts_dot___hash_selFP3_hash_files :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Func C_Options C_Options)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_parseOpts_dot___hash_selFP3_hash_files x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_parseOpts_dot___hash_selFP3_hash_files x1002 x3000 x3500) (nd_OP_parseOpts_dot___hash_selFP3_hash_files x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_parseOpts_dot___hash_selFP3_hash_files z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_parseOpts_dot___hash_selFP3_hash_files x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseOpts_dot___hash_selFP4_hash_errs :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (C_Options -> ConstStore -> C_Options)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_parseOpts_dot___hash_selFP4_hash_errs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseOpts_dot___hash_selFP4_hash_errs x1002 x3500) (d_OP_parseOpts_dot___hash_selFP4_hash_errs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseOpts_dot___hash_selFP4_hash_errs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseOpts_dot___hash_selFP4_hash_errs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_parseOpts_dot___hash_selFP4_hash_errs :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Func C_Options C_Options)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_parseOpts_dot___hash_selFP4_hash_errs x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_parseOpts_dot___hash_selFP4_hash_errs x1002 x3000 x3500) (nd_OP_parseOpts_dot___hash_selFP4_hash_errs x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_parseOpts_dot___hash_selFP4_hash_errs z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_parseOpts_dot___hash_selFP4_hash_errs x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_checkOpts :: C_Options -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_checkOpts x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_checkOpts x1 x1002 x3500) (d_C_checkOpts x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_checkOpts x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_checkOpts x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_printVersion :: Curry_Prelude.Curry t0 => ConstStore -> Curry_Prelude.C_IO t0
d_C_printVersion x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (d_C_versionString x3500) x3500) (Curry_System.d_C_exitWith (Curry_Prelude.C_Int 0#) x3500) x3500

d_C_printUsage :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO t0
d_C_printUsage x1 x3500 = let
     x2 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))))))))))))))) x3500) x3500
      in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_GetOpt.d_C_usageInfo x2 (d_C_options x3500) x3500) x3500) (Curry_System.d_C_exitWith (Curry_Prelude.C_Int 0#) x3500) x3500)

d_C_badUsage :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO t0
d_C_badUsage x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_IO.d_C_hPutStrLn (Curry_IO.d_C_stderr x3500)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))) x3500) x3500) x3500) (Curry_System.d_C_exitWith (Curry_Prelude.C_Int 1#) x3500) x3500
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.d_OP_gt_gt (Curry_IO.d_C_hPutStrLn (Curry_IO.d_C_stderr x3500) x3 x3500) (d_C_badUsage x1 x4 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_badUsage x1 x1002 x3500) (d_C_badUsage x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_badUsage x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_badUsage x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_compilerOpts :: ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_Options (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_compilerOpts x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_System.d_C_getArgs x3500) d_OP_compilerOpts_dot___hash_lambda22 x3500

d_OP_compilerOpts_dot___hash_lambda22 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_Options (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_compilerOpts_dot___hash_lambda22 x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_System.d_C_getProgName x3500) (d_OP_compilerOpts_dot___hash_lambda22_dot___hash_lambda23 x1) x3500

d_OP_compilerOpts_dot___hash_lambda22_dot___hash_lambda23 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_Options (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_compilerOpts_dot___hash_lambda22_dot___hash_lambda23 x1 x2 x3500 = Curry_Prelude.d_OP_dollar (d_C_processOpts x2) (d_C_parseOpts x1 x3500) x3500

d_C_processOpts :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple3 C_Options (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_Options (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_processOpts x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple3 x3 x4 x5) -> let
          x6 = Curry_Prelude.d_OP_plus_plus x5 (d_C_checkOpts x3 x4 x3500) x3500
           in (d_OP__case_3 x1 x3 x4 x6 (d_OP___hash_selR_at_Options_dot_optHelp x3 x3500) x3500)
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_processOpts x1 x1002 x3500) (d_C_processOpts x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_processOpts x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_processOpts x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x3 x4 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_printUsage x1 x3500
     Curry_Prelude.C_False -> d_OP__case_2 x1 x3 x4 x6 (d_OP___hash_selR_at_Options_dot_optVersion x3 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x3 x4 x6 x1002 x3500) (d_OP__case_3 x1 x3 x4 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x3 x4 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x3 x4 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x3 x4 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_printUsage x1 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x1 x3 x4 x6 (d_OP___hash_selR_at_Options_dot_optVersion x3 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x3 x4 x6 x1002 x3000 x3500) (nd_OP__case_3 x1 x3 x4 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x3 x4 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x3 x4 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x3 x4 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_printVersion x3500
     Curry_Prelude.C_False -> d_OP__case_1 x1 x3 x4 x6 (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x6 x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x3 x4 x6 x1002 x3500) (d_OP__case_2 x1 x3 x4 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x3 x4 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x3 x4 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x3 x4 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_printVersion x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x1 x3 x4 x6 (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x6 x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x3 x4 x6 x1002 x3000 x3500) (nd_OP__case_2 x1 x3 x4 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x3 x4 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x3 x4 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x3 x4 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_badUsage x1 x6 x3500
     Curry_Prelude.C_False -> d_OP__case_0 x3 x4 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x3 x4 x6 x1002 x3500) (d_OP__case_1 x1 x3 x4 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x3 x4 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x3 x4 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x3 x4 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_badUsage x1 x6 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x3 x4 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x3 x4 x6 x1002 x3000 x3500) (nd_OP__case_1 x1 x3 x4 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x3 x4 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x3 x4 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x3 x4) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x3 x4 x1002 x3500) (d_OP__case_0 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x3 x4) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x3 x4 x1002 x3000 x3500) (nd_OP__case_0 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_38 x1 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x4 = x2
           in (d_OP__case_37 x1 x3 x4 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Char 'n'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x1 x1002 x3500) (d_OP__case_38 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_38 x1 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2000 = x3000
           in (seq x2000 (let
               x4 = x2
                in (nd_OP__case_37 x1 x3 x4 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Char 'n'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_38 x1 x1002 x3000 x3500) (nd_OP__case_38 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_38 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_38 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_37 x1 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> d_OP__case_36 x1 x3 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x1 x3 x4 x1002 x3500) (d_OP__case_37 x1 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x1 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_37 x1 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_36 x1 x3 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_37 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_37 x1 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_37 x1 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_37 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_36 x1 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = x5
           in (d_OP__case_35 x1 x6 x7 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Char 'o'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x1 x1002 x3500) (d_OP__case_36 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_36 x1 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (let
               x7 = x5
                in (nd_OP__case_35 x1 x6 x7 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Char 'o'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_36 x1 x1002 x3000 x3500) (nd_OP__case_36 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_36 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_36 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_35 x1 x6 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> d_OP__case_34 x1 x6 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x1 x6 x7 x1002 x3500) (d_OP__case_35 x1 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x1 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x1 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_35 x1 x6 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_34 x1 x6 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_35 x1 x6 x7 x1002 x3000 x3500) (nd_OP__case_35 x1 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_35 x1 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_35 x1 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_34 x1 x6 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x10 = x8
           in (d_OP__case_33 x1 x9 x10 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char 'i'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x1 x1002 x3500) (d_OP__case_34 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_34 x1 x6 x3000 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (let
               x10 = x8
                in (nd_OP__case_33 x1 x9 x10 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char 'i'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_34 x1 x1002 x3000 x3500) (nd_OP__case_34 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_34 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_34 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_33 x1 x9 x10 x11 x3500 = case x11 of
     Curry_Prelude.C_True -> d_OP__case_32 x1 x9 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x1 x9 x10 x1002 x3500) (d_OP__case_33 x1 x9 x10 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x1 x9 x10 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x1 x9 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_33 x1 x9 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_32 x1 x9 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_33 x1 x9 x10 x1002 x3000 x3500) (nd_OP__case_33 x1 x9 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_33 x1 x9 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_33 x1 x9 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_32 x1 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x11 x12) -> let
          x13 = x11
           in (d_OP__case_31 x1 x12 x13 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char 'm'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x1 x1002 x3500) (d_OP__case_32 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_32 x1 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x11 x12) -> let
          x2000 = x3000
           in (seq x2000 (let
               x13 = x11
                in (nd_OP__case_31 x1 x12 x13 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char 'm'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_32 x1 x1002 x3000 x3500) (nd_OP__case_32 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_32 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_32 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_31 x1 x12 x13 x14 x3500 = case x14 of
     Curry_Prelude.C_True -> d_OP__case_30 x1 x12 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x1 x12 x13 x1002 x3500) (d_OP__case_31 x1 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x1 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x1 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_31 x1 x12 x13 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_30 x1 x12 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_31 x1 x12 x13 x1002 x3000 x3500) (nd_OP__case_31 x1 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_31 x1 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_31 x1 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_30 x1 x12 x3500 = case x12 of
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x16 = x14
           in (d_OP__case_29 x1 x15 x16 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Char 'p'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x1 x1002 x3500) (d_OP__case_30 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_30 x1 x12 x3000 x3500 = case x12 of
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (let
               x16 = x14
                in (nd_OP__case_29 x1 x15 x16 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Char 'p'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_30 x1 x1002 x3000 x3500) (nd_OP__case_30 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_30 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_30 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_29 x1 x15 x16 x17 x3500 = case x17 of
     Curry_Prelude.C_True -> d_OP__case_28 x1 x15 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x1 x15 x16 x1002 x3500) (d_OP__case_29 x1 x15 x16 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x1 x15 x16 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x1 x15 x16 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_29 x1 x15 x16 x17 x3000 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_28 x1 x15 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_29 x1 x15 x16 x1002 x3000 x3500) (nd_OP__case_29 x1 x15 x16 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_29 x1 x15 x16 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_29 x1 x15 x16 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_28 x1 x15 x3500 = case x15 of
     (Curry_Prelude.OP_Cons x17 x18) -> let
          x19 = x17
           in (d_OP__case_27 x1 x18 x19 (Curry_Prelude.d_OP_eq_eq x19 (Curry_Prelude.C_Char 'l'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x1 x1002 x3500) (d_OP__case_28 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_28 x1 x15 x3000 x3500 = case x15 of
     (Curry_Prelude.OP_Cons x17 x18) -> let
          x2000 = x3000
           in (seq x2000 (let
               x19 = x17
                in (nd_OP__case_27 x1 x18 x19 (Curry_Prelude.d_OP_eq_eq x19 (Curry_Prelude.C_Char 'l'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_28 x1 x1002 x3000 x3500) (nd_OP__case_28 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_28 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_28 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_27 x1 x18 x19 x20 x3500 = case x20 of
     Curry_Prelude.C_True -> d_OP__case_26 x1 x18 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x1 x18 x19 x1002 x3500) (d_OP__case_27 x1 x18 x19 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x1 x18 x19 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x1 x18 x19 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_27 x1 x18 x19 x20 x3000 x3500 = case x20 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_26 x1 x18 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x1 x18 x19 x1002 x3000 x3500) (nd_OP__case_27 x1 x18 x19 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 x1 x18 x19 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x1 x18 x19 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_26 x1 x18 x3500 = case x18 of
     (Curry_Prelude.OP_Cons x20 x21) -> let
          x22 = x20
           in (d_OP__case_25 x1 x21 x22 (Curry_Prelude.d_OP_eq_eq x22 (Curry_Prelude.C_Char 'i'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x1 x1002 x3500) (d_OP__case_26 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_26 x1 x18 x3000 x3500 = case x18 of
     (Curry_Prelude.OP_Cons x20 x21) -> let
          x2000 = x3000
           in (seq x2000 (let
               x22 = x20
                in (nd_OP__case_25 x1 x21 x22 (Curry_Prelude.d_OP_eq_eq x22 (Curry_Prelude.C_Char 'i'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x1 x1002 x3000 x3500) (nd_OP__case_26 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_25 x1 x21 x22 x23 x3500 = case x23 of
     Curry_Prelude.C_True -> d_OP__case_24 x1 x21 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x1 x21 x22 x1002 x3500) (d_OP__case_25 x1 x21 x22 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x1 x21 x22 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x1 x21 x22 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_25 x1 x21 x22 x23 x3000 x3500 = case x23 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_24 x1 x21 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x1 x21 x22 x1002 x3000 x3500) (nd_OP__case_25 x1 x21 x22 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 x1 x21 x22 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x1 x21 x22 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_24 x1 x21 x3500 = case x21 of
     (Curry_Prelude.OP_Cons x23 x24) -> let
          x25 = x23
           in (d_OP__case_23 x1 x24 x25 (Curry_Prelude.d_OP_eq_eq x25 (Curry_Prelude.C_Char 'c'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x1 x1002 x3500) (d_OP__case_24 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_24 x1 x21 x3000 x3500 = case x21 of
     (Curry_Prelude.OP_Cons x23 x24) -> let
          x2000 = x3000
           in (seq x2000 (let
               x25 = x23
                in (nd_OP__case_23 x1 x24 x25 (Curry_Prelude.d_OP_eq_eq x25 (Curry_Prelude.C_Char 'c'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x1 x1002 x3000 x3500) (nd_OP__case_24 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_23 x1 x24 x25 x26 x3500 = case x26 of
     Curry_Prelude.C_True -> d_OP__case_22 x1 x24 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x1 x24 x25 x1002 x3500) (d_OP__case_23 x1 x24 x25 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x1 x24 x25 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x1 x24 x25 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_23 x1 x24 x25 x26 x3000 x3500 = case x26 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_22 x1 x24 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x1 x24 x25 x1002 x3000 x3500) (nd_OP__case_23 x1 x24 x25 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x1 x24 x25 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x1 x24 x25 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_22 x1 x24 x3500 = case x24 of
     (Curry_Prelude.OP_Cons x26 x27) -> let
          x28 = x26
           in (d_OP__case_21 x1 x27 x28 (Curry_Prelude.d_OP_eq_eq x28 (Curry_Prelude.C_Char 'i'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1 x1002 x3500) (d_OP__case_22 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x1 x24 x3000 x3500 = case x24 of
     (Curry_Prelude.OP_Cons x26 x27) -> let
          x2000 = x3000
           in (seq x2000 (let
               x28 = x26
                in (nd_OP__case_21 x1 x27 x28 (Curry_Prelude.d_OP_eq_eq x28 (Curry_Prelude.C_Char 'i'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x1 x1002 x3000 x3500) (nd_OP__case_22 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x1 x27 x28 x29 x3500 = case x29 of
     Curry_Prelude.C_True -> d_OP__case_20 x1 x27 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x1 x27 x28 x1002 x3500) (d_OP__case_21 x1 x27 x28 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x1 x27 x28 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x1 x27 x28 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x1 x27 x28 x29 x3000 x3500 = case x29 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_20 x1 x27 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x1 x27 x28 x1002 x3000 x3500) (nd_OP__case_21 x1 x27 x28 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x1 x27 x28 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x1 x27 x28 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_20 x1 x27 x3500 = case x27 of
     (Curry_Prelude.OP_Cons x29 x30) -> let
          x31 = x29
           in (d_OP__case_19 x1 x30 x31 (Curry_Prelude.d_OP_eq_eq x31 (Curry_Prelude.C_Char 't'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1 x1002 x3500) (d_OP__case_20 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x1 x27 x3000 x3500 = case x27 of
     (Curry_Prelude.OP_Cons x29 x30) -> let
          x2000 = x3000
           in (seq x2000 (let
               x31 = x29
                in (nd_OP__case_19 x1 x30 x31 (Curry_Prelude.d_OP_eq_eq x31 (Curry_Prelude.C_Char 't'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x1 x1002 x3000 x3500) (nd_OP__case_20 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x1 x30 x31 x32 x3500 = case x32 of
     Curry_Prelude.C_True -> d_OP__case_18 x1 x30 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x1 x30 x31 x1002 x3500) (d_OP__case_19 x1 x30 x31 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x1 x30 x31 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x1 x30 x31 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x1 x30 x31 x32 x3000 x3500 = case x32 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_18 x1 x30 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x1 x30 x31 x1002 x3000 x3500) (nd_OP__case_19 x1 x30 x31 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x1 x30 x31 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x1 x30 x31 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x1 x30 x3500 = case x30 of
     (Curry_Prelude.OP_Cons x32 x33) -> let
          x34 = x32
           in (d_OP__case_17 x1 x33 x34 (Curry_Prelude.d_OP_eq_eq x34 (Curry_Prelude.C_Char 'p'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x1 x1002 x3500) (d_OP__case_18 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x1 x30 x3000 x3500 = case x30 of
     (Curry_Prelude.OP_Cons x32 x33) -> let
          x2000 = x3000
           in (seq x2000 (let
               x34 = x32
                in (nd_OP__case_17 x1 x33 x34 (Curry_Prelude.d_OP_eq_eq x34 (Curry_Prelude.C_Char 'p'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x1 x1002 x3000 x3500) (nd_OP__case_18 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x1 x33 x34 x35 x3500 = case x35 of
     Curry_Prelude.C_True -> d_OP__case_16 x1 x33 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1 x33 x34 x1002 x3500) (d_OP__case_17 x1 x33 x34 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x1 x33 x34 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1 x33 x34 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x1 x33 x34 x35 x3000 x3500 = case x35 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x1 x33 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x1 x33 x34 x1002 x3000 x3500) (nd_OP__case_17 x1 x33 x34 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x1 x33 x34 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x1 x33 x34 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x1 x33 x3500 = case x33 of
     (Curry_Prelude.OP_Cons x35 x36) -> let
          x37 = x35
           in (d_OP__case_15 x1 x36 x37 (Curry_Prelude.d_OP_eq_eq x37 (Curry_Prelude.C_Char 'r'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x1 x1002 x3500) (d_OP__case_16 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x1 x33 x3000 x3500 = case x33 of
     (Curry_Prelude.OP_Cons x35 x36) -> let
          x2000 = x3000
           in (seq x2000 (let
               x37 = x35
                in (nd_OP__case_15 x1 x36 x37 (Curry_Prelude.d_OP_eq_eq x37 (Curry_Prelude.C_Char 'r'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x1 x1002 x3000 x3500) (nd_OP__case_16 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x1 x36 x37 x38 x3500 = case x38 of
     Curry_Prelude.C_True -> d_OP__case_14 x1 x36 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x1 x36 x37 x1002 x3500) (d_OP__case_15 x1 x36 x37 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x1 x36 x37 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x1 x36 x37 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x1 x36 x37 x38 x3000 x3500 = case x38 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x1 x36 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x1 x36 x37 x1002 x3000 x3500) (nd_OP__case_15 x1 x36 x37 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x1 x36 x37 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x1 x36 x37 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x1 x36 x3500 = case x36 of
     (Curry_Prelude.OP_Cons x38 x39) -> let
          x40 = x38
           in (d_OP__case_13 x1 x39 x40 (Curry_Prelude.d_OP_eq_eq x40 (Curry_Prelude.C_Char 'e'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1 x1002 x3500) (d_OP__case_14 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x1 x36 x3000 x3500 = case x36 of
     (Curry_Prelude.OP_Cons x38 x39) -> let
          x2000 = x3000
           in (seq x2000 (let
               x40 = x38
                in (nd_OP__case_13 x1 x39 x40 (Curry_Prelude.d_OP_eq_eq x40 (Curry_Prelude.C_Char 'e'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x1 x1002 x3000 x3500) (nd_OP__case_14 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x1 x39 x40 x41 x3500 = case x41 of
     Curry_Prelude.C_True -> d_OP__case_12 x1 x39 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1 x39 x40 x1002 x3500) (d_OP__case_13 x1 x39 x40 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x1 x39 x40 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1 x39 x40 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x1 x39 x40 x41 x3000 x3500 = case x41 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x1 x39 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x1 x39 x40 x1002 x3000 x3500) (nd_OP__case_13 x1 x39 x40 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x1 x39 x40 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x1 x39 x40 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x1 x39 x3500 = case x39 of
     (Curry_Prelude.OP_Cons x41 x42) -> let
          x43 = x41
           in (d_OP__case_11 x1 x42 x43 (Curry_Prelude.d_OP_eq_eq x43 (Curry_Prelude.C_Char 'l'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1 x1002 x3500) (d_OP__case_12 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x1 x39 x3000 x3500 = case x39 of
     (Curry_Prelude.OP_Cons x41 x42) -> let
          x2000 = x3000
           in (seq x2000 (let
               x43 = x41
                in (nd_OP__case_11 x1 x42 x43 (Curry_Prelude.d_OP_eq_eq x43 (Curry_Prelude.C_Char 'l'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1 x1002 x3000 x3500) (nd_OP__case_12 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x1 x42 x43 x44 x3500 = case x44 of
     Curry_Prelude.C_True -> d_OP__case_10 x1 x42 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1 x42 x43 x1002 x3500) (d_OP__case_11 x1 x42 x43 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x1 x42 x43 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1 x42 x43 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x1 x42 x43 x44 x3000 x3500 = case x44 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x1 x42 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1 x42 x43 x1002 x3000 x3500) (nd_OP__case_11 x1 x42 x43 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x1 x42 x43 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1 x42 x43 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x1 x42 x3500 = case x42 of
     (Curry_Prelude.OP_Cons x44 x45) -> let
          x46 = x44
           in (d_OP__case_9 x1 x45 x46 (Curry_Prelude.d_OP_eq_eq x46 (Curry_Prelude.C_Char 'u'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1 x1002 x3500) (d_OP__case_10 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x1 x42 x3000 x3500 = case x42 of
     (Curry_Prelude.OP_Cons x44 x45) -> let
          x2000 = x3000
           in (seq x2000 (let
               x46 = x44
                in (nd_OP__case_9 x1 x45 x46 (Curry_Prelude.d_OP_eq_eq x46 (Curry_Prelude.C_Char 'u'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x1 x1002 x3000 x3500) (nd_OP__case_10 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x1 x45 x46 x47 x3500 = case x47 of
     Curry_Prelude.C_True -> d_OP__case_8 x1 x45 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x45 x46 x1002 x3500) (d_OP__case_9 x1 x45 x46 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 x45 x46 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x45 x46 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x1 x45 x46 x47 x3000 x3500 = case x47 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x1 x45 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x45 x46 x1002 x3000 x3500) (nd_OP__case_9 x1 x45 x46 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 x45 x46 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x45 x46 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x1 x45 x3500 = case x45 of
     (Curry_Prelude.OP_Cons x47 x48) -> let
          x49 = x47
           in (d_OP__case_7 x1 x48 x49 (Curry_Prelude.d_OP_eq_eq x49 (Curry_Prelude.C_Char 'd'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x1002 x3500) (d_OP__case_8 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x1 x45 x3000 x3500 = case x45 of
     (Curry_Prelude.OP_Cons x47 x48) -> let
          x2000 = x3000
           in (seq x2000 (let
               x49 = x47
                in (nd_OP__case_7 x1 x48 x49 (Curry_Prelude.d_OP_eq_eq x49 (Curry_Prelude.C_Char 'd'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1 x1002 x3000 x3500) (nd_OP__case_8 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x1 x48 x49 x50 x3500 = case x50 of
     Curry_Prelude.C_True -> d_OP__case_6 x1 x48 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x48 x49 x1002 x3500) (d_OP__case_7 x1 x48 x49 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 x48 x49 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x48 x49 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x1 x48 x49 x50 x3000 x3500 = case x50 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x1 x48 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1 x48 x49 x1002 x3000 x3500) (nd_OP__case_7 x1 x48 x49 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x1 x48 x49 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1 x48 x49 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x1 x48 x3500 = case x48 of
     (Curry_Prelude.OP_Cons x50 x51) -> let
          x52 = x50
           in (d_OP__case_5 x1 x51 x52 (Curry_Prelude.d_OP_eq_eq x52 (Curry_Prelude.C_Char 'e'#) x3500) x3500)
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x1002 x3500) (d_OP__case_6 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x1 x48 x3000 x3500 = case x48 of
     (Curry_Prelude.OP_Cons x50 x51) -> let
          x2000 = x3000
           in (seq x2000 (let
               x52 = x50
                in (nd_OP__case_5 x1 x51 x52 (Curry_Prelude.d_OP_eq_eq x52 (Curry_Prelude.C_Char 'e'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1 x1002 x3000 x3500) (nd_OP__case_6 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x1 x51 x52 x53 x3500 = case x53 of
     Curry_Prelude.C_True -> d_OP__case_4 x1 x51 x3500
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x51 x52 x1002 x3500) (d_OP__case_5 x1 x51 x52 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x51 x52 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x51 x52 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x1 x51 x52 x53 x3000 x3500 = case x53 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x1 x51 x2000 x3500))
     Curry_Prelude.C_False -> C_ExtUnknown x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1 x51 x52 x1002 x3000 x3500) (nd_OP__case_5 x1 x51 x52 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x1 x51 x52 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1 x51 x52 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x51 x3500 = case x51 of
     Curry_Prelude.OP_List -> C_ExtNoImplicitPrelude
     (Curry_Prelude.OP_Cons x53 x54) -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x1002 x3500) (d_OP__case_4 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x51 x3000 x3500 = case x51 of
     Curry_Prelude.OP_List -> C_ExtNoImplicitPrelude
     (Curry_Prelude.OP_Cons x53 x54) -> C_ExtUnknown x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x1002 x3000 x3500) (nd_OP__case_4 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_44 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_43 x2 x4 x3500
     Curry_Prelude.C_False -> d_OP__case_42 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '1'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x2 x4 x5 x1002 x3500) (d_OP__case_44 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_44 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_43 x2 x4 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_42 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '1'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_44 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_44 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_44 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_44 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_42 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_41 x2 x4 x3500
     Curry_Prelude.C_False -> d_OP__case_40 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '2'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x2 x4 x5 x1002 x3500) (d_OP__case_42 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_42 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_41 x2 x4 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_40 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '2'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_42 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_42 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_42 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_42 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_40 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_39 x2 x4 x3500
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x2 x4 x5 x1002 x3500) (d_OP__case_40 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_40 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_39 x2 x4 x2000 x3500))
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_40 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_40 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_40 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_40 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_39 x2 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_OptimStrictSupply
     (Curry_Prelude.OP_Cons x10 x11) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x2 x1002 x3500) (d_OP__case_39 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_39 x2 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_OptimStrictSupply
     (Curry_Prelude.OP_Cons x10 x11) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_39 x2 x1002 x3000 x3500) (nd_OP__case_39 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_39 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_39 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_41 x2 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_OptimHigherOrder
     (Curry_Prelude.OP_Cons x8 x9) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x2 x1002 x3500) (d_OP__case_41 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_41 x2 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_OptimHigherOrder
     (Curry_Prelude.OP_Cons x8 x9) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_41 x2 x1002 x3000 x3500) (nd_OP__case_41 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_41 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_41 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_43 x2 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_OptimNone
     (Curry_Prelude.OP_Cons x6 x7) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x2 x1002 x3500) (d_OP__case_43 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_43 x2 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_OptimNone
     (Curry_Prelude.OP_Cons x6 x7) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_43 x2 x1002 x3000 x3500) (nd_OP__case_43 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_43 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_43 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_54 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_53 x2 x4 x3500
     Curry_Prelude.C_False -> d_OP__case_52 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '1'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_54 x2 x4 x5 x1002 x3500) (d_OP__case_54 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_54 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_54 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_54 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_53 x2 x4 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_52 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '1'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_54 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_54 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_54 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_54 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_52 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_51 x2 x4 x3500
     Curry_Prelude.C_False -> d_OP__case_50 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '2'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_52 x2 x4 x5 x1002 x3500) (d_OP__case_52 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_52 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_52 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_52 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_51 x2 x4 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_50 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '2'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_52 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_52 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_52 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_52 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_50 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_49 x2 x4 x3500
     Curry_Prelude.C_False -> d_OP__case_48 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '3'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_50 x2 x4 x5 x1002 x3500) (d_OP__case_50 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_50 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_50 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_50 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_49 x2 x4 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_48 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '3'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_50 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_50 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_50 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_50 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_48 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_47 x2 x4 x3500
     Curry_Prelude.C_False -> d_OP__case_46 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '4'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_48 x2 x4 x5 x1002 x3500) (d_OP__case_48 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_48 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_48 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_48 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_47 x2 x4 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_46 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '4'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_48 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_48 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_48 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_48 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_46 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_45 x2 x4 x3500
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_46 x2 x4 x5 x1002 x3500) (d_OP__case_46 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_46 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_46 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_46 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_45 x2 x4 x2000 x3500))
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_46 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_46 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_46 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_46 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_45 x2 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_VerbDetails
     (Curry_Prelude.OP_Cons x14 x15) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x2 x1002 x3500) (d_OP__case_45 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_45 x2 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_VerbDetails
     (Curry_Prelude.OP_Cons x14 x15) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_45 x2 x1002 x3000 x3500) (nd_OP__case_45 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_45 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_45 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_47 x2 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_VerbAnalysis
     (Curry_Prelude.OP_Cons x12 x13) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_47 x2 x1002 x3500) (d_OP__case_47 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_47 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_47 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_47 x2 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_VerbAnalysis
     (Curry_Prelude.OP_Cons x12 x13) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_47 x2 x1002 x3000 x3500) (nd_OP__case_47 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_47 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_47 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_49 x2 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_VerbFrontend
     (Curry_Prelude.OP_Cons x10 x11) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_49 x2 x1002 x3500) (d_OP__case_49 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_49 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_49 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_49 x2 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_VerbFrontend
     (Curry_Prelude.OP_Cons x10 x11) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_49 x2 x1002 x3000 x3500) (nd_OP__case_49 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_49 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_49 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_51 x2 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_VerbStatus
     (Curry_Prelude.OP_Cons x8 x9) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_51 x2 x1002 x3500) (d_OP__case_51 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_51 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_51 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_51 x2 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_VerbStatus
     (Curry_Prelude.OP_Cons x8 x9) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_51 x2 x1002 x3000 x3500) (nd_OP__case_51 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_51 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_51 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_53 x2 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_VerbQuiet
     (Curry_Prelude.OP_Cons x6 x7) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_53 x2 x1002 x3500) (d_OP__case_53 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_53 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_53 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_53 x2 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> C_VerbQuiet
     (Curry_Prelude.OP_Cons x6 x7) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_53 x2 x1002 x3000 x3500) (nd_OP__case_53 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_53 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_53 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
