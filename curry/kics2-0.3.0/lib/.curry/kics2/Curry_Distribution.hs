{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Distribution (C_FrontendTarget (..), C_FrontendParams, d_C_rcFileName, d_C_rcFileContents, d_C_getRcVar, d_C_getRcVars, d_C_currySubdir, d_C_inCurrySubdir, d_C_addCurrySubdir, d_C_lookupFileInLoadPath, d_C_findFileInLoadPath, d_C_readFirstFileInLoadPath, d_C_getLoadPath, d_C_getLoadPathForFile, d_C_defaultParams, d_C_rcParams, d_C_setQuiet, d_C_setExtended, d_C_setOverlapWarn, d_C_setFullPath, d_C_setOutfile, d_C_setLogfile, d_C_quiet, d_C_extended, d_C_overlapWarn, d_C_fullPath, d_C_outfile, d_C_logfile, d_C_callFrontend, d_C_callFrontendWithParams, d_C_curryCompiler, d_C_curryCompilerMajorVersion, d_C_curryCompilerMinorVersion, d_C_curryRuntime, d_C_curryRuntimeMajorVersion, d_C_curryRuntimeMinorVersion, d_C_installDir) where

import Basics
import qualified Curry_Char
import qualified Curry_FileGoodies
import qualified Curry_FilePath
import qualified Curry_IO
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_PropertyFile
import qualified Curry_System
import qualified Installation  as I
import qualified Curry_Prelude as CP


data C_FrontendTarget
     = C_FCY
     | C_FINT
     | C_ACY
     | C_UACY
     | C_HTML
     | C_CY
     | Choice_C_FrontendTarget Cover ID C_FrontendTarget C_FrontendTarget
     | Choices_C_FrontendTarget Cover ID ([C_FrontendTarget])
     | Fail_C_FrontendTarget Cover FailInfo
     | Guard_C_FrontendTarget Cover Constraints C_FrontendTarget

instance Show C_FrontendTarget where
  showsPrec d (Choice_C_FrontendTarget cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_FrontendTarget cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_FrontendTarget cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_FrontendTarget cd info) = showChar '!'
  showsPrec _ C_FCY = showString "FCY"
  showsPrec _ C_FINT = showString "FINT"
  showsPrec _ C_ACY = showString "ACY"
  showsPrec _ C_UACY = showString "UACY"
  showsPrec _ C_HTML = showString "HTML"
  showsPrec _ C_CY = showString "CY"


instance Read C_FrontendTarget where
  readsPrec _ s = (readParen False (\r -> [ (C_FCY,r0) | (_,r0) <- readQualified "Distribution" "FCY" r]) s) ++ ((readParen False (\r -> [ (C_FINT,r0) | (_,r0) <- readQualified "Distribution" "FINT" r]) s) ++ ((readParen False (\r -> [ (C_ACY,r0) | (_,r0) <- readQualified "Distribution" "ACY" r]) s) ++ ((readParen False (\r -> [ (C_UACY,r0) | (_,r0) <- readQualified "Distribution" "UACY" r]) s) ++ ((readParen False (\r -> [ (C_HTML,r0) | (_,r0) <- readQualified "Distribution" "HTML" r]) s) ++ (readParen False (\r -> [ (C_CY,r0) | (_,r0) <- readQualified "Distribution" "CY" r]) s)))))


instance NonDet C_FrontendTarget where
  choiceCons = Choice_C_FrontendTarget
  choicesCons = Choices_C_FrontendTarget
  failCons = Fail_C_FrontendTarget
  guardCons = Guard_C_FrontendTarget
  try (Choice_C_FrontendTarget cd i x y) = tryChoice cd i x y
  try (Choices_C_FrontendTarget cd i xs) = tryChoices cd i xs
  try (Fail_C_FrontendTarget cd info) = Fail cd info
  try (Guard_C_FrontendTarget cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_FrontendTarget cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_FrontendTarget cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_FrontendTarget cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_FrontendTarget cd i _) = error ("Distribution.FrontendTarget.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_FrontendTarget cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_FrontendTarget cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_FrontendTarget where
  generate s c = Choices_C_FrontendTarget c (freeID [0,0,0,0,0,0] s) [C_FCY,C_FINT,C_ACY,C_UACY,C_HTML,C_CY]


instance NormalForm C_FrontendTarget where
  ($!!) cont C_FCY d cs = cont C_FCY d cs
  ($!!) cont C_FINT d cs = cont C_FINT d cs
  ($!!) cont C_ACY d cs = cont C_ACY d cs
  ($!!) cont C_UACY d cs = cont C_UACY d cs
  ($!!) cont C_HTML d cs = cont C_HTML d cs
  ($!!) cont C_CY d cs = cont C_CY d cs
  ($!!) cont (Choice_C_FrontendTarget cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_FrontendTarget cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_FrontendTarget cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_FrontendTarget cd info) _ _ = failCons cd info
  ($##) cont C_FCY d cs = cont C_FCY d cs
  ($##) cont C_FINT d cs = cont C_FINT d cs
  ($##) cont C_ACY d cs = cont C_ACY d cs
  ($##) cont C_UACY d cs = cont C_UACY d cs
  ($##) cont C_HTML d cs = cont C_HTML d cs
  ($##) cont C_CY d cs = cont C_CY d cs
  ($##) cont (Choice_C_FrontendTarget cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_FrontendTarget cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_FrontendTarget cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_FrontendTarget cd info) _ _ = failCons cd info
  searchNF _ cont C_FCY = cont C_FCY
  searchNF _ cont C_FINT = cont C_FINT
  searchNF _ cont C_ACY = cont C_ACY
  searchNF _ cont C_UACY = cont C_UACY
  searchNF _ cont C_HTML = cont C_HTML
  searchNF _ cont C_CY = cont C_CY
  searchNF _ _ x = error ("Distribution.FrontendTarget.searchNF: no constructor: " ++ (show x))


instance Unifiable C_FrontendTarget where
  (=.=) C_FCY C_FCY d cs = C_Success
  (=.=) C_FINT C_FINT d cs = C_Success
  (=.=) C_ACY C_ACY d cs = C_Success
  (=.=) C_UACY C_UACY d cs = C_Success
  (=.=) C_HTML C_HTML d cs = C_Success
  (=.=) C_CY C_CY d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_FCY C_FCY d cs = C_Success
  (=.<=) C_FINT C_FINT d cs = C_Success
  (=.<=) C_ACY C_ACY d cs = C_Success
  (=.<=) C_UACY C_UACY d cs = C_Success
  (=.<=) C_HTML C_HTML d cs = C_Success
  (=.<=) C_CY C_CY d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_FCY = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_FINT = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_ACY = ((i :=: (ChooseN 2 0)):(concat []))
  bind cd i C_UACY = ((i :=: (ChooseN 3 0)):(concat []))
  bind cd i C_HTML = ((i :=: (ChooseN 4 0)):(concat []))
  bind cd i C_CY = ((i :=: (ChooseN 5 0)):(concat []))
  bind d i (Choice_C_FrontendTarget cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_FrontendTarget cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_FrontendTarget cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_FrontendTarget cd i _) = error ("Distribution.FrontendTarget.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_FrontendTarget cd info) = [(Unsolvable info)]
  bind d i (Guard_C_FrontendTarget cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_FCY = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_FINT = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_ACY = [(i :=: (ChooseN 2 0))]
  lazyBind cd i C_UACY = [(i :=: (ChooseN 3 0))]
  lazyBind cd i C_HTML = [(i :=: (ChooseN 4 0))]
  lazyBind cd i C_CY = [(i :=: (ChooseN 5 0))]
  lazyBind d i (Choice_C_FrontendTarget cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_FrontendTarget cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_FrontendTarget cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_FrontendTarget cd i _) = error ("Distribution.FrontendTarget.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_FrontendTarget cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_FrontendTarget cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_FrontendTarget where
  (=?=) (Choice_C_FrontendTarget cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_FrontendTarget cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_FrontendTarget cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_FrontendTarget cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_FrontendTarget cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_FrontendTarget cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_FrontendTarget cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_FrontendTarget cd info) _ _ = failCons cd info
  (=?=) C_FCY C_FCY d cs = Curry_Prelude.C_True
  (=?=) C_FINT C_FINT d cs = Curry_Prelude.C_True
  (=?=) C_ACY C_ACY d cs = Curry_Prelude.C_True
  (=?=) C_UACY C_UACY d cs = Curry_Prelude.C_True
  (=?=) C_HTML C_HTML d cs = Curry_Prelude.C_True
  (=?=) C_CY C_CY d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_FrontendTarget cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_FrontendTarget cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_FrontendTarget cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_FrontendTarget cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_FrontendTarget cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_FrontendTarget cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_FrontendTarget cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_FrontendTarget cd info) _ _ = failCons cd info
  (<?=) C_FCY C_FCY d cs = Curry_Prelude.C_True
  (<?=) C_FCY C_FINT _ _ = Curry_Prelude.C_True
  (<?=) C_FCY C_ACY _ _ = Curry_Prelude.C_True
  (<?=) C_FCY C_UACY _ _ = Curry_Prelude.C_True
  (<?=) C_FCY C_HTML _ _ = Curry_Prelude.C_True
  (<?=) C_FCY C_CY _ _ = Curry_Prelude.C_True
  (<?=) C_FINT C_FINT d cs = Curry_Prelude.C_True
  (<?=) C_FINT C_ACY _ _ = Curry_Prelude.C_True
  (<?=) C_FINT C_UACY _ _ = Curry_Prelude.C_True
  (<?=) C_FINT C_HTML _ _ = Curry_Prelude.C_True
  (<?=) C_FINT C_CY _ _ = Curry_Prelude.C_True
  (<?=) C_ACY C_ACY d cs = Curry_Prelude.C_True
  (<?=) C_ACY C_UACY _ _ = Curry_Prelude.C_True
  (<?=) C_ACY C_HTML _ _ = Curry_Prelude.C_True
  (<?=) C_ACY C_CY _ _ = Curry_Prelude.C_True
  (<?=) C_UACY C_UACY d cs = Curry_Prelude.C_True
  (<?=) C_UACY C_HTML _ _ = Curry_Prelude.C_True
  (<?=) C_UACY C_CY _ _ = Curry_Prelude.C_True
  (<?=) C_HTML C_HTML d cs = Curry_Prelude.C_True
  (<?=) C_HTML C_CY _ _ = Curry_Prelude.C_True
  (<?=) C_CY C_CY d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_FrontendParams
     = C_FrontendParams Curry_Prelude.C_Bool Curry_Prelude.C_Bool Curry_Prelude.C_Bool (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | Choice_C_FrontendParams Cover ID C_FrontendParams C_FrontendParams
     | Choices_C_FrontendParams Cover ID ([C_FrontendParams])
     | Fail_C_FrontendParams Cover FailInfo
     | Guard_C_FrontendParams Cover Constraints C_FrontendParams

instance Show C_FrontendParams where
  showsPrec d (Choice_C_FrontendParams cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_FrontendParams cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_FrontendParams cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_FrontendParams cd info) = showChar '!'
  showsPrec _ (C_FrontendParams x1 x2 x3 x4 x5 x6) = (showString "(FrontendParams") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . ((showChar ' ') . ((shows x6) . (showChar ')')))))))))))))


instance Read C_FrontendParams where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_FrontendParams x1 x2 x3 x4 x5 x6,r6) | (_,r0) <- readQualified "Distribution" "FrontendParams" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4, (x6,r6) <- readsPrec 11 r5]) s


instance NonDet C_FrontendParams where
  choiceCons = Choice_C_FrontendParams
  choicesCons = Choices_C_FrontendParams
  failCons = Fail_C_FrontendParams
  guardCons = Guard_C_FrontendParams
  try (Choice_C_FrontendParams cd i x y) = tryChoice cd i x y
  try (Choices_C_FrontendParams cd i xs) = tryChoices cd i xs
  try (Fail_C_FrontendParams cd info) = Fail cd info
  try (Guard_C_FrontendParams cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_FrontendParams cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_FrontendParams cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_FrontendParams cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_FrontendParams cd i _) = error ("Distribution.FrontendParams.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_FrontendParams cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_FrontendParams cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_FrontendParams where
  generate s c = Choices_C_FrontendParams c (freeID [6] s) [(C_FrontendParams (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_FrontendParams where
  ($!!) cont (C_FrontendParams x1 x2 x3 x4 x5 x6) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> cont (C_FrontendParams y1 y2 y3 y4 y5 y6) d cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_FrontendParams cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_FrontendParams cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_FrontendParams cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_FrontendParams cd info) _ _ = failCons cd info
  ($##) cont (C_FrontendParams x1 x2 x3 x4 x5 x6) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> cont (C_FrontendParams y1 y2 y3 y4 y5 y6) d cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_FrontendParams cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_FrontendParams cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_FrontendParams cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_FrontendParams cd info) _ _ = failCons cd info
  searchNF search cont (C_FrontendParams x1 x2 x3 x4 x5 x6) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> cont (C_FrontendParams y1 y2 y3 y4 y5 y6)) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Distribution.FrontendParams.searchNF: no constructor: " ++ (show x))


instance Unifiable C_FrontendParams where
  (=.=) (C_FrontendParams x1 x2 x3 x4 x5 x6) (C_FrontendParams y1 y2 y3 y4 y5 y6) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & (((x6 =:= y6) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_FrontendParams x1 x2 x3 x4 x5 x6) (C_FrontendParams y1 y2 y3 y4 y5 y6) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & (((x6 =:<= y6) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_FrontendParams x3 x4 x5 x6 x7 x8) = ((i :=: (ChooseN 0 6)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (leftID (rightID i))) x6),(bind cd (rightID (leftID (rightID i))) x7),(bind cd (rightID (rightID i)) x8)]))
  bind d i (Choice_C_FrontendParams cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_FrontendParams cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_FrontendParams cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_FrontendParams cd i _) = error ("Distribution.FrontendParams.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_FrontendParams cd info) = [(Unsolvable info)]
  bind d i (Guard_C_FrontendParams cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_FrontendParams x3 x4 x5 x6 x7 x8) = [(i :=: (ChooseN 0 6)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID i))) x6))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x7))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x8)))]
  lazyBind d i (Choice_C_FrontendParams cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_FrontendParams cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_FrontendParams cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_FrontendParams cd i _) = error ("Distribution.FrontendParams.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_FrontendParams cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_FrontendParams cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_FrontendParams where
  (=?=) (Choice_C_FrontendParams cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_FrontendParams cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_FrontendParams cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_FrontendParams cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_FrontendParams cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_FrontendParams cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_FrontendParams cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_FrontendParams cd info) _ _ = failCons cd info
  (=?=) (C_FrontendParams x1 x2 x3 x4 x5 x6) (C_FrontendParams y1 y2 y3 y4 y5 y6) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (((x6 Curry_Prelude.=?= y6) d) cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_C_FrontendParams cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_FrontendParams cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_FrontendParams cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_FrontendParams cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_FrontendParams cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_FrontendParams cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_FrontendParams cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_FrontendParams cd info) _ _ = failCons cd info
  (<?=) (C_FrontendParams x1 x2 x3 x4 x5 x6) (C_FrontendParams y1 y2 y3 y4 y5 y6) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x5 y5 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (((x6 Curry_Prelude.<?= y6) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


d_C_rcFileName :: Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_rcFileName x3250 x3500 = let
     x1 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.d_OP_plus_plus (d_C_curryCompiler x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List)) x3250 x3500)
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_System.d_C_getEnviron (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) Curry_Prelude.OP_List)))) x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (Curry_Prelude.d_C_flip (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) x1) x3250 x3500) x3250 x3500)

d_C_rcFileContents :: Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_rcFileContents x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_rcFileName x3250 x3500) Curry_PropertyFile.d_C_readPropertyFile x3250 x3500

d_C_getRcVar :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getRcVar x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getRcVars (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return Curry_Prelude.d_C_head x3250 x3500) x3250 x3500

d_C_getRcVars :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_getRcVars x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_rcFileContents x3250 x3500) (d_OP_getRcVars_dot___hash_lambda1 x1) x3250 x3500

d_OP_getRcVars_dot___hash_lambda1 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_getRcVars_dot___hash_lambda1 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_C_map (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_C_lookup) (Curry_Prelude.d_C_map d_OP_getRcVars_dot___hash_lambda1_dot___hash_lambda2 x2 x3250 x3500)) (Curry_Prelude.d_C_map (Curry_Prelude.d_C_map Curry_Char.d_C_toLower) x1 x3250 x3500) x3250 x3500) x3250 x3500

d_OP_getRcVars_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getRcVars_dot___hash_lambda1_dot___hash_lambda2 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_map Curry_Char.d_C_toLower x2 x3250 x3500) x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getRcVars_dot___hash_lambda1_dot___hash_lambda2 x1002 x3250 x3500) (d_OP_getRcVars_dot___hash_lambda1_dot___hash_lambda2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getRcVars_dot___hash_lambda1_dot___hash_lambda2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getRcVars_dot___hash_lambda1_dot___hash_lambda2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_currySubdir :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_currySubdir x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))

d_C_inCurrySubdir :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_inCurrySubdir x1 x3250 x3500 = let
     x2 = Curry_FileGoodies.d_C_splitDirectoryBaseName x1 x3250 x3500
     x3 = d_OP_inCurrySubdir_dot___hash_selFP2_hash_base x2 x3250 x3500
     x4 = d_OP_inCurrySubdir_dot___hash_selFP3_hash_file x2 x3250 x3500
      in (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) x3 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_C_currySubdir x3250 x3500) x3250 x3500) x4 x3250 x3500) x3250 x3500)

d_OP_inCurrySubdir_dot___hash_selFP2_hash_base :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_inCurrySubdir_dot___hash_selFP2_hash_base x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_inCurrySubdir_dot___hash_selFP2_hash_base x1002 x3250 x3500) (d_OP_inCurrySubdir_dot___hash_selFP2_hash_base x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_inCurrySubdir_dot___hash_selFP2_hash_base z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_inCurrySubdir_dot___hash_selFP2_hash_base x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_inCurrySubdir_dot___hash_selFP3_hash_file :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_inCurrySubdir_dot___hash_selFP3_hash_file x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_inCurrySubdir_dot___hash_selFP3_hash_file x1002 x3250 x3500) (d_OP_inCurrySubdir_dot___hash_selFP3_hash_file x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_inCurrySubdir_dot___hash_selFP3_hash_file z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_inCurrySubdir_dot___hash_selFP3_hash_file x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_addCurrySubdir :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_addCurrySubdir x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) x1 x3250 x3500) (d_C_currySubdir x3250 x3500) x3250 x3500

d_C_getSysLibPath :: Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getSysLibPath x3250 x3500 = d_OP__case_52 (d_C_curryCompiler x3250 x3500) x3250 x3500

d_OP_getSysLibPath_dot___hash_lambda4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getSysLibPath_dot___hash_lambda4 x1 x3250 x3500 = Curry_Prelude.d_C_return (d_OP__case_31 x1 (Curry_Prelude.d_C_null x1 x3250 x3500) x3250 x3500) x3250 x3500

d_C_lookupFileInLoadPath :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_lookupFileInLoadPath x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getLoadPathForFile x1 x3250 x3500) (Curry_FileGoodies.d_C_lookupFileInPath (Curry_FileGoodies.d_C_baseName x1 x3250 x3500) (Curry_Prelude.OP_Cons Curry_Prelude.OP_List Curry_Prelude.OP_List)) x3250 x3500

d_C_findFileInLoadPath :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_findFileInLoadPath x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getLoadPathForFile x1 x3250 x3500) (Curry_FileGoodies.d_C_getFileInPath (Curry_FileGoodies.d_C_baseName x1 x3250 x3500) (Curry_Prelude.OP_Cons Curry_Prelude.OP_List Curry_Prelude.OP_List)) x3250 x3500

d_C_readFirstFileInLoadPath :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_readFirstFileInLoadPath x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_findFileInLoadPath x1 x3250 x3500) Curry_Prelude.d_C_readFile x3250 x3500

d_C_getLoadPath :: Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getLoadPath x3250 x3500 = d_C_getLoadPathForFile (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List))) x3250 x3500

d_C_getLoadPathForFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getLoadPathForFile x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_concatMap d_OP_getLoadPathForFile_dot___hash_lambda5 x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (d_C_getSysLibPath x3250 x3500) (d_OP_getLoadPathForFile_dot___hash_lambda6 x2 x1) x3250 x3500)

d_OP_getLoadPathForFile_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getLoadPathForFile_dot___hash_lambda5 x1 x3250 x3500 = Curry_Prelude.OP_Cons x1 (Curry_Prelude.OP_Cons (d_C_addCurrySubdir x1 x3250 x3500) Curry_Prelude.OP_List)

d_OP_getLoadPathForFile_dot___hash_lambda6 :: (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getLoadPathForFile_dot___hash_lambda6 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getRcVar (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))) x3250 x3500) (d_OP_getLoadPathForFile_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3) x3250 x3500

nd_OP_getLoadPathForFile_dot___hash_lambda6 :: Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_getLoadPathForFile_dot___hash_lambda6 x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getRcVar (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))) x3250 x3500) (wrapNX id (nd_OP_getLoadPathForFile_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3)) x2000 x3250 x3500))

d_OP_getLoadPathForFile_dot___hash_lambda6_dot___hash_lambda7 :: (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getLoadPathForFile_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3 x4 x3250 x3500 = let
     x5 = Curry_FileGoodies.d_C_dirName x2 x3250 x3500
      in (d_OP__case_30 x3 x4 x5 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem (d_C_curryCompiler x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500)

nd_OP_getLoadPathForFile_dot___hash_lambda6_dot___hash_lambda7 :: Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_getLoadPathForFile_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x5 = Curry_FileGoodies.d_C_dirName x2 x3250 x3500
           in (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_30 x3 x4 x5 x1 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_elem (d_C_curryCompiler x3250 x3500) x2000 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List))) x2001 x3250 x3500)))) x2003 x3250 x3500))))))

d_OP_getLoadPathForFile_dot___hash_lambda6_dot___hash_lambda7_dot___hash_lambda8 :: (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getLoadPathForFile_dot___hash_lambda6_dot___hash_lambda7_dot___hash_lambda8 x1 x2 x3 x4 x5 x3250 x3500 = let
     x6 = Curry_Prelude.d_C_maybe Curry_Prelude.OP_List Curry_FileGoodies.d_C_splitPath x3 x3250 x3500
      in (Curry_Prelude.d_C_return (Curry_Prelude.d_C_apply x1 (Curry_Prelude.OP_Cons x2 (Curry_Prelude.d_OP_plus_plus (d_OP__case_29 x5 (Curry_Prelude.d_C_null x5 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus x6 x4 x3250 x3500) x3250 x3500)) x3250 x3500) x3250 x3500)

nd_OP_getLoadPathForFile_dot___hash_lambda6_dot___hash_lambda7_dot___hash_lambda8 :: Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_getLoadPathForFile_dot___hash_lambda6_dot___hash_lambda7_dot___hash_lambda8 x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (let
               x6 = Curry_Prelude.nd_C_maybe Curry_Prelude.OP_List (wrapDX id Curry_FileGoodies.d_C_splitPath) x3 x2000 x3250 x3500
                in (Curry_Prelude.d_C_return (Curry_Prelude.nd_C_apply x1 (Curry_Prelude.OP_Cons x2 (Curry_Prelude.d_OP_plus_plus (d_OP__case_29 x5 (Curry_Prelude.d_C_null x5 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus x6 x4 x3250 x3500) x3250 x3500)) x2001 x3250 x3500) x3250 x3500))))))

d_C_defaultParams :: Cover -> ConstStore -> C_FrontendParams
d_C_defaultParams x3250 x3500 = C_FrontendParams Curry_Prelude.C_False Curry_Prelude.C_True Curry_Prelude.C_True Curry_Prelude.C_Nothing Curry_Prelude.C_Nothing Curry_Prelude.C_Nothing

d_C_rcParams :: Cover -> ConstStore -> Curry_Prelude.C_IO C_FrontendParams
d_C_rcParams x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getRcVars (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))))))))))) Curry_Prelude.OP_List)) x3250 x3500) d_OP_rcParams_dot___hash_lambda9 x3250 x3500

d_OP_rcParams_dot___hash_lambda9 :: Curry_Prelude.OP_List (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.C_IO C_FrontendParams
d_OP_rcParams_dot___hash_lambda9 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_28 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_rcParams_dot___hash_lambda9 x1002 x3250 x3500) (d_OP_rcParams_dot___hash_lambda9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_rcParams_dot___hash_lambda9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_rcParams_dot___hash_lambda9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_setQuiet :: Curry_Prelude.C_Bool -> C_FrontendParams -> Cover -> ConstStore -> C_FrontendParams
d_C_setQuiet x1 x2 x3250 x3500 = case x2 of
     (C_FrontendParams x3 x4 x5 x6 x7 x8) -> C_FrontendParams x1 x4 x5 x6 x7 x8
     (Choice_C_FrontendParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_setQuiet x1 x1002 x3250 x3500) (d_C_setQuiet x1 x1003 x3250 x3500)
     (Choices_C_FrontendParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_setQuiet x1 z x3250 x3500) x1002
     (Guard_C_FrontendParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_setQuiet x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FrontendParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_setExtended :: Curry_Prelude.C_Bool -> C_FrontendParams -> Cover -> ConstStore -> C_FrontendParams
d_C_setExtended x1 x2 x3250 x3500 = case x2 of
     (C_FrontendParams x3 x4 x5 x6 x7 x8) -> C_FrontendParams x3 x1 x5 x6 x7 x8
     (Choice_C_FrontendParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_setExtended x1 x1002 x3250 x3500) (d_C_setExtended x1 x1003 x3250 x3500)
     (Choices_C_FrontendParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_setExtended x1 z x3250 x3500) x1002
     (Guard_C_FrontendParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_setExtended x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FrontendParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_setOverlapWarn :: Curry_Prelude.C_Bool -> C_FrontendParams -> Cover -> ConstStore -> C_FrontendParams
d_C_setOverlapWarn x1 x2 x3250 x3500 = case x2 of
     (C_FrontendParams x3 x4 x5 x6 x7 x8) -> C_FrontendParams x3 x4 x1 x6 x7 x8
     (Choice_C_FrontendParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_setOverlapWarn x1 x1002 x3250 x3500) (d_C_setOverlapWarn x1 x1003 x3250 x3500)
     (Choices_C_FrontendParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_setOverlapWarn x1 z x3250 x3500) x1002
     (Guard_C_FrontendParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_setOverlapWarn x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FrontendParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_setFullPath :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_FrontendParams -> Cover -> ConstStore -> C_FrontendParams
d_C_setFullPath x1 x2 x3250 x3500 = case x2 of
     (C_FrontendParams x3 x4 x5 x6 x7 x8) -> C_FrontendParams x3 x4 x5 (Curry_Prelude.C_Just x1) x7 x8
     (Choice_C_FrontendParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_setFullPath x1 x1002 x3250 x3500) (d_C_setFullPath x1 x1003 x3250 x3500)
     (Choices_C_FrontendParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_setFullPath x1 z x3250 x3500) x1002
     (Guard_C_FrontendParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_setFullPath x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FrontendParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_setOutfile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_FrontendParams -> Cover -> ConstStore -> C_FrontendParams
d_C_setOutfile x1 x2 x3250 x3500 = case x2 of
     (C_FrontendParams x3 x4 x5 x6 x7 x8) -> C_FrontendParams x3 x4 x5 x6 (Curry_Prelude.C_Just x1) x8
     (Choice_C_FrontendParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_setOutfile x1 x1002 x3250 x3500) (d_C_setOutfile x1 x1003 x3250 x3500)
     (Choices_C_FrontendParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_setOutfile x1 z x3250 x3500) x1002
     (Guard_C_FrontendParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_setOutfile x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FrontendParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_setLogfile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_FrontendParams -> Cover -> ConstStore -> C_FrontendParams
d_C_setLogfile x1 x2 x3250 x3500 = case x2 of
     (C_FrontendParams x3 x4 x5 x6 x7 x8) -> C_FrontendParams x3 x4 x5 x6 x7 (Curry_Prelude.C_Just x1)
     (Choice_C_FrontendParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_setLogfile x1 x1002 x3250 x3500) (d_C_setLogfile x1 x1003 x3250 x3500)
     (Choices_C_FrontendParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_setLogfile x1 z x3250 x3500) x1002
     (Guard_C_FrontendParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_setLogfile x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FrontendParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_quiet :: C_FrontendParams -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_quiet x1 x3250 x3500 = case x1 of
     (C_FrontendParams x2 x3 x4 x5 x6 x7) -> x2
     (Choice_C_FrontendParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_quiet x1002 x3250 x3500) (d_C_quiet x1003 x3250 x3500)
     (Choices_C_FrontendParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_quiet z x3250 x3500) x1002
     (Guard_C_FrontendParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_quiet x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FrontendParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_extended :: C_FrontendParams -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_extended x1 x3250 x3500 = case x1 of
     (C_FrontendParams x2 x3 x4 x5 x6 x7) -> x3
     (Choice_C_FrontendParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_extended x1002 x3250 x3500) (d_C_extended x1003 x3250 x3500)
     (Choices_C_FrontendParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_extended z x3250 x3500) x1002
     (Guard_C_FrontendParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_extended x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FrontendParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_overlapWarn :: C_FrontendParams -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_overlapWarn x1 x3250 x3500 = case x1 of
     (C_FrontendParams x2 x3 x4 x5 x6 x7) -> x4
     (Choice_C_FrontendParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_overlapWarn x1002 x3250 x3500) (d_C_overlapWarn x1003 x3250 x3500)
     (Choices_C_FrontendParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_overlapWarn z x3250 x3500) x1002
     (Guard_C_FrontendParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_overlapWarn x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FrontendParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_fullPath :: C_FrontendParams -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_fullPath x1 x3250 x3500 = case x1 of
     (C_FrontendParams x2 x3 x4 x5 x6 x7) -> x5
     (Choice_C_FrontendParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fullPath x1002 x3250 x3500) (d_C_fullPath x1003 x3250 x3500)
     (Choices_C_FrontendParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fullPath z x3250 x3500) x1002
     (Guard_C_FrontendParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fullPath x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FrontendParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_outfile :: C_FrontendParams -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_outfile x1 x3250 x3500 = case x1 of
     (C_FrontendParams x2 x3 x4 x5 x6 x7) -> x6
     (Choice_C_FrontendParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_outfile x1002 x3250 x3500) (d_C_outfile x1003 x3250 x3500)
     (Choices_C_FrontendParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_outfile z x3250 x3500) x1002
     (Guard_C_FrontendParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_outfile x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FrontendParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_logfile :: C_FrontendParams -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_logfile x1 x3250 x3500 = case x1 of
     (C_FrontendParams x2 x3 x4 x5 x6 x7) -> x7
     (Choice_C_FrontendParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_logfile x1002 x3250 x3500) (d_C_logfile x1003 x3250 x3500)
     (Choices_C_FrontendParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_logfile z x3250 x3500) x1002
     (Guard_C_FrontendParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_logfile x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FrontendParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_callFrontend :: C_FrontendTarget -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_callFrontend x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_rcParams x3250 x3500) (d_OP_callFrontend_dot___hash_lambda10 x2 x1) x3250 x3500

d_OP_callFrontend_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_FrontendTarget -> C_FrontendParams -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_callFrontend_dot___hash_lambda10 x1 x2 x3 x3250 x3500 = d_C_callFrontendWithParams x2 x3 x1 x3250 x3500

d_C_callFrontendWithParams :: C_FrontendTarget -> C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_callFrontendWithParams x1 x2 x3 x3250 x3500 = let
     x4 = d_OP__case_26 x2 (d_C_curryCompiler x3250 x3500) x3250 x3500
     x36 = d_OP__case_5 (Curry_Prelude.d_OP_eq_eq (d_C_curryCompiler x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500
     x37 = Curry_Prelude.d_C_concat (Curry_Prelude.OP_Cons (d_OP__case_4 x2 x36 (d_C_quiet x2 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (d_OP__case_3 x2 (d_C_extended x2 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (d_OP__case_2 x2 (d_C_overlapWarn x2 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_maybe Curry_Prelude.OP_List (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (d_C_outfile x2 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_maybe Curry_Prelude.OP_List d_OP_callFrontendWithParams_dot___hash_lambda15 (d_C_fullPath x2 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List))))) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq x4 (d_OP_callFrontendWithParams_dot___hash_lambda16 x2 x3 x37 x1) x3250 x3500)

d_OP_callFrontendWithParams_dot___hash_lambda12 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_callFrontendWithParams_dot___hash_lambda12 x1 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_C_installDir x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) Curry_Prelude.OP_List)))) x3250 x3500) x1 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_callFrontendWithParams_dot___hash_lambda13 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_callFrontendWithParams_dot___hash_lambda13 x1 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_C_installDir x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_callFrontendWithParams_dot___hash_lambda13_dot___hash_lambda14 x3250 x3500) x1 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_callFrontendWithParams_dot___hash_lambda13_dot___hash_lambda14 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_callFrontendWithParams_dot___hash_lambda13_dot___hash_lambda14 x1 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500

d_OP_callFrontendWithParams_dot_showFrontendTarget_dot_120 :: C_FrontendTarget -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_callFrontendWithParams_dot_showFrontendTarget_dot_120 x1 x3250 x3500 = case x1 of
     C_FCY -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))
     C_FINT -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))
     C_ACY -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))
     C_UACY -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))
     C_HTML -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))
     C_CY -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))))))))
     (Choice_C_FrontendTarget x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_callFrontendWithParams_dot_showFrontendTarget_dot_120 x1002 x3250 x3500) (d_OP_callFrontendWithParams_dot_showFrontendTarget_dot_120 x1003 x3250 x3500)
     (Choices_C_FrontendTarget x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_callFrontendWithParams_dot_showFrontendTarget_dot_120 z x3250 x3500) x1002
     (Guard_C_FrontendTarget x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_callFrontendWithParams_dot_showFrontendTarget_dot_120 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FrontendTarget x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_callFrontendWithParams_dot___hash_lambda15 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_callFrontendWithParams_dot___hash_lambda15 x1 x3250 x3500 = d_OP__case_1 x1 (Curry_Prelude.d_OP_eq_eq (d_C_curryCompiler x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500

d_OP_callFrontendWithParams_dot___hash_lambda16 :: C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_FrontendTarget -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_callFrontendWithParams_dot___hash_lambda16 x1 x2 x3 x4 x5 x3250 x3500 = let
     x6 = Curry_Prelude.d_C_maybe Curry_Prelude.OP_List Curry_Prelude.d_C_id (d_C_logfile x1 x3250 x3500) x3250 x3500
     x7 = Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_OP_callFrontendWithParams_dot_showFrontendTarget_dot_120 x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt (d_OP__case_0 x6 x7 (Curry_Prelude.d_C_null x6 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return Curry_Prelude.OP_Unit x3250 x3500) x3250 x3500)

d_C_rcErr :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_rcErr x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_IO.d_C_hPutStrLn (Curry_IO.d_C_stderr x3250 x3500) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))))))))))) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return x2 x3250 x3500) x3250 x3500

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_OP__case_0 x6 x7 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_System.d_C_system x7 x3250 x3500
     Curry_Prelude.C_False -> Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x6 x7 x1002 x3250 x3500) (d_OP__case_0 x6 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x6 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x6 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_1 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x1002 x3250 x3500) (d_OP__case_1 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: C_FrontendParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_2 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)))))))))))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x2 x1002 x3250 x3500) (d_OP__case_2 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: C_FrontendParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_3 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x2 x1002 x3250 x3500) (d_OP__case_3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_4 x2 x36 x37 x3250 x3500 = case x37 of
     Curry_Prelude.C_True -> x36
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x2 x36 x1002 x3250 x3500) (d_OP__case_4 x2 x36 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x2 x36 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x2 x36 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_5 x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1002 x3250 x3500) (d_OP__case_5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_26 x2 x8 x3250 x3500 = case x8 of
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = x5
           in (d_OP__case_25 x7 x6 x2 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Char 'p'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x2 x1002 x3250 x3500) (d_OP__case_26 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_FrontendParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_25 x7 x6 x2 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_OP__case_24 x6 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_15 x7 x6 x2 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Char 'k'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x7 x6 x2 x1002 x3250 x3500) (d_OP__case_25 x7 x6 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x7 x6 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x7 x6 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_FrontendParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_15 x7 x6 x2 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_OP__case_14 x2 x6 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x7 x6 x2 x1002 x3250 x3500) (d_OP__case_15 x7 x6 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x7 x6 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x7 x6 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_14 x2 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x22 x23) -> let
          x24 = x22
           in (d_OP__case_13 x24 x23 x2 (Curry_Prelude.d_OP_eq_eq x24 (Curry_Prelude.C_Char 'i'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x2 x1002 x3250 x3500) (d_OP__case_14 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_FrontendParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_13 x24 x23 x2 x25 x3250 x3500 = case x25 of
     Curry_Prelude.C_True -> d_OP__case_12 x2 x23 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x24 x23 x2 x1002 x3250 x3500) (d_OP__case_13 x24 x23 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x24 x23 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x24 x23 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_12 x2 x23 x3250 x3500 = case x23 of
     (Curry_Prelude.OP_Cons x25 x26) -> let
          x27 = x25
           in (d_OP__case_11 x27 x26 x2 (Curry_Prelude.d_OP_eq_eq x27 (Curry_Prelude.C_Char 'c'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x2 x1002 x3250 x3500) (d_OP__case_12 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_FrontendParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_11 x27 x26 x2 x28 x3250 x3500 = case x28 of
     Curry_Prelude.C_True -> d_OP__case_10 x2 x26 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x27 x26 x2 x1002 x3250 x3500) (d_OP__case_11 x27 x26 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x27 x26 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x27 x26 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_10 x2 x26 x3250 x3500 = case x26 of
     (Curry_Prelude.OP_Cons x28 x29) -> let
          x30 = x28
           in (d_OP__case_9 x30 x29 x2 (Curry_Prelude.d_OP_eq_eq x30 (Curry_Prelude.C_Char 's'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x2 x1002 x3250 x3500) (d_OP__case_10 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_FrontendParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_9 x30 x29 x2 x31 x3250 x3500 = case x31 of
     Curry_Prelude.C_True -> d_OP__case_8 x2 x29 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x30 x29 x2 x1002 x3250 x3500) (d_OP__case_9 x30 x29 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x30 x29 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x30 x29 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_8 x2 x29 x3250 x3500 = case x29 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_maybe (d_C_getLoadPath x3250 x3500) Curry_Prelude.d_C_return (d_C_fullPath x2 x3250 x3500) x3250 x3500) d_OP_callFrontendWithParams_dot___hash_lambda12 x3250 x3500
     (Curry_Prelude.OP_Cons x31 x32) -> let
          x33 = x31
           in (d_OP__case_7 x33 x32 x2 (Curry_Prelude.d_OP_eq_eq x33 (Curry_Prelude.C_Char '2'#) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x2 x1002 x3250 x3500) (d_OP__case_8 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_FrontendParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_7 x33 x32 x2 x34 x3250 x3500 = case x34 of
     Curry_Prelude.C_True -> d_OP__case_6 x2 x32 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x33 x32 x2 x1002 x3250 x3500) (d_OP__case_7 x33 x32 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x33 x32 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x33 x32 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_6 x2 x32 x3250 x3500 = case x32 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_maybe (d_C_getLoadPath x3250 x3500) Curry_Prelude.d_C_return (d_C_fullPath x2 x3250 x3500) x3250 x3500) d_OP_callFrontendWithParams_dot___hash_lambda13 x3250 x3500
     (Curry_Prelude.OP_Cons x34 x35) -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x2 x1002 x3250 x3500) (d_OP__case_6 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_24 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x10 = x8
           in (d_OP__case_23 x10 x9 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char 'a'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x1002 x3250 x3500) (d_OP__case_24 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_23 x10 x9 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> d_OP__case_22 x9 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x10 x9 x1002 x3250 x3500) (d_OP__case_23 x10 x9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x10 x9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x10 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_22 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x11 x12) -> let
          x13 = x11
           in (d_OP__case_21 x13 x12 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char 'k'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1002 x3250 x3500) (d_OP__case_22 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_21 x13 x12 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> d_OP__case_20 x12 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x13 x12 x1002 x3250 x3500) (d_OP__case_21 x13 x12 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x13 x12 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x13 x12 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_20 x12 x3250 x3500 = case x12 of
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x16 = x14
           in (d_OP__case_19 x16 x15 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Char 'c'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1002 x3250 x3500) (d_OP__case_20 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_19 x16 x15 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> d_OP__case_18 x15 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x16 x15 x1002 x3250 x3500) (d_OP__case_19 x16 x15 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x16 x15 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x16 x15 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_18 x15 x3250 x3500 = case x15 of
     (Curry_Prelude.OP_Cons x17 x18) -> let
          x19 = x17
           in (d_OP__case_17 x19 x18 (Curry_Prelude.d_OP_eq_eq x19 (Curry_Prelude.C_Char 's'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x1002 x3250 x3500) (d_OP__case_18 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_17 x19 x18 x20 x3250 x3500 = case x20 of
     Curry_Prelude.C_True -> d_OP__case_16 x18 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x19 x18 x1002 x3250 x3500) (d_OP__case_17 x19 x18 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x19 x18 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x19 x18 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_16 x18 x3250 x3500 = case x18 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_C_installDir x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x20 x21) -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x1002 x3250 x3500) (d_OP__case_16 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.C_IO C_FrontendParams
d_OP__case_28 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_27 x4 x2 x5 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x2 x1002 x3250 x3500) (d_OP__case_28 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.C_IO C_FrontendParams
d_OP__case_27 x4 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_Prelude.d_OP_dollar (d_C_setExtended (Curry_Prelude.d_OP_slash_eq x2 (Curry_Prelude.C_Just (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List))) x3250 x3500)) (Curry_Prelude.d_OP_dollar (d_C_setOverlapWarn (Curry_Prelude.d_OP_slash_eq x4 (Curry_Prelude.C_Just (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List))) x3250 x3500)) (d_C_defaultParams x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x4 x2 x1002 x3250 x3500) (d_OP__case_27 x4 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x4 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x4 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_29 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_29 x5 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_FileGoodies.d_C_splitPath x5 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x5 x1002 x3250 x3500) (d_OP__case_29 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_30 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_30 x3 x4 x5 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_System.d_C_getEnviron (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'Y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) Curry_Prelude.OP_List))))))))) x3250 x3500) (d_OP_getLoadPathForFile_dot___hash_lambda6_dot___hash_lambda7_dot___hash_lambda8 x1 x5 x4 x3) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x3 x4 x5 x1 x1002 x3250 x3500) (d_OP__case_30 x3 x4 x5 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x3 x4 x5 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x3 x4 x5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_30 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_30 x3 x4 x5 x1 x6 x3000 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_System.d_C_getEnviron (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'Y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) Curry_Prelude.OP_List))))))))) x3250 x3500) (wrapNX id (nd_OP_getLoadPathForFile_dot___hash_lambda6_dot___hash_lambda7_dot___hash_lambda8 x1 x5 x4 x3)) x2000 x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_30 x3 x4 x5 x1 x1002 x3000 x3250 x3500) (nd_OP__case_30 x3 x4 x5 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_30 x3 x4 x5 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_30 x3 x4 x5 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_31 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_31 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_C_installDir x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List))) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_C_installDir x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List))) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> Curry_FileGoodies.d_C_splitPath x1 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x1 x1002 x3250 x3500) (d_OP__case_31 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_52 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_52 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x1 x2) -> let
          x3 = x1
           in (d_OP__case_51 x3 x2 (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Char 'p'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_52 x1002 x3250 x3500) (d_OP__case_52 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_52 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_52 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_51 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_51 x3 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> d_OP__case_50 x2 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_41 x3 x2 (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Char 'k'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_51 x3 x2 x1002 x3250 x3500) (d_OP__case_51 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_51 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_51 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_41 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_41 x3 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> d_OP__case_40 x2 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x3 x2 x1002 x3250 x3500) (d_OP__case_41 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_40 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_40 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x18 x19) -> let
          x20 = x18
           in (d_OP__case_39 x20 x19 (Curry_Prelude.d_OP_eq_eq x20 (Curry_Prelude.C_Char 'i'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x1002 x3250 x3500) (d_OP__case_40 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_39 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_39 x20 x19 x21 x3250 x3500 = case x21 of
     Curry_Prelude.C_True -> d_OP__case_38 x19 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x20 x19 x1002 x3250 x3500) (d_OP__case_39 x20 x19 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x20 x19 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x20 x19 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_38 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_38 x19 x3250 x3500 = case x19 of
     (Curry_Prelude.OP_Cons x21 x22) -> let
          x23 = x21
           in (d_OP__case_37 x23 x22 (Curry_Prelude.d_OP_eq_eq x23 (Curry_Prelude.C_Char 'c'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x1002 x3250 x3500) (d_OP__case_38 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_37 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_37 x23 x22 x24 x3250 x3500 = case x24 of
     Curry_Prelude.C_True -> d_OP__case_36 x22 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x23 x22 x1002 x3250 x3500) (d_OP__case_37 x23 x22 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x23 x22 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x23 x22 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_36 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_36 x22 x3250 x3500 = case x22 of
     (Curry_Prelude.OP_Cons x24 x25) -> let
          x26 = x24
           in (d_OP__case_35 x26 x25 (Curry_Prelude.d_OP_eq_eq x26 (Curry_Prelude.C_Char 's'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x1002 x3250 x3500) (d_OP__case_36 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_35 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_35 x26 x25 x27 x3250 x3500 = case x27 of
     Curry_Prelude.C_True -> d_OP__case_34 x25 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x26 x25 x1002 x3250 x3500) (d_OP__case_35 x26 x25 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x26 x25 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x26 x25 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_34 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_34 x25 x3250 x3500 = case x25 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_C_installDir x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500) Curry_Prelude.OP_List) x3250 x3500
     (Curry_Prelude.OP_Cons x27 x28) -> let
          x29 = x27
           in (d_OP__case_33 x29 x28 (Curry_Prelude.d_OP_eq_eq x29 (Curry_Prelude.C_Char '2'#) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x1002 x3250 x3500) (d_OP__case_34 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_33 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_33 x29 x28 x30 x3250 x3500 = case x30 of
     Curry_Prelude.C_True -> d_OP__case_32 x28 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x29 x28 x1002 x3250 x3500) (d_OP__case_33 x29 x28 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x29 x28 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x29 x28 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_32 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_32 x28 x3250 x3500 = case x28 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_C_installDir x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List))) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_C_installDir x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List))) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) Curry_Prelude.OP_List)) x3250 x3500
     (Curry_Prelude.OP_Cons x30 x31) -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x1002 x3250 x3500) (d_OP__case_32 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_50 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_50 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = x4
           in (d_OP__case_49 x6 x5 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.C_Char 'a'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_50 x1002 x3250 x3500) (d_OP__case_50 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_50 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_50 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_49 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_49 x6 x5 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_OP__case_48 x5 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_49 x6 x5 x1002 x3250 x3500) (d_OP__case_49 x6 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_49 x6 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_49 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_48 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_48 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x7 x8) -> let
          x9 = x7
           in (d_OP__case_47 x9 x8 (Curry_Prelude.d_OP_eq_eq x9 (Curry_Prelude.C_Char 'k'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_48 x1002 x3250 x3500) (d_OP__case_48 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_48 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_48 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_47 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_47 x9 x8 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> d_OP__case_46 x8 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_47 x9 x8 x1002 x3250 x3500) (d_OP__case_47 x9 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_47 x9 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_47 x9 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_46 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_46 x8 x3250 x3500 = case x8 of
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x12 = x10
           in (d_OP__case_45 x12 x11 (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.C_Char 'c'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_46 x1002 x3250 x3500) (d_OP__case_46 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_46 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_46 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_45 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_45 x12 x11 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> d_OP__case_44 x11 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x12 x11 x1002 x3250 x3500) (d_OP__case_45 x12 x11 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 x12 x11 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x12 x11 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_44 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_44 x11 x3250 x3500 = case x11 of
     (Curry_Prelude.OP_Cons x13 x14) -> let
          x15 = x13
           in (d_OP__case_43 x15 x14 (Curry_Prelude.d_OP_eq_eq x15 (Curry_Prelude.C_Char 's'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x1002 x3250 x3500) (d_OP__case_44 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_43 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_43 x15 x14 x16 x3250 x3500 = case x16 of
     Curry_Prelude.C_True -> d_OP__case_42 x14 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x15 x14 x1002 x3250 x3500) (d_OP__case_43 x15 x14 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x15 x14 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x15 x14 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_42 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_42 x14 x3250 x3500 = case x14 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_gt_gt_eq (Curry_System.d_C_getEnviron (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'K'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) Curry_Prelude.OP_List)))))))))))) x3250 x3500) d_OP_getSysLibPath_dot___hash_lambda4 x3250 x3500
     (Curry_Prelude.OP_Cons x16 x17) -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x1002 x3250 x3500) (d_OP__case_42 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_curryCompiler :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_curryCompiler x3250 x3500 = external_d_C_curryCompiler x3250 x3500

d_C_curryCompilerMajorVersion :: Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_curryCompilerMajorVersion x3250 x3500 = external_d_C_curryCompilerMajorVersion x3250 x3500

d_C_curryCompilerMinorVersion :: Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_curryCompilerMinorVersion x3250 x3500 = external_d_C_curryCompilerMinorVersion x3250 x3500

d_C_curryRuntime :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_curryRuntime x3250 x3500 = external_d_C_curryRuntime x3250 x3500

d_C_curryRuntimeMajorVersion :: Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_curryRuntimeMajorVersion x3250 x3500 = external_d_C_curryRuntimeMajorVersion x3250 x3500

d_C_curryRuntimeMinorVersion :: Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_curryRuntimeMinorVersion x3250 x3500 = external_d_C_curryRuntimeMinorVersion x3250 x3500

d_C_installDir :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_installDir x3250 x3500 = external_d_C_installDir x3250 x3500
external_d_C_curryCompiler :: Cover -> ConstStore -> CP.C_String
external_d_C_curryCompiler _ _ = toCurry "kics2"

external_d_C_curryCompilerMajorVersion ::  Cover -> ConstStore -> CP.C_Int
external_d_C_curryCompilerMajorVersion _ _ = toCurry I.majorVersion

external_d_C_curryCompilerMinorVersion ::  Cover -> ConstStore -> CP.C_Int
external_d_C_curryCompilerMinorVersion _ _ = toCurry I.minorVersion

external_d_C_curryRuntime ::  Cover -> ConstStore -> CP.C_String
external_d_C_curryRuntime _ _ = toCurry I.runtime

external_d_C_curryRuntimeMajorVersion ::  Cover -> ConstStore -> CP.C_Int
external_d_C_curryRuntimeMajorVersion _ _ = toCurry I.runtimeMajor

external_d_C_curryRuntimeMinorVersion ::  Cover -> ConstStore -> CP.C_Int
external_d_C_curryRuntimeMinorVersion _ _ = toCurry I.runtimeMinor

external_d_C_installDir ::  Cover -> ConstStore -> CP.C_String
external_d_C_installDir _ _ = toCurry I.installDir

