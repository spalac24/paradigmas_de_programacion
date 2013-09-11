{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Linker (OP___hash_Rec_colon_ReplState (..), C_MainCompile (..), C_NonDetMode (..), C_ReplState, C_EvalMode, C_MoreDefault, d_OP___hash_selR_at_ReplState_dot_kics2Home, d_OP___hash_updR_at_ReplState_dot_kics2Home, d_OP___hash_selR_at_ReplState_dot_rcvars, d_OP___hash_updR_at_ReplState_dot_rcvars, d_OP___hash_selR_at_ReplState_dot_idSupply, d_OP___hash_updR_at_ReplState_dot_idSupply, d_OP___hash_selR_at_ReplState_dot_verbose, d_OP___hash_updR_at_ReplState_dot_verbose, d_OP___hash_selR_at_ReplState_dot_importPaths, d_OP___hash_updR_at_ReplState_dot_importPaths, d_OP___hash_selR_at_ReplState_dot_libPaths, d_OP___hash_updR_at_ReplState_dot_libPaths, d_OP___hash_selR_at_ReplState_dot_outputSubdir, d_OP___hash_updR_at_ReplState_dot_outputSubdir, d_OP___hash_selR_at_ReplState_dot_mainMod, d_OP___hash_updR_at_ReplState_dot_mainMod, d_OP___hash_selR_at_ReplState_dot_addMods, d_OP___hash_updR_at_ReplState_dot_addMods, d_OP___hash_selR_at_ReplState_dot_prompt, d_OP___hash_updR_at_ReplState_dot_prompt, d_OP___hash_selR_at_ReplState_dot_optim, d_OP___hash_updR_at_ReplState_dot_optim, d_OP___hash_selR_at_ReplState_dot_ndMode, d_OP___hash_updR_at_ReplState_dot_ndMode, d_OP___hash_selR_at_ReplState_dot_firstSol, d_OP___hash_updR_at_ReplState_dot_firstSol, d_OP___hash_selR_at_ReplState_dot_interactive, d_OP___hash_updR_at_ReplState_dot_interactive, d_OP___hash_selR_at_ReplState_dot_showBindings, d_OP___hash_updR_at_ReplState_dot_showBindings, d_OP___hash_selR_at_ReplState_dot_showTime, d_OP___hash_updR_at_ReplState_dot_showTime, d_OP___hash_selR_at_ReplState_dot_useGhci, d_OP___hash_updR_at_ReplState_dot_useGhci, d_OP___hash_selR_at_ReplState_dot_cmpOpts, d_OP___hash_updR_at_ReplState_dot_cmpOpts, d_OP___hash_selR_at_ReplState_dot_ghcOpts, d_OP___hash_updR_at_ReplState_dot_ghcOpts, d_OP___hash_selR_at_ReplState_dot_rtsOpts, d_OP___hash_updR_at_ReplState_dot_rtsOpts, d_OP___hash_selR_at_ReplState_dot_rtsArgs, d_OP___hash_updR_at_ReplState_dot_rtsArgs, d_OP___hash_selR_at_ReplState_dot_quit, d_OP___hash_updR_at_ReplState_dot_quit, d_OP___hash_selR_at_ReplState_dot_sourceguis, d_OP___hash_updR_at_ReplState_dot_sourceguis, d_OP___hash_selR_at_ReplState_dot_ghcicomm, d_OP___hash_updR_at_ReplState_dot_ghcicomm, d_C_initReplState, d_C_loadPaths, d_C_mainGoalFile, d_C_writeVerboseInfo, d_C_createAndCompileMain) where

import Basics
import qualified Curry_FilePath
import qualified Curry_GhciComm
import qualified Curry_IO
import qualified Curry_Installation
import qualified Curry_List
import qualified Curry_Names
import qualified Curry_Prelude
import qualified Curry_RCFile
import qualified Curry_ReadShowTerm
import qualified Curry_System
import qualified Curry_Utils
import qualified Curry_AbstractCurry
import qualified Curry_Directory
import qualified Curry_PropertyFile
data OP___hash_Rec_colon_ReplState
     = OP___hash_Lab_colon_kics2Home (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | OP___hash_Lab_colon_rcvars (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
     | OP___hash_Lab_colon_idSupply (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | OP___hash_Lab_colon_verbose Curry_Prelude.C_Int
     | OP___hash_Lab_colon_importPaths (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | OP___hash_Lab_colon_libPaths (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | OP___hash_Lab_colon_outputSubdir (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | OP___hash_Lab_colon_mainMod (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | OP___hash_Lab_colon_addMods (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | OP___hash_Lab_colon_prompt (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | OP___hash_Lab_colon_optim Curry_Prelude.C_Bool
     | OP___hash_Lab_colon_ndMode C_NonDetMode
     | OP___hash_Lab_colon_firstSol Curry_Prelude.C_Bool
     | OP___hash_Lab_colon_interactive Curry_Prelude.C_Bool
     | OP___hash_Lab_colon_showBindings Curry_Prelude.C_Bool
     | OP___hash_Lab_colon_showTime Curry_Prelude.C_Bool
     | OP___hash_Lab_colon_useGhci Curry_Prelude.C_Bool
     | OP___hash_Lab_colon_cmpOpts (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | OP___hash_Lab_colon_ghcOpts (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | OP___hash_Lab_colon_rtsOpts (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | OP___hash_Lab_colon_rtsArgs (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | OP___hash_Lab_colon_quit Curry_Prelude.C_Bool
     | OP___hash_Lab_colon_sourceguis (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle)))
     | OP___hash_Lab_colon_ghcicomm (Curry_Prelude.C_Maybe Curry_GhciComm.C_GhciComm)
     | Choice_OP___hash_Rec_colon_ReplState Cover ID OP___hash_Rec_colon_ReplState OP___hash_Rec_colon_ReplState
     | Choices_OP___hash_Rec_colon_ReplState Cover ID ([OP___hash_Rec_colon_ReplState])
     | Fail_OP___hash_Rec_colon_ReplState Cover FailInfo
     | Guard_OP___hash_Rec_colon_ReplState Cover Constraints OP___hash_Rec_colon_ReplState

instance Show OP___hash_Rec_colon_ReplState where
  showsPrec d (Choice_OP___hash_Rec_colon_ReplState cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP___hash_Rec_colon_ReplState cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP___hash_Rec_colon_ReplState cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP___hash_Rec_colon_ReplState cd info) = showChar '!'
  showsPrec _ (OP___hash_Lab_colon_kics2Home x1) = (showString "(OP___hash_Lab_colon_kics2Home") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_rcvars x1) = (showString "(OP___hash_Lab_colon_rcvars") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_idSupply x1) = (showString "(OP___hash_Lab_colon_idSupply") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_verbose x1) = (showString "(OP___hash_Lab_colon_verbose") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_importPaths x1) = (showString "(OP___hash_Lab_colon_importPaths") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_libPaths x1) = (showString "(OP___hash_Lab_colon_libPaths") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_outputSubdir x1) = (showString "(OP___hash_Lab_colon_outputSubdir") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_mainMod x1) = (showString "(OP___hash_Lab_colon_mainMod") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_addMods x1) = (showString "(OP___hash_Lab_colon_addMods") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_prompt x1) = (showString "(OP___hash_Lab_colon_prompt") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_optim x1) = (showString "(OP___hash_Lab_colon_optim") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_ndMode x1) = (showString "(OP___hash_Lab_colon_ndMode") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_firstSol x1) = (showString "(OP___hash_Lab_colon_firstSol") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_interactive x1) = (showString "(OP___hash_Lab_colon_interactive") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_showBindings x1) = (showString "(OP___hash_Lab_colon_showBindings") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_showTime x1) = (showString "(OP___hash_Lab_colon_showTime") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_useGhci x1) = (showString "(OP___hash_Lab_colon_useGhci") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_cmpOpts x1) = (showString "(OP___hash_Lab_colon_cmpOpts") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_ghcOpts x1) = (showString "(OP___hash_Lab_colon_ghcOpts") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_rtsOpts x1) = (showString "(OP___hash_Lab_colon_rtsOpts") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_rtsArgs x1) = (showString "(OP___hash_Lab_colon_rtsArgs") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_quit x1) = (showString "(OP___hash_Lab_colon_quit") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_sourceguis x1) = (showString "(OP___hash_Lab_colon_sourceguis") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (OP___hash_Lab_colon_ghcicomm x1) = (showString "(OP___hash_Lab_colon_ghcicomm") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read OP___hash_Rec_colon_ReplState where
  readsPrec d s = (readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_kics2Home x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_kics2Home" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_rcvars x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_rcvars" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_idSupply x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_idSupply" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_verbose x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_verbose" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_importPaths x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_importPaths" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_libPaths x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_libPaths" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_outputSubdir x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_outputSubdir" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_mainMod x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_mainMod" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_addMods x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_addMods" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_prompt x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_prompt" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_optim x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_optim" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_ndMode x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_ndMode" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_firstSol x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_firstSol" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_interactive x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_interactive" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_showBindings x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_showBindings" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_showTime x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_showTime" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_useGhci x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_useGhci" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_cmpOpts x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_cmpOpts" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_ghcOpts x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_ghcOpts" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_rtsOpts x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_rtsOpts" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_rtsArgs x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_rtsArgs" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_quit x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_quit" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_sourceguis x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_sourceguis" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (OP___hash_Lab_colon_ghcicomm x1,r1) | (_,r0) <- readQualified "Linker" "OP___hash_Lab_colon_ghcicomm" r, (x1,r1) <- readsPrec 11 r0]) s)))))))))))))))))))))))


instance NonDet OP___hash_Rec_colon_ReplState where
  choiceCons = Choice_OP___hash_Rec_colon_ReplState
  choicesCons = Choices_OP___hash_Rec_colon_ReplState
  failCons = Fail_OP___hash_Rec_colon_ReplState
  guardCons = Guard_OP___hash_Rec_colon_ReplState
  try (Choice_OP___hash_Rec_colon_ReplState cd i x y) = tryChoice cd i x y
  try (Choices_OP___hash_Rec_colon_ReplState cd i xs) = tryChoices cd i xs
  try (Fail_OP___hash_Rec_colon_ReplState cd info) = Fail cd info
  try (Guard_OP___hash_Rec_colon_ReplState cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP___hash_Rec_colon_ReplState cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP___hash_Rec_colon_ReplState cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP___hash_Rec_colon_ReplState cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP___hash_Rec_colon_ReplState cd i _) = error ("Linker.OP___hash_Rec_colon_ReplState.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP___hash_Rec_colon_ReplState cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP___hash_Rec_colon_ReplState cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable OP___hash_Rec_colon_ReplState where
  generate s c = Choices_OP___hash_Rec_colon_ReplState c (freeID [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] s) [(OP___hash_Lab_colon_kics2Home (generate (leftSupply s) c)),(OP___hash_Lab_colon_rcvars (generate (leftSupply s) c)),(OP___hash_Lab_colon_idSupply (generate (leftSupply s) c)),(OP___hash_Lab_colon_verbose (generate (leftSupply s) c)),(OP___hash_Lab_colon_importPaths (generate (leftSupply s) c)),(OP___hash_Lab_colon_libPaths (generate (leftSupply s) c)),(OP___hash_Lab_colon_outputSubdir (generate (leftSupply s) c)),(OP___hash_Lab_colon_mainMod (generate (leftSupply s) c)),(OP___hash_Lab_colon_addMods (generate (leftSupply s) c)),(OP___hash_Lab_colon_prompt (generate (leftSupply s) c)),(OP___hash_Lab_colon_optim (generate (leftSupply s) c)),(OP___hash_Lab_colon_ndMode (generate (leftSupply s) c)),(OP___hash_Lab_colon_firstSol (generate (leftSupply s) c)),(OP___hash_Lab_colon_interactive (generate (leftSupply s) c)),(OP___hash_Lab_colon_showBindings (generate (leftSupply s) c)),(OP___hash_Lab_colon_showTime (generate (leftSupply s) c)),(OP___hash_Lab_colon_useGhci (generate (leftSupply s) c)),(OP___hash_Lab_colon_cmpOpts (generate (leftSupply s) c)),(OP___hash_Lab_colon_ghcOpts (generate (leftSupply s) c)),(OP___hash_Lab_colon_rtsOpts (generate (leftSupply s) c)),(OP___hash_Lab_colon_rtsArgs (generate (leftSupply s) c)),(OP___hash_Lab_colon_quit (generate (leftSupply s) c)),(OP___hash_Lab_colon_sourceguis (generate (leftSupply s) c)),(OP___hash_Lab_colon_ghcicomm (generate (leftSupply s) c))]


instance NormalForm OP___hash_Rec_colon_ReplState where
  ($!!) cont (OP___hash_Lab_colon_kics2Home x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_kics2Home y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_rcvars x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_rcvars y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_idSupply x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_idSupply y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_verbose x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_verbose y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_importPaths x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_importPaths y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_libPaths x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_libPaths y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_outputSubdir x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_outputSubdir y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_mainMod x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_mainMod y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_addMods x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_addMods y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_prompt x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_prompt y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_optim x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_optim y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_ndMode x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_ndMode y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_firstSol x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_firstSol y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_interactive x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_interactive y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_showBindings x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_showBindings y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_showTime x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_showTime y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_useGhci x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_useGhci y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_cmpOpts x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_cmpOpts y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_ghcOpts x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_ghcOpts y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_rtsOpts x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_rtsOpts y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_rtsArgs x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_rtsArgs y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_quit x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_quit y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_sourceguis x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_sourceguis y1) d cs) $!! x1) d) cs
  ($!!) cont (OP___hash_Lab_colon_ghcicomm x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_ghcicomm y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_OP___hash_Rec_colon_ReplState cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP___hash_Rec_colon_ReplState cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP___hash_Rec_colon_ReplState cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP___hash_Rec_colon_ReplState cd info) _ _ = failCons cd info
  ($##) cont (OP___hash_Lab_colon_kics2Home x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_kics2Home y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_rcvars x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_rcvars y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_idSupply x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_idSupply y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_verbose x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_verbose y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_importPaths x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_importPaths y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_libPaths x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_libPaths y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_outputSubdir x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_outputSubdir y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_mainMod x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_mainMod y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_addMods x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_addMods y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_prompt x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_prompt y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_optim x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_optim y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_ndMode x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_ndMode y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_firstSol x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_firstSol y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_interactive x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_interactive y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_showBindings x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_showBindings y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_showTime x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_showTime y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_useGhci x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_useGhci y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_cmpOpts x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_cmpOpts y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_ghcOpts x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_ghcOpts y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_rtsOpts x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_rtsOpts y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_rtsArgs x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_rtsArgs y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_quit x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_quit y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_sourceguis x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_sourceguis y1) d cs) $## x1) d) cs
  ($##) cont (OP___hash_Lab_colon_ghcicomm x1) d cs = (((\y1 d cs -> cont (OP___hash_Lab_colon_ghcicomm y1) d cs) $## x1) d) cs
  ($##) cont (Choice_OP___hash_Rec_colon_ReplState cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP___hash_Rec_colon_ReplState cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP___hash_Rec_colon_ReplState cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP___hash_Rec_colon_ReplState cd info) _ _ = failCons cd info
  searchNF search cont (OP___hash_Lab_colon_kics2Home x1) = search (\y1 -> cont (OP___hash_Lab_colon_kics2Home y1)) x1
  searchNF search cont (OP___hash_Lab_colon_rcvars x1) = search (\y1 -> cont (OP___hash_Lab_colon_rcvars y1)) x1
  searchNF search cont (OP___hash_Lab_colon_idSupply x1) = search (\y1 -> cont (OP___hash_Lab_colon_idSupply y1)) x1
  searchNF search cont (OP___hash_Lab_colon_verbose x1) = search (\y1 -> cont (OP___hash_Lab_colon_verbose y1)) x1
  searchNF search cont (OP___hash_Lab_colon_importPaths x1) = search (\y1 -> cont (OP___hash_Lab_colon_importPaths y1)) x1
  searchNF search cont (OP___hash_Lab_colon_libPaths x1) = search (\y1 -> cont (OP___hash_Lab_colon_libPaths y1)) x1
  searchNF search cont (OP___hash_Lab_colon_outputSubdir x1) = search (\y1 -> cont (OP___hash_Lab_colon_outputSubdir y1)) x1
  searchNF search cont (OP___hash_Lab_colon_mainMod x1) = search (\y1 -> cont (OP___hash_Lab_colon_mainMod y1)) x1
  searchNF search cont (OP___hash_Lab_colon_addMods x1) = search (\y1 -> cont (OP___hash_Lab_colon_addMods y1)) x1
  searchNF search cont (OP___hash_Lab_colon_prompt x1) = search (\y1 -> cont (OP___hash_Lab_colon_prompt y1)) x1
  searchNF search cont (OP___hash_Lab_colon_optim x1) = search (\y1 -> cont (OP___hash_Lab_colon_optim y1)) x1
  searchNF search cont (OP___hash_Lab_colon_ndMode x1) = search (\y1 -> cont (OP___hash_Lab_colon_ndMode y1)) x1
  searchNF search cont (OP___hash_Lab_colon_firstSol x1) = search (\y1 -> cont (OP___hash_Lab_colon_firstSol y1)) x1
  searchNF search cont (OP___hash_Lab_colon_interactive x1) = search (\y1 -> cont (OP___hash_Lab_colon_interactive y1)) x1
  searchNF search cont (OP___hash_Lab_colon_showBindings x1) = search (\y1 -> cont (OP___hash_Lab_colon_showBindings y1)) x1
  searchNF search cont (OP___hash_Lab_colon_showTime x1) = search (\y1 -> cont (OP___hash_Lab_colon_showTime y1)) x1
  searchNF search cont (OP___hash_Lab_colon_useGhci x1) = search (\y1 -> cont (OP___hash_Lab_colon_useGhci y1)) x1
  searchNF search cont (OP___hash_Lab_colon_cmpOpts x1) = search (\y1 -> cont (OP___hash_Lab_colon_cmpOpts y1)) x1
  searchNF search cont (OP___hash_Lab_colon_ghcOpts x1) = search (\y1 -> cont (OP___hash_Lab_colon_ghcOpts y1)) x1
  searchNF search cont (OP___hash_Lab_colon_rtsOpts x1) = search (\y1 -> cont (OP___hash_Lab_colon_rtsOpts y1)) x1
  searchNF search cont (OP___hash_Lab_colon_rtsArgs x1) = search (\y1 -> cont (OP___hash_Lab_colon_rtsArgs y1)) x1
  searchNF search cont (OP___hash_Lab_colon_quit x1) = search (\y1 -> cont (OP___hash_Lab_colon_quit y1)) x1
  searchNF search cont (OP___hash_Lab_colon_sourceguis x1) = search (\y1 -> cont (OP___hash_Lab_colon_sourceguis y1)) x1
  searchNF search cont (OP___hash_Lab_colon_ghcicomm x1) = search (\y1 -> cont (OP___hash_Lab_colon_ghcicomm y1)) x1
  searchNF _ _ x = error ("Linker.OP___hash_Rec_colon_ReplState.searchNF: no constructor: " ++ (show x))


instance Unifiable OP___hash_Rec_colon_ReplState where
  (=.=) (OP___hash_Lab_colon_kics2Home x1) (OP___hash_Lab_colon_kics2Home y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_rcvars x1) (OP___hash_Lab_colon_rcvars y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_idSupply x1) (OP___hash_Lab_colon_idSupply y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_verbose x1) (OP___hash_Lab_colon_verbose y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_importPaths x1) (OP___hash_Lab_colon_importPaths y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_libPaths x1) (OP___hash_Lab_colon_libPaths y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_outputSubdir x1) (OP___hash_Lab_colon_outputSubdir y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_mainMod x1) (OP___hash_Lab_colon_mainMod y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_addMods x1) (OP___hash_Lab_colon_addMods y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_prompt x1) (OP___hash_Lab_colon_prompt y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_optim x1) (OP___hash_Lab_colon_optim y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_ndMode x1) (OP___hash_Lab_colon_ndMode y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_firstSol x1) (OP___hash_Lab_colon_firstSol y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_interactive x1) (OP___hash_Lab_colon_interactive y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_showBindings x1) (OP___hash_Lab_colon_showBindings y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_showTime x1) (OP___hash_Lab_colon_showTime y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_useGhci x1) (OP___hash_Lab_colon_useGhci y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_cmpOpts x1) (OP___hash_Lab_colon_cmpOpts y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_ghcOpts x1) (OP___hash_Lab_colon_ghcOpts y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_rtsOpts x1) (OP___hash_Lab_colon_rtsOpts y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_rtsArgs x1) (OP___hash_Lab_colon_rtsArgs y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_quit x1) (OP___hash_Lab_colon_quit y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_sourceguis x1) (OP___hash_Lab_colon_sourceguis y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (OP___hash_Lab_colon_ghcicomm x1) (OP___hash_Lab_colon_ghcicomm y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP___hash_Lab_colon_kics2Home x1) (OP___hash_Lab_colon_kics2Home y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_rcvars x1) (OP___hash_Lab_colon_rcvars y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_idSupply x1) (OP___hash_Lab_colon_idSupply y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_verbose x1) (OP___hash_Lab_colon_verbose y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_importPaths x1) (OP___hash_Lab_colon_importPaths y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_libPaths x1) (OP___hash_Lab_colon_libPaths y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_outputSubdir x1) (OP___hash_Lab_colon_outputSubdir y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_mainMod x1) (OP___hash_Lab_colon_mainMod y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_addMods x1) (OP___hash_Lab_colon_addMods y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_prompt x1) (OP___hash_Lab_colon_prompt y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_optim x1) (OP___hash_Lab_colon_optim y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_ndMode x1) (OP___hash_Lab_colon_ndMode y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_firstSol x1) (OP___hash_Lab_colon_firstSol y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_interactive x1) (OP___hash_Lab_colon_interactive y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_showBindings x1) (OP___hash_Lab_colon_showBindings y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_showTime x1) (OP___hash_Lab_colon_showTime y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_useGhci x1) (OP___hash_Lab_colon_useGhci y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_cmpOpts x1) (OP___hash_Lab_colon_cmpOpts y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_ghcOpts x1) (OP___hash_Lab_colon_ghcOpts y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_rtsOpts x1) (OP___hash_Lab_colon_rtsOpts y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_rtsArgs x1) (OP___hash_Lab_colon_rtsArgs y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_quit x1) (OP___hash_Lab_colon_quit y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_sourceguis x1) (OP___hash_Lab_colon_sourceguis y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (OP___hash_Lab_colon_ghcicomm x1) (OP___hash_Lab_colon_ghcicomm y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP___hash_Lab_colon_kics2Home x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_rcvars x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_idSupply x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_verbose x3) = ((i :=: (ChooseN 3 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_importPaths x3) = ((i :=: (ChooseN 4 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_libPaths x3) = ((i :=: (ChooseN 5 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_outputSubdir x3) = ((i :=: (ChooseN 6 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_mainMod x3) = ((i :=: (ChooseN 7 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_addMods x3) = ((i :=: (ChooseN 8 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_prompt x3) = ((i :=: (ChooseN 9 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_optim x3) = ((i :=: (ChooseN 10 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_ndMode x3) = ((i :=: (ChooseN 11 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_firstSol x3) = ((i :=: (ChooseN 12 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_interactive x3) = ((i :=: (ChooseN 13 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_showBindings x3) = ((i :=: (ChooseN 14 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_showTime x3) = ((i :=: (ChooseN 15 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_useGhci x3) = ((i :=: (ChooseN 16 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_cmpOpts x3) = ((i :=: (ChooseN 17 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_ghcOpts x3) = ((i :=: (ChooseN 18 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_rtsOpts x3) = ((i :=: (ChooseN 19 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_rtsArgs x3) = ((i :=: (ChooseN 20 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_quit x3) = ((i :=: (ChooseN 21 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_sourceguis x3) = ((i :=: (ChooseN 22 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (OP___hash_Lab_colon_ghcicomm x3) = ((i :=: (ChooseN 23 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_OP___hash_Rec_colon_ReplState cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP___hash_Rec_colon_ReplState cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP___hash_Rec_colon_ReplState cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP___hash_Rec_colon_ReplState cd i _) = error ("Linker.OP___hash_Rec_colon_ReplState.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP___hash_Rec_colon_ReplState cd info) = [(Unsolvable info)]
  bind d i (Guard_OP___hash_Rec_colon_ReplState cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP___hash_Lab_colon_kics2Home x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_rcvars x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_idSupply x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_verbose x3) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_importPaths x3) = [(i :=: (ChooseN 4 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_libPaths x3) = [(i :=: (ChooseN 5 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_outputSubdir x3) = [(i :=: (ChooseN 6 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_mainMod x3) = [(i :=: (ChooseN 7 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_addMods x3) = [(i :=: (ChooseN 8 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_prompt x3) = [(i :=: (ChooseN 9 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_optim x3) = [(i :=: (ChooseN 10 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_ndMode x3) = [(i :=: (ChooseN 11 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_firstSol x3) = [(i :=: (ChooseN 12 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_interactive x3) = [(i :=: (ChooseN 13 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_showBindings x3) = [(i :=: (ChooseN 14 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_showTime x3) = [(i :=: (ChooseN 15 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_useGhci x3) = [(i :=: (ChooseN 16 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_cmpOpts x3) = [(i :=: (ChooseN 17 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_ghcOpts x3) = [(i :=: (ChooseN 18 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_rtsOpts x3) = [(i :=: (ChooseN 19 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_rtsArgs x3) = [(i :=: (ChooseN 20 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_quit x3) = [(i :=: (ChooseN 21 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_sourceguis x3) = [(i :=: (ChooseN 22 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (OP___hash_Lab_colon_ghcicomm x3) = [(i :=: (ChooseN 23 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_OP___hash_Rec_colon_ReplState cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP___hash_Rec_colon_ReplState cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP___hash_Rec_colon_ReplState cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP___hash_Rec_colon_ReplState cd i _) = error ("Linker.OP___hash_Rec_colon_ReplState.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP___hash_Rec_colon_ReplState cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP___hash_Rec_colon_ReplState cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry OP___hash_Rec_colon_ReplState where
  (=?=) (Choice_OP___hash_Rec_colon_ReplState cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_OP___hash_Rec_colon_ReplState cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_OP___hash_Rec_colon_ReplState cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_OP___hash_Rec_colon_ReplState cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP___hash_Rec_colon_ReplState cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_OP___hash_Rec_colon_ReplState cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_OP___hash_Rec_colon_ReplState cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP___hash_Rec_colon_ReplState cd info) _ _ = failCons cd info
  (=?=) (OP___hash_Lab_colon_kics2Home x1) (OP___hash_Lab_colon_kics2Home y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_rcvars x1) (OP___hash_Lab_colon_rcvars y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_idSupply x1) (OP___hash_Lab_colon_idSupply y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_verbose x1) (OP___hash_Lab_colon_verbose y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_importPaths x1) (OP___hash_Lab_colon_importPaths y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_libPaths x1) (OP___hash_Lab_colon_libPaths y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_outputSubdir x1) (OP___hash_Lab_colon_outputSubdir y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_mainMod x1) (OP___hash_Lab_colon_mainMod y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_addMods x1) (OP___hash_Lab_colon_addMods y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_prompt x1) (OP___hash_Lab_colon_prompt y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_optim x1) (OP___hash_Lab_colon_optim y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_ndMode x1) (OP___hash_Lab_colon_ndMode y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_firstSol x1) (OP___hash_Lab_colon_firstSol y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_interactive x1) (OP___hash_Lab_colon_interactive y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_showBindings x1) (OP___hash_Lab_colon_showBindings y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_showTime x1) (OP___hash_Lab_colon_showTime y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_useGhci x1) (OP___hash_Lab_colon_useGhci y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_cmpOpts x1) (OP___hash_Lab_colon_cmpOpts y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_ghcOpts x1) (OP___hash_Lab_colon_ghcOpts y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_rtsOpts x1) (OP___hash_Lab_colon_rtsOpts y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_rtsArgs x1) (OP___hash_Lab_colon_rtsArgs y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_quit x1) (OP___hash_Lab_colon_quit y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_sourceguis x1) (OP___hash_Lab_colon_sourceguis y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (OP___hash_Lab_colon_ghcicomm x1) (OP___hash_Lab_colon_ghcicomm y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_OP___hash_Rec_colon_ReplState cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_OP___hash_Rec_colon_ReplState cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_OP___hash_Rec_colon_ReplState cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_OP___hash_Rec_colon_ReplState cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP___hash_Rec_colon_ReplState cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_OP___hash_Rec_colon_ReplState cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_OP___hash_Rec_colon_ReplState cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP___hash_Rec_colon_ReplState cd info) _ _ = failCons cd info
  (<?=) (OP___hash_Lab_colon_kics2Home x1) (OP___hash_Lab_colon_kics2Home y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_rcvars _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_idSupply _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_verbose _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_importPaths _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_libPaths _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_outputSubdir _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_mainMod _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_addMods _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_prompt _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_optim _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_ndMode _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_firstSol _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_kics2Home _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars x1) (OP___hash_Lab_colon_rcvars y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_idSupply _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_verbose _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_importPaths _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_libPaths _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_outputSubdir _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_mainMod _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_addMods _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_prompt _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_optim _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_ndMode _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_firstSol _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rcvars _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply x1) (OP___hash_Lab_colon_idSupply y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_verbose _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_importPaths _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_libPaths _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_outputSubdir _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_mainMod _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_addMods _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_prompt _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_optim _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_ndMode _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_firstSol _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_idSupply _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose x1) (OP___hash_Lab_colon_verbose y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_importPaths _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_libPaths _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_outputSubdir _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_mainMod _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_addMods _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_prompt _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_optim _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_ndMode _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_firstSol _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_verbose _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths x1) (OP___hash_Lab_colon_importPaths y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_libPaths _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_outputSubdir _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_mainMod _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_addMods _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_prompt _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_optim _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_ndMode _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_firstSol _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_importPaths _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths x1) (OP___hash_Lab_colon_libPaths y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_outputSubdir _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_mainMod _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_addMods _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_prompt _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_optim _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_ndMode _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_firstSol _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_libPaths _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir x1) (OP___hash_Lab_colon_outputSubdir y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_mainMod _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_addMods _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_prompt _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_optim _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_ndMode _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_firstSol _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_outputSubdir _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod x1) (OP___hash_Lab_colon_mainMod y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_addMods _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_prompt _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_optim _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_ndMode _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_firstSol _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_mainMod _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods x1) (OP___hash_Lab_colon_addMods y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_prompt _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_optim _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_ndMode _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_firstSol _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_addMods _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt x1) (OP___hash_Lab_colon_prompt y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_optim _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_ndMode _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_firstSol _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_prompt _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optim x1) (OP___hash_Lab_colon_optim y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_optim _) (OP___hash_Lab_colon_ndMode _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optim _) (OP___hash_Lab_colon_firstSol _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optim _) (OP___hash_Lab_colon_interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optim _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optim _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optim _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optim _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optim _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optim _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optim _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optim _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optim _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_optim _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ndMode x1) (OP___hash_Lab_colon_ndMode y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_ndMode _) (OP___hash_Lab_colon_firstSol _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ndMode _) (OP___hash_Lab_colon_interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ndMode _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ndMode _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ndMode _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ndMode _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ndMode _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ndMode _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ndMode _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ndMode _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ndMode _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ndMode _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_firstSol x1) (OP___hash_Lab_colon_firstSol y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_firstSol _) (OP___hash_Lab_colon_interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_firstSol _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_firstSol _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_firstSol _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_firstSol _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_firstSol _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_firstSol _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_firstSol _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_firstSol _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_firstSol _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_firstSol _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_interactive x1) (OP___hash_Lab_colon_interactive y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_interactive _) (OP___hash_Lab_colon_showBindings _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_interactive _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_interactive _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_interactive _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_interactive _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_interactive _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_interactive _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_interactive _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_interactive _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_interactive _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showBindings x1) (OP___hash_Lab_colon_showBindings y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_showBindings _) (OP___hash_Lab_colon_showTime _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showBindings _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showBindings _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showBindings _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showBindings _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showBindings _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showBindings _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showBindings _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showBindings _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showTime x1) (OP___hash_Lab_colon_showTime y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_showTime _) (OP___hash_Lab_colon_useGhci _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showTime _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showTime _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showTime _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showTime _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showTime _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showTime _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_showTime _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_useGhci x1) (OP___hash_Lab_colon_useGhci y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_useGhci _) (OP___hash_Lab_colon_cmpOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_useGhci _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_useGhci _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_useGhci _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_useGhci _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_useGhci _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_useGhci _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_cmpOpts x1) (OP___hash_Lab_colon_cmpOpts y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_cmpOpts _) (OP___hash_Lab_colon_ghcOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_cmpOpts _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_cmpOpts _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_cmpOpts _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_cmpOpts _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_cmpOpts _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ghcOpts x1) (OP___hash_Lab_colon_ghcOpts y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_ghcOpts _) (OP___hash_Lab_colon_rtsOpts _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ghcOpts _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ghcOpts _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ghcOpts _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ghcOpts _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rtsOpts x1) (OP___hash_Lab_colon_rtsOpts y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_rtsOpts _) (OP___hash_Lab_colon_rtsArgs _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rtsOpts _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rtsOpts _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rtsOpts _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rtsArgs x1) (OP___hash_Lab_colon_rtsArgs y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_rtsArgs _) (OP___hash_Lab_colon_quit _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rtsArgs _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_rtsArgs _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_quit x1) (OP___hash_Lab_colon_quit y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_quit _) (OP___hash_Lab_colon_sourceguis _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_quit _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_sourceguis x1) (OP___hash_Lab_colon_sourceguis y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (OP___hash_Lab_colon_sourceguis _) (OP___hash_Lab_colon_ghcicomm _) _ _ = Curry_Prelude.C_True
  (<?=) (OP___hash_Lab_colon_ghcicomm x1) (OP___hash_Lab_colon_ghcicomm y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_ReplState
     = C_ReplState (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Int (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool C_NonDetMode Curry_Prelude.C_Bool Curry_Prelude.C_Bool Curry_Prelude.C_Bool Curry_Prelude.C_Bool Curry_Prelude.C_Bool (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle))) (Curry_Prelude.C_Maybe Curry_GhciComm.C_GhciComm)
     | Choice_C_ReplState Cover ID C_ReplState C_ReplState
     | Choices_C_ReplState Cover ID ([C_ReplState])
     | Fail_C_ReplState Cover FailInfo
     | Guard_C_ReplState Cover Constraints C_ReplState

instance Show C_ReplState where
  showsPrec d (Choice_C_ReplState cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ReplState cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ReplState cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ReplState cd info) = showChar '!'
  showsPrec _ (C_ReplState x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24) = (showString "(ReplState") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . ((showChar ' ') . ((shows x6) . ((showChar ' ') . ((shows x7) . ((showChar ' ') . ((shows x8) . ((showChar ' ') . ((shows x9) . ((showChar ' ') . ((shows x10) . ((showChar ' ') . ((shows x11) . ((showChar ' ') . ((shows x12) . ((showChar ' ') . ((shows x13) . ((showChar ' ') . ((shows x14) . ((showChar ' ') . ((shows x15) . ((showChar ' ') . ((shows x16) . ((showChar ' ') . ((shows x17) . ((showChar ' ') . ((shows x18) . ((showChar ' ') . ((shows x19) . ((showChar ' ') . ((shows x20) . ((showChar ' ') . ((shows x21) . ((showChar ' ') . ((shows x22) . ((showChar ' ') . ((shows x23) . ((showChar ' ') . ((shows x24) . (showChar ')')))))))))))))))))))))))))))))))))))))))))))))))))


instance Read C_ReplState where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_ReplState x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24,r24) | (_,r0) <- readQualified "Linker" "ReplState" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4, (x6,r6) <- readsPrec 11 r5, (x7,r7) <- readsPrec 11 r6, (x8,r8) <- readsPrec 11 r7, (x9,r9) <- readsPrec 11 r8, (x10,r10) <- readsPrec 11 r9, (x11,r11) <- readsPrec 11 r10, (x12,r12) <- readsPrec 11 r11, (x13,r13) <- readsPrec 11 r12, (x14,r14) <- readsPrec 11 r13, (x15,r15) <- readsPrec 11 r14, (x16,r16) <- readsPrec 11 r15, (x17,r17) <- readsPrec 11 r16, (x18,r18) <- readsPrec 11 r17, (x19,r19) <- readsPrec 11 r18, (x20,r20) <- readsPrec 11 r19, (x21,r21) <- readsPrec 11 r20, (x22,r22) <- readsPrec 11 r21, (x23,r23) <- readsPrec 11 r22, (x24,r24) <- readsPrec 11 r23]) s


instance NonDet C_ReplState where
  choiceCons = Choice_C_ReplState
  choicesCons = Choices_C_ReplState
  failCons = Fail_C_ReplState
  guardCons = Guard_C_ReplState
  try (Choice_C_ReplState cd i x y) = tryChoice cd i x y
  try (Choices_C_ReplState cd i xs) = tryChoices cd i xs
  try (Fail_C_ReplState cd info) = Fail cd info
  try (Guard_C_ReplState cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ReplState cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ReplState cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ReplState cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ReplState cd i _) = error ("Linker.ReplState.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ReplState cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ReplState cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_ReplState where
  generate s c = Choices_C_ReplState c (freeID [24] s) [(C_ReplState (generate (leftSupply (leftSupply (leftSupply (leftSupply (leftSupply s))))) c) (generate (rightSupply (leftSupply (leftSupply (leftSupply (leftSupply s))))) c) (generate (rightSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (leftSupply (leftSupply (rightSupply (leftSupply (leftSupply s))))) c) (generate (rightSupply (leftSupply (rightSupply (leftSupply (leftSupply s))))) c) (generate (rightSupply (rightSupply (leftSupply (leftSupply s)))) c) (generate (leftSupply (leftSupply (leftSupply (rightSupply (leftSupply s))))) c) (generate (rightSupply (leftSupply (leftSupply (rightSupply (leftSupply s))))) c) (generate (rightSupply (leftSupply (rightSupply (leftSupply s)))) c) (generate (leftSupply (leftSupply (rightSupply (rightSupply (leftSupply s))))) c) (generate (rightSupply (leftSupply (rightSupply (rightSupply (leftSupply s))))) c) (generate (rightSupply (rightSupply (rightSupply (leftSupply s)))) c) (generate (leftSupply (leftSupply (leftSupply (leftSupply (rightSupply s))))) c) (generate (rightSupply (leftSupply (leftSupply (leftSupply (rightSupply s))))) c) (generate (rightSupply (leftSupply (leftSupply (rightSupply s)))) c) (generate (leftSupply (leftSupply (rightSupply (leftSupply (rightSupply s))))) c) (generate (rightSupply (leftSupply (rightSupply (leftSupply (rightSupply s))))) c) (generate (rightSupply (rightSupply (leftSupply (rightSupply s)))) c) (generate (leftSupply (leftSupply (leftSupply (rightSupply (rightSupply s))))) c) (generate (rightSupply (leftSupply (leftSupply (rightSupply (rightSupply s))))) c) (generate (rightSupply (leftSupply (rightSupply (rightSupply s)))) c) (generate (leftSupply (leftSupply (rightSupply (rightSupply (rightSupply s))))) c) (generate (rightSupply (leftSupply (rightSupply (rightSupply (rightSupply s))))) c) (generate (rightSupply (rightSupply (rightSupply (rightSupply s)))) c))]


instance NormalForm C_ReplState where
  ($!!) cont (C_ReplState x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> (((\y11 d cs -> (((\y12 d cs -> (((\y13 d cs -> (((\y14 d cs -> (((\y15 d cs -> (((\y16 d cs -> (((\y17 d cs -> (((\y18 d cs -> (((\y19 d cs -> (((\y20 d cs -> (((\y21 d cs -> (((\y22 d cs -> (((\y23 d cs -> (((\y24 d cs -> cont (C_ReplState y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24) d cs) $!! x24) d) cs) $!! x23) d) cs) $!! x22) d) cs) $!! x21) d) cs) $!! x20) d) cs) $!! x19) d) cs) $!! x18) d) cs) $!! x17) d) cs) $!! x16) d) cs) $!! x15) d) cs) $!! x14) d) cs) $!! x13) d) cs) $!! x12) d) cs) $!! x11) d) cs) $!! x10) d) cs) $!! x9) d) cs) $!! x8) d) cs) $!! x7) d) cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_ReplState cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_ReplState cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_ReplState cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_ReplState cd info) _ _ = failCons cd info
  ($##) cont (C_ReplState x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> (((\y11 d cs -> (((\y12 d cs -> (((\y13 d cs -> (((\y14 d cs -> (((\y15 d cs -> (((\y16 d cs -> (((\y17 d cs -> (((\y18 d cs -> (((\y19 d cs -> (((\y20 d cs -> (((\y21 d cs -> (((\y22 d cs -> (((\y23 d cs -> (((\y24 d cs -> cont (C_ReplState y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24) d cs) $## x24) d) cs) $## x23) d) cs) $## x22) d) cs) $## x21) d) cs) $## x20) d) cs) $## x19) d) cs) $## x18) d) cs) $## x17) d) cs) $## x16) d) cs) $## x15) d) cs) $## x14) d) cs) $## x13) d) cs) $## x12) d) cs) $## x11) d) cs) $## x10) d) cs) $## x9) d) cs) $## x8) d) cs) $## x7) d) cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_ReplState cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_ReplState cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_ReplState cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_ReplState cd info) _ _ = failCons cd info
  searchNF search cont (C_ReplState x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> search (\y10 -> search (\y11 -> search (\y12 -> search (\y13 -> search (\y14 -> search (\y15 -> search (\y16 -> search (\y17 -> search (\y18 -> search (\y19 -> search (\y20 -> search (\y21 -> search (\y22 -> search (\y23 -> search (\y24 -> cont (C_ReplState y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24)) x24) x23) x22) x21) x20) x19) x18) x17) x16) x15) x14) x13) x12) x11) x10) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Linker.ReplState.searchNF: no constructor: " ++ (show x))


instance Unifiable C_ReplState where
  (=.=) (C_ReplState x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24) (C_ReplState y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & ((((((x6 =:= y6) d) cs) & ((((((x7 =:= y7) d) cs) & ((((((x8 =:= y8) d) cs) & ((((((x9 =:= y9) d) cs) & ((((((x10 =:= y10) d) cs) & ((((((x11 =:= y11) d) cs) & ((((((x12 =:= y12) d) cs) & ((((((x13 =:= y13) d) cs) & ((((((x14 =:= y14) d) cs) & ((((((x15 =:= y15) d) cs) & ((((((x16 =:= y16) d) cs) & ((((((x17 =:= y17) d) cs) & ((((((x18 =:= y18) d) cs) & ((((((x19 =:= y19) d) cs) & ((((((x20 =:= y20) d) cs) & ((((((x21 =:= y21) d) cs) & ((((((x22 =:= y22) d) cs) & ((((((x23 =:= y23) d) cs) & (((x24 =:= y24) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_ReplState x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24) (C_ReplState y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & ((((((x6 =:<= y6) d) cs) & ((((((x7 =:<= y7) d) cs) & ((((((x8 =:<= y8) d) cs) & ((((((x9 =:<= y9) d) cs) & ((((((x10 =:<= y10) d) cs) & ((((((x11 =:<= y11) d) cs) & ((((((x12 =:<= y12) d) cs) & ((((((x13 =:<= y13) d) cs) & ((((((x14 =:<= y14) d) cs) & ((((((x15 =:<= y15) d) cs) & ((((((x16 =:<= y16) d) cs) & ((((((x17 =:<= y17) d) cs) & ((((((x18 =:<= y18) d) cs) & ((((((x19 =:<= y19) d) cs) & ((((((x20 =:<= y20) d) cs) & ((((((x21 =:<= y21) d) cs) & ((((((x22 =:<= y22) d) cs) & ((((((x23 =:<= y23) d) cs) & (((x24 =:<= y24) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) = ((i :=: (ChooseN 0 24)):(concat [(bind cd (leftID (leftID (leftID (leftID (leftID i))))) x3),(bind cd (rightID (leftID (leftID (leftID (leftID i))))) x4),(bind cd (rightID (leftID (leftID (leftID i)))) x5),(bind cd (leftID (leftID (rightID (leftID (leftID i))))) x6),(bind cd (rightID (leftID (rightID (leftID (leftID i))))) x7),(bind cd (rightID (rightID (leftID (leftID i)))) x8),(bind cd (leftID (leftID (leftID (rightID (leftID i))))) x9),(bind cd (rightID (leftID (leftID (rightID (leftID i))))) x10),(bind cd (rightID (leftID (rightID (leftID i)))) x11),(bind cd (leftID (leftID (rightID (rightID (leftID i))))) x12),(bind cd (rightID (leftID (rightID (rightID (leftID i))))) x13),(bind cd (rightID (rightID (rightID (leftID i)))) x14),(bind cd (leftID (leftID (leftID (leftID (rightID i))))) x15),(bind cd (rightID (leftID (leftID (leftID (rightID i))))) x16),(bind cd (rightID (leftID (leftID (rightID i)))) x17),(bind cd (leftID (leftID (rightID (leftID (rightID i))))) x18),(bind cd (rightID (leftID (rightID (leftID (rightID i))))) x19),(bind cd (rightID (rightID (leftID (rightID i)))) x20),(bind cd (leftID (leftID (leftID (rightID (rightID i))))) x21),(bind cd (rightID (leftID (leftID (rightID (rightID i))))) x22),(bind cd (rightID (leftID (rightID (rightID i)))) x23),(bind cd (leftID (leftID (rightID (rightID (rightID i))))) x24),(bind cd (rightID (leftID (rightID (rightID (rightID i))))) x25),(bind cd (rightID (rightID (rightID (rightID i)))) x26)]))
  bind d i (Choice_C_ReplState cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_ReplState cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_ReplState cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_ReplState cd i _) = error ("Linker.ReplState.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_ReplState cd info) = [(Unsolvable info)]
  bind d i (Guard_C_ReplState cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) = [(i :=: (ChooseN 0 24)),((leftID (leftID (leftID (leftID (leftID i))))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (leftID (leftID i))))) x3))),((rightID (leftID (leftID (leftID (leftID i))))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (leftID (leftID i))))) x4))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (leftID i)))) x5))),((leftID (leftID (rightID (leftID (leftID i))))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID (leftID (leftID i))))) x6))),((rightID (leftID (rightID (leftID (leftID i))))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (leftID (leftID i))))) x7))),((rightID (rightID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID (leftID i)))) x8))),((leftID (leftID (leftID (rightID (leftID i))))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (rightID (leftID i))))) x9))),((rightID (leftID (leftID (rightID (leftID i))))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (rightID (leftID i))))) x10))),((rightID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (leftID i)))) x11))),((leftID (leftID (rightID (rightID (leftID i))))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID (rightID (leftID i))))) x12))),((rightID (leftID (rightID (rightID (leftID i))))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (rightID (leftID i))))) x13))),((rightID (rightID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (rightID (rightID (leftID i)))) x14))),((leftID (leftID (leftID (leftID (rightID i))))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (leftID (rightID i))))) x15))),((rightID (leftID (leftID (leftID (rightID i))))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (leftID (rightID i))))) x16))),((rightID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (rightID i)))) x17))),((leftID (leftID (rightID (leftID (rightID i))))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID (leftID (rightID i))))) x18))),((rightID (leftID (rightID (leftID (rightID i))))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (leftID (rightID i))))) x19))),((rightID (rightID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID (rightID i)))) x20))),((leftID (leftID (leftID (rightID (rightID i))))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (rightID (rightID i))))) x21))),((rightID (leftID (leftID (rightID (rightID i))))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (rightID (rightID i))))) x22))),((rightID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (rightID i)))) x23))),((leftID (leftID (rightID (rightID (rightID i))))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID (rightID (rightID i))))) x24))),((rightID (leftID (rightID (rightID (rightID i))))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (rightID (rightID i))))) x25))),((rightID (rightID (rightID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (rightID (rightID (rightID i)))) x26)))]
  lazyBind d i (Choice_C_ReplState cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_ReplState cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_ReplState cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_ReplState cd i _) = error ("Linker.ReplState.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_ReplState cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_ReplState cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_ReplState where
  (=?=) (Choice_C_ReplState cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_ReplState cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_ReplState cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_ReplState cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_ReplState cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_ReplState cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_ReplState cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_ReplState cd info) _ _ = failCons cd info
  (=?=) (C_ReplState x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24) (C_ReplState y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x6 Curry_Prelude.=?= y6) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x7 Curry_Prelude.=?= y7) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x8 Curry_Prelude.=?= y8) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x9 Curry_Prelude.=?= y9) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x10 Curry_Prelude.=?= y10) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x11 Curry_Prelude.=?= y11) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x12 Curry_Prelude.=?= y12) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x13 Curry_Prelude.=?= y13) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x14 Curry_Prelude.=?= y14) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x15 Curry_Prelude.=?= y15) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x16 Curry_Prelude.=?= y16) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x17 Curry_Prelude.=?= y17) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x18 Curry_Prelude.=?= y18) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x19 Curry_Prelude.=?= y19) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x20 Curry_Prelude.=?= y20) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x21 Curry_Prelude.=?= y21) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x22 Curry_Prelude.=?= y22) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x23 Curry_Prelude.=?= y23) d) cs) (((x24 Curry_Prelude.=?= y24) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_C_ReplState cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_ReplState cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_ReplState cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_ReplState cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_ReplState cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_ReplState cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_ReplState cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_ReplState cd info) _ _ = failCons cd info
  (<?=) (C_ReplState x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24) (C_ReplState y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x5 y5 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x6 y6 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x6 Curry_Prelude.=?= y6) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x7 y7 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x7 Curry_Prelude.=?= y7) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x8 y8 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x8 Curry_Prelude.=?= y8) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x9 y9 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x9 Curry_Prelude.=?= y9) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x10 y10 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x10 Curry_Prelude.=?= y10) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x11 y11 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x11 Curry_Prelude.=?= y11) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x12 y12 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x12 Curry_Prelude.=?= y12) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x13 y13 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x13 Curry_Prelude.=?= y13) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x14 y14 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x14 Curry_Prelude.=?= y14) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x15 y15 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x15 Curry_Prelude.=?= y15) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x16 y16 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x16 Curry_Prelude.=?= y16) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x17 y17 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x17 Curry_Prelude.=?= y17) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x18 y18 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x18 Curry_Prelude.=?= y18) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x19 y19 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x19 Curry_Prelude.=?= y19) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x20 y20 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x20 Curry_Prelude.=?= y20) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x21 y21 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x21 Curry_Prelude.=?= y21) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x22 y22 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x22 Curry_Prelude.=?= y22) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x23 y23 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x23 Curry_Prelude.=?= y23) d) cs) (((x24 Curry_Prelude.<?= y24) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data C_MainCompile
     = C_MainError
     | C_MainDet
     | C_MainNonDet
     | Choice_C_MainCompile Cover ID C_MainCompile C_MainCompile
     | Choices_C_MainCompile Cover ID ([C_MainCompile])
     | Fail_C_MainCompile Cover FailInfo
     | Guard_C_MainCompile Cover Constraints C_MainCompile

instance Show C_MainCompile where
  showsPrec d (Choice_C_MainCompile cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_MainCompile cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_MainCompile cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_MainCompile cd info) = showChar '!'
  showsPrec _ C_MainError = showString "MainError"
  showsPrec _ C_MainDet = showString "MainDet"
  showsPrec _ C_MainNonDet = showString "MainNonDet"


instance Read C_MainCompile where
  readsPrec _ s = (readParen False (\r -> [ (C_MainError,r0) | (_,r0) <- readQualified "Linker" "MainError" r]) s) ++ ((readParen False (\r -> [ (C_MainDet,r0) | (_,r0) <- readQualified "Linker" "MainDet" r]) s) ++ (readParen False (\r -> [ (C_MainNonDet,r0) | (_,r0) <- readQualified "Linker" "MainNonDet" r]) s))


instance NonDet C_MainCompile where
  choiceCons = Choice_C_MainCompile
  choicesCons = Choices_C_MainCompile
  failCons = Fail_C_MainCompile
  guardCons = Guard_C_MainCompile
  try (Choice_C_MainCompile cd i x y) = tryChoice cd i x y
  try (Choices_C_MainCompile cd i xs) = tryChoices cd i xs
  try (Fail_C_MainCompile cd info) = Fail cd info
  try (Guard_C_MainCompile cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_MainCompile cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_MainCompile cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_MainCompile cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_MainCompile cd i _) = error ("Linker.MainCompile.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_MainCompile cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_MainCompile cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_MainCompile where
  generate s c = Choices_C_MainCompile c (freeID [0,0,0] s) [C_MainError,C_MainDet,C_MainNonDet]


instance NormalForm C_MainCompile where
  ($!!) cont C_MainError d cs = cont C_MainError d cs
  ($!!) cont C_MainDet d cs = cont C_MainDet d cs
  ($!!) cont C_MainNonDet d cs = cont C_MainNonDet d cs
  ($!!) cont (Choice_C_MainCompile cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_MainCompile cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_MainCompile cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_MainCompile cd info) _ _ = failCons cd info
  ($##) cont C_MainError d cs = cont C_MainError d cs
  ($##) cont C_MainDet d cs = cont C_MainDet d cs
  ($##) cont C_MainNonDet d cs = cont C_MainNonDet d cs
  ($##) cont (Choice_C_MainCompile cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_MainCompile cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_MainCompile cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_MainCompile cd info) _ _ = failCons cd info
  searchNF _ cont C_MainError = cont C_MainError
  searchNF _ cont C_MainDet = cont C_MainDet
  searchNF _ cont C_MainNonDet = cont C_MainNonDet
  searchNF _ _ x = error ("Linker.MainCompile.searchNF: no constructor: " ++ (show x))


instance Unifiable C_MainCompile where
  (=.=) C_MainError C_MainError d cs = C_Success
  (=.=) C_MainDet C_MainDet d cs = C_Success
  (=.=) C_MainNonDet C_MainNonDet d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_MainError C_MainError d cs = C_Success
  (=.<=) C_MainDet C_MainDet d cs = C_Success
  (=.<=) C_MainNonDet C_MainNonDet d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_MainError = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_MainDet = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_MainNonDet = ((i :=: (ChooseN 2 0)):(concat []))
  bind d i (Choice_C_MainCompile cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_MainCompile cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_MainCompile cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_MainCompile cd i _) = error ("Linker.MainCompile.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_MainCompile cd info) = [(Unsolvable info)]
  bind d i (Guard_C_MainCompile cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_MainError = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_MainDet = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_MainNonDet = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_MainCompile cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_MainCompile cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_MainCompile cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_MainCompile cd i _) = error ("Linker.MainCompile.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_MainCompile cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_MainCompile cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_MainCompile where
  (=?=) (Choice_C_MainCompile cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_MainCompile cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_MainCompile cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_MainCompile cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_MainCompile cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_MainCompile cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_MainCompile cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_MainCompile cd info) _ _ = failCons cd info
  (=?=) C_MainError C_MainError d cs = Curry_Prelude.C_True
  (=?=) C_MainDet C_MainDet d cs = Curry_Prelude.C_True
  (=?=) C_MainNonDet C_MainNonDet d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_MainCompile cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_MainCompile cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_MainCompile cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_MainCompile cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_MainCompile cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_MainCompile cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_MainCompile cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_MainCompile cd info) _ _ = failCons cd info
  (<?=) C_MainError C_MainError d cs = Curry_Prelude.C_True
  (<?=) C_MainError C_MainDet _ _ = Curry_Prelude.C_True
  (<?=) C_MainError C_MainNonDet _ _ = Curry_Prelude.C_True
  (<?=) C_MainDet C_MainDet d cs = Curry_Prelude.C_True
  (<?=) C_MainDet C_MainNonDet _ _ = Curry_Prelude.C_True
  (<?=) C_MainNonDet C_MainNonDet d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_NonDetMode
     = C_DFS
     | C_BFS
     | C_IDS Curry_Prelude.C_Int
     | C_Par Curry_Prelude.C_Int
     | C_PrDFS
     | C_PrtChoices Curry_Prelude.C_Int
     | Choice_C_NonDetMode Cover ID C_NonDetMode C_NonDetMode
     | Choices_C_NonDetMode Cover ID ([C_NonDetMode])
     | Fail_C_NonDetMode Cover FailInfo
     | Guard_C_NonDetMode Cover Constraints C_NonDetMode

instance Show C_NonDetMode where
  showsPrec d (Choice_C_NonDetMode cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_NonDetMode cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_NonDetMode cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_NonDetMode cd info) = showChar '!'
  showsPrec _ C_DFS = showString "DFS"
  showsPrec _ C_BFS = showString "BFS"
  showsPrec _ (C_IDS x1) = (showString "(IDS") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Par x1) = (showString "(Par") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ C_PrDFS = showString "PrDFS"
  showsPrec _ (C_PrtChoices x1) = (showString "(PrtChoices") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_NonDetMode where
  readsPrec d s = (readParen False (\r -> [ (C_DFS,r0) | (_,r0) <- readQualified "Linker" "DFS" r]) s) ++ ((readParen False (\r -> [ (C_BFS,r0) | (_,r0) <- readQualified "Linker" "BFS" r]) s) ++ ((readParen (d > 10) (\r -> [ (C_IDS x1,r1) | (_,r0) <- readQualified "Linker" "IDS" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Par x1,r1) | (_,r0) <- readQualified "Linker" "Par" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen False (\r -> [ (C_PrDFS,r0) | (_,r0) <- readQualified "Linker" "PrDFS" r]) s) ++ (readParen (d > 10) (\r -> [ (C_PrtChoices x1,r1) | (_,r0) <- readQualified "Linker" "PrtChoices" r, (x1,r1) <- readsPrec 11 r0]) s)))))


instance NonDet C_NonDetMode where
  choiceCons = Choice_C_NonDetMode
  choicesCons = Choices_C_NonDetMode
  failCons = Fail_C_NonDetMode
  guardCons = Guard_C_NonDetMode
  try (Choice_C_NonDetMode cd i x y) = tryChoice cd i x y
  try (Choices_C_NonDetMode cd i xs) = tryChoices cd i xs
  try (Fail_C_NonDetMode cd info) = Fail cd info
  try (Guard_C_NonDetMode cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_NonDetMode cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_NonDetMode cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_NonDetMode cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_NonDetMode cd i _) = error ("Linker.NonDetMode.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_NonDetMode cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_NonDetMode cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_NonDetMode where
  generate s c = Choices_C_NonDetMode c (freeID [0,0,1,1,0,1] s) [C_DFS,C_BFS,(C_IDS (generate (leftSupply s) c)),(C_Par (generate (leftSupply s) c)),C_PrDFS,(C_PrtChoices (generate (leftSupply s) c))]


instance NormalForm C_NonDetMode where
  ($!!) cont C_DFS d cs = cont C_DFS d cs
  ($!!) cont C_BFS d cs = cont C_BFS d cs
  ($!!) cont (C_IDS x1) d cs = (((\y1 d cs -> cont (C_IDS y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Par x1) d cs = (((\y1 d cs -> cont (C_Par y1) d cs) $!! x1) d) cs
  ($!!) cont C_PrDFS d cs = cont C_PrDFS d cs
  ($!!) cont (C_PrtChoices x1) d cs = (((\y1 d cs -> cont (C_PrtChoices y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_NonDetMode cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_NonDetMode cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_NonDetMode cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_NonDetMode cd info) _ _ = failCons cd info
  ($##) cont C_DFS d cs = cont C_DFS d cs
  ($##) cont C_BFS d cs = cont C_BFS d cs
  ($##) cont (C_IDS x1) d cs = (((\y1 d cs -> cont (C_IDS y1) d cs) $## x1) d) cs
  ($##) cont (C_Par x1) d cs = (((\y1 d cs -> cont (C_Par y1) d cs) $## x1) d) cs
  ($##) cont C_PrDFS d cs = cont C_PrDFS d cs
  ($##) cont (C_PrtChoices x1) d cs = (((\y1 d cs -> cont (C_PrtChoices y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_NonDetMode cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_NonDetMode cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_NonDetMode cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_NonDetMode cd info) _ _ = failCons cd info
  searchNF _ cont C_DFS = cont C_DFS
  searchNF _ cont C_BFS = cont C_BFS
  searchNF search cont (C_IDS x1) = search (\y1 -> cont (C_IDS y1)) x1
  searchNF search cont (C_Par x1) = search (\y1 -> cont (C_Par y1)) x1
  searchNF _ cont C_PrDFS = cont C_PrDFS
  searchNF search cont (C_PrtChoices x1) = search (\y1 -> cont (C_PrtChoices y1)) x1
  searchNF _ _ x = error ("Linker.NonDetMode.searchNF: no constructor: " ++ (show x))


instance Unifiable C_NonDetMode where
  (=.=) C_DFS C_DFS d cs = C_Success
  (=.=) C_BFS C_BFS d cs = C_Success
  (=.=) (C_IDS x1) (C_IDS y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Par x1) (C_Par y1) d cs = ((x1 =:= y1) d) cs
  (=.=) C_PrDFS C_PrDFS d cs = C_Success
  (=.=) (C_PrtChoices x1) (C_PrtChoices y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_DFS C_DFS d cs = C_Success
  (=.<=) C_BFS C_BFS d cs = C_Success
  (=.<=) (C_IDS x1) (C_IDS y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Par x1) (C_Par y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) C_PrDFS C_PrDFS d cs = C_Success
  (=.<=) (C_PrtChoices x1) (C_PrtChoices y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_DFS = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_BFS = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i (C_IDS x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Par x3) = ((i :=: (ChooseN 3 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i C_PrDFS = ((i :=: (ChooseN 4 0)):(concat []))
  bind cd i (C_PrtChoices x3) = ((i :=: (ChooseN 5 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_NonDetMode cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_NonDetMode cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_NonDetMode cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_NonDetMode cd i _) = error ("Linker.NonDetMode.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_NonDetMode cd info) = [(Unsolvable info)]
  bind d i (Guard_C_NonDetMode cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_DFS = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_BFS = [(i :=: (ChooseN 1 0))]
  lazyBind cd i (C_IDS x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Par x3) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i C_PrDFS = [(i :=: (ChooseN 4 0))]
  lazyBind cd i (C_PrtChoices x3) = [(i :=: (ChooseN 5 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_NonDetMode cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_NonDetMode cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_NonDetMode cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_NonDetMode cd i _) = error ("Linker.NonDetMode.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_NonDetMode cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_NonDetMode cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_NonDetMode where
  (=?=) (Choice_C_NonDetMode cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_NonDetMode cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_NonDetMode cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_NonDetMode cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_NonDetMode cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_NonDetMode cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_NonDetMode cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_NonDetMode cd info) _ _ = failCons cd info
  (=?=) C_DFS C_DFS d cs = Curry_Prelude.C_True
  (=?=) C_BFS C_BFS d cs = Curry_Prelude.C_True
  (=?=) (C_IDS x1) (C_IDS y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Par x1) (C_Par y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) C_PrDFS C_PrDFS d cs = Curry_Prelude.C_True
  (=?=) (C_PrtChoices x1) (C_PrtChoices y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_NonDetMode cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_NonDetMode cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_NonDetMode cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_NonDetMode cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_NonDetMode cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_NonDetMode cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_NonDetMode cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_NonDetMode cd info) _ _ = failCons cd info
  (<?=) C_DFS C_DFS d cs = Curry_Prelude.C_True
  (<?=) C_DFS C_BFS _ _ = Curry_Prelude.C_True
  (<?=) C_DFS (C_IDS _) _ _ = Curry_Prelude.C_True
  (<?=) C_DFS (C_Par _) _ _ = Curry_Prelude.C_True
  (<?=) C_DFS C_PrDFS _ _ = Curry_Prelude.C_True
  (<?=) C_DFS (C_PrtChoices _) _ _ = Curry_Prelude.C_True
  (<?=) C_BFS C_BFS d cs = Curry_Prelude.C_True
  (<?=) C_BFS (C_IDS _) _ _ = Curry_Prelude.C_True
  (<?=) C_BFS (C_Par _) _ _ = Curry_Prelude.C_True
  (<?=) C_BFS C_PrDFS _ _ = Curry_Prelude.C_True
  (<?=) C_BFS (C_PrtChoices _) _ _ = Curry_Prelude.C_True
  (<?=) (C_IDS x1) (C_IDS y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_IDS _) (C_Par _) _ _ = Curry_Prelude.C_True
  (<?=) (C_IDS _) C_PrDFS _ _ = Curry_Prelude.C_True
  (<?=) (C_IDS _) (C_PrtChoices _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Par x1) (C_Par y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Par _) C_PrDFS _ _ = Curry_Prelude.C_True
  (<?=) (C_Par _) (C_PrtChoices _) _ _ = Curry_Prelude.C_True
  (<?=) C_PrDFS C_PrDFS d cs = Curry_Prelude.C_True
  (<?=) C_PrDFS (C_PrtChoices _) _ _ = Curry_Prelude.C_True
  (<?=) (C_PrtChoices x1) (C_PrtChoices y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_EvalMode
     = C_All
     | C_One
     | C_Interactive C_MoreDefault
     | Choice_C_EvalMode Cover ID C_EvalMode C_EvalMode
     | Choices_C_EvalMode Cover ID ([C_EvalMode])
     | Fail_C_EvalMode Cover FailInfo
     | Guard_C_EvalMode Cover Constraints C_EvalMode

instance Show C_EvalMode where
  showsPrec d (Choice_C_EvalMode cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_EvalMode cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_EvalMode cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_EvalMode cd info) = showChar '!'
  showsPrec _ C_All = showString "All"
  showsPrec _ C_One = showString "One"
  showsPrec _ (C_Interactive x1) = (showString "(Interactive") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_EvalMode where
  readsPrec d s = (readParen False (\r -> [ (C_All,r0) | (_,r0) <- readQualified "Linker" "All" r]) s) ++ ((readParen False (\r -> [ (C_One,r0) | (_,r0) <- readQualified "Linker" "One" r]) s) ++ (readParen (d > 10) (\r -> [ (C_Interactive x1,r1) | (_,r0) <- readQualified "Linker" "Interactive" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet C_EvalMode where
  choiceCons = Choice_C_EvalMode
  choicesCons = Choices_C_EvalMode
  failCons = Fail_C_EvalMode
  guardCons = Guard_C_EvalMode
  try (Choice_C_EvalMode cd i x y) = tryChoice cd i x y
  try (Choices_C_EvalMode cd i xs) = tryChoices cd i xs
  try (Fail_C_EvalMode cd info) = Fail cd info
  try (Guard_C_EvalMode cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_EvalMode cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_EvalMode cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_EvalMode cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_EvalMode cd i _) = error ("Linker.EvalMode.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_EvalMode cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_EvalMode cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_EvalMode where
  generate s c = Choices_C_EvalMode c (freeID [0,0,1] s) [C_All,C_One,(C_Interactive (generate (leftSupply s) c))]


instance NormalForm C_EvalMode where
  ($!!) cont C_All d cs = cont C_All d cs
  ($!!) cont C_One d cs = cont C_One d cs
  ($!!) cont (C_Interactive x1) d cs = (((\y1 d cs -> cont (C_Interactive y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_EvalMode cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_EvalMode cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_EvalMode cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_EvalMode cd info) _ _ = failCons cd info
  ($##) cont C_All d cs = cont C_All d cs
  ($##) cont C_One d cs = cont C_One d cs
  ($##) cont (C_Interactive x1) d cs = (((\y1 d cs -> cont (C_Interactive y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_EvalMode cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_EvalMode cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_EvalMode cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_EvalMode cd info) _ _ = failCons cd info
  searchNF _ cont C_All = cont C_All
  searchNF _ cont C_One = cont C_One
  searchNF search cont (C_Interactive x1) = search (\y1 -> cont (C_Interactive y1)) x1
  searchNF _ _ x = error ("Linker.EvalMode.searchNF: no constructor: " ++ (show x))


instance Unifiable C_EvalMode where
  (=.=) C_All C_All d cs = C_Success
  (=.=) C_One C_One d cs = C_Success
  (=.=) (C_Interactive x1) (C_Interactive y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_All C_All d cs = C_Success
  (=.<=) C_One C_One d cs = C_Success
  (=.<=) (C_Interactive x1) (C_Interactive y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_All = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_One = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i (C_Interactive x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_EvalMode cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_EvalMode cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_EvalMode cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_EvalMode cd i _) = error ("Linker.EvalMode.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_EvalMode cd info) = [(Unsolvable info)]
  bind d i (Guard_C_EvalMode cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_All = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_One = [(i :=: (ChooseN 1 0))]
  lazyBind cd i (C_Interactive x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_EvalMode cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_EvalMode cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_EvalMode cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_EvalMode cd i _) = error ("Linker.EvalMode.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_EvalMode cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_EvalMode cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_EvalMode where
  (=?=) (Choice_C_EvalMode cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_EvalMode cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_EvalMode cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_EvalMode cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_EvalMode cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_EvalMode cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_EvalMode cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_EvalMode cd info) _ _ = failCons cd info
  (=?=) C_All C_All d cs = Curry_Prelude.C_True
  (=?=) C_One C_One d cs = Curry_Prelude.C_True
  (=?=) (C_Interactive x1) (C_Interactive y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_EvalMode cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_EvalMode cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_EvalMode cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_EvalMode cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_EvalMode cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_EvalMode cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_EvalMode cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_EvalMode cd info) _ _ = failCons cd info
  (<?=) C_All C_All d cs = Curry_Prelude.C_True
  (<?=) C_All C_One _ _ = Curry_Prelude.C_True
  (<?=) C_All (C_Interactive _) _ _ = Curry_Prelude.C_True
  (<?=) C_One C_One d cs = Curry_Prelude.C_True
  (<?=) C_One (C_Interactive _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Interactive x1) (C_Interactive y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_MoreDefault
     = C_MoreYes
     | C_MoreNo
     | C_MoreAll
     | Choice_C_MoreDefault Cover ID C_MoreDefault C_MoreDefault
     | Choices_C_MoreDefault Cover ID ([C_MoreDefault])
     | Fail_C_MoreDefault Cover FailInfo
     | Guard_C_MoreDefault Cover Constraints C_MoreDefault

instance Show C_MoreDefault where
  showsPrec d (Choice_C_MoreDefault cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_MoreDefault cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_MoreDefault cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_MoreDefault cd info) = showChar '!'
  showsPrec _ C_MoreYes = showString "MoreYes"
  showsPrec _ C_MoreNo = showString "MoreNo"
  showsPrec _ C_MoreAll = showString "MoreAll"


instance Read C_MoreDefault where
  readsPrec _ s = (readParen False (\r -> [ (C_MoreYes,r0) | (_,r0) <- readQualified "Linker" "MoreYes" r]) s) ++ ((readParen False (\r -> [ (C_MoreNo,r0) | (_,r0) <- readQualified "Linker" "MoreNo" r]) s) ++ (readParen False (\r -> [ (C_MoreAll,r0) | (_,r0) <- readQualified "Linker" "MoreAll" r]) s))


instance NonDet C_MoreDefault where
  choiceCons = Choice_C_MoreDefault
  choicesCons = Choices_C_MoreDefault
  failCons = Fail_C_MoreDefault
  guardCons = Guard_C_MoreDefault
  try (Choice_C_MoreDefault cd i x y) = tryChoice cd i x y
  try (Choices_C_MoreDefault cd i xs) = tryChoices cd i xs
  try (Fail_C_MoreDefault cd info) = Fail cd info
  try (Guard_C_MoreDefault cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_MoreDefault cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_MoreDefault cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_MoreDefault cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_MoreDefault cd i _) = error ("Linker.MoreDefault.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_MoreDefault cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_MoreDefault cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_MoreDefault where
  generate s c = Choices_C_MoreDefault c (freeID [0,0,0] s) [C_MoreYes,C_MoreNo,C_MoreAll]


instance NormalForm C_MoreDefault where
  ($!!) cont C_MoreYes d cs = cont C_MoreYes d cs
  ($!!) cont C_MoreNo d cs = cont C_MoreNo d cs
  ($!!) cont C_MoreAll d cs = cont C_MoreAll d cs
  ($!!) cont (Choice_C_MoreDefault cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_MoreDefault cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_MoreDefault cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_MoreDefault cd info) _ _ = failCons cd info
  ($##) cont C_MoreYes d cs = cont C_MoreYes d cs
  ($##) cont C_MoreNo d cs = cont C_MoreNo d cs
  ($##) cont C_MoreAll d cs = cont C_MoreAll d cs
  ($##) cont (Choice_C_MoreDefault cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_MoreDefault cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_MoreDefault cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_MoreDefault cd info) _ _ = failCons cd info
  searchNF _ cont C_MoreYes = cont C_MoreYes
  searchNF _ cont C_MoreNo = cont C_MoreNo
  searchNF _ cont C_MoreAll = cont C_MoreAll
  searchNF _ _ x = error ("Linker.MoreDefault.searchNF: no constructor: " ++ (show x))


instance Unifiable C_MoreDefault where
  (=.=) C_MoreYes C_MoreYes d cs = C_Success
  (=.=) C_MoreNo C_MoreNo d cs = C_Success
  (=.=) C_MoreAll C_MoreAll d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_MoreYes C_MoreYes d cs = C_Success
  (=.<=) C_MoreNo C_MoreNo d cs = C_Success
  (=.<=) C_MoreAll C_MoreAll d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_MoreYes = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_MoreNo = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_MoreAll = ((i :=: (ChooseN 2 0)):(concat []))
  bind d i (Choice_C_MoreDefault cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_MoreDefault cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_MoreDefault cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_MoreDefault cd i _) = error ("Linker.MoreDefault.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_MoreDefault cd info) = [(Unsolvable info)]
  bind d i (Guard_C_MoreDefault cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_MoreYes = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_MoreNo = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_MoreAll = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_MoreDefault cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_MoreDefault cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_MoreDefault cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_MoreDefault cd i _) = error ("Linker.MoreDefault.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_MoreDefault cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_MoreDefault cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_MoreDefault where
  (=?=) (Choice_C_MoreDefault cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_MoreDefault cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_MoreDefault cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_MoreDefault cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_MoreDefault cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_MoreDefault cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_MoreDefault cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_MoreDefault cd info) _ _ = failCons cd info
  (=?=) C_MoreYes C_MoreYes d cs = Curry_Prelude.C_True
  (=?=) C_MoreNo C_MoreNo d cs = Curry_Prelude.C_True
  (=?=) C_MoreAll C_MoreAll d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_MoreDefault cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_MoreDefault cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_MoreDefault cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_MoreDefault cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_MoreDefault cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_MoreDefault cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_MoreDefault cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_MoreDefault cd info) _ _ = failCons cd info
  (<?=) C_MoreYes C_MoreYes d cs = Curry_Prelude.C_True
  (<?=) C_MoreYes C_MoreNo _ _ = Curry_Prelude.C_True
  (<?=) C_MoreYes C_MoreAll _ _ = Curry_Prelude.C_True
  (<?=) C_MoreNo C_MoreNo d cs = Curry_Prelude.C_True
  (<?=) C_MoreNo C_MoreAll _ _ = Curry_Prelude.C_True
  (<?=) C_MoreAll C_MoreAll d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_OP___hash_selR_at_ReplState_dot_kics2Home :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP___hash_selR_at_ReplState_dot_kics2Home x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x2
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_kics2Home x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_kics2Home x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_kics2Home z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_kics2Home x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_kics2Home :: C_ReplState -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_kics2Home x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x2 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_kics2Home x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_kics2Home x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_kics2Home z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_kics2Home x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_rcvars :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP___hash_selR_at_ReplState_dot_rcvars x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x3
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_rcvars x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_rcvars x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_rcvars z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_rcvars x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_rcvars :: C_ReplState -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_rcvars x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x2 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_rcvars x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_rcvars x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_rcvars z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_rcvars x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_idSupply :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP___hash_selR_at_ReplState_dot_idSupply x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x4
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_idSupply x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_idSupply x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_idSupply z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_idSupply x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_idSupply :: C_ReplState -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_idSupply x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x2 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_idSupply x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_idSupply x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_idSupply z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_idSupply x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_verbose :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP___hash_selR_at_ReplState_dot_verbose x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x5
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_verbose x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_verbose x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_verbose z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_verbose x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_verbose :: C_ReplState -> Curry_Prelude.C_Int -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_verbose x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x2 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_verbose x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_verbose x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_verbose z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_verbose x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_importPaths :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP___hash_selR_at_ReplState_dot_importPaths x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x6
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_importPaths x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_importPaths x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_importPaths z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_importPaths x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_importPaths :: C_ReplState -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_importPaths x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x2 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_importPaths x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_importPaths x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_importPaths z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_importPaths x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_libPaths :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP___hash_selR_at_ReplState_dot_libPaths x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x7
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_libPaths x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_libPaths x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_libPaths z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_libPaths x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_libPaths :: C_ReplState -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_libPaths x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x2 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_libPaths x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_libPaths x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_libPaths z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_libPaths x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_outputSubdir :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP___hash_selR_at_ReplState_dot_outputSubdir x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x8
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_outputSubdir x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_outputSubdir x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_outputSubdir z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_outputSubdir x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_outputSubdir :: C_ReplState -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_outputSubdir x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x2 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_outputSubdir x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_outputSubdir x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_outputSubdir z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_outputSubdir x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_mainMod :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP___hash_selR_at_ReplState_dot_mainMod x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x9
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_mainMod x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_mainMod x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_mainMod z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_mainMod x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_mainMod :: C_ReplState -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_mainMod x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x2 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_mainMod x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_mainMod x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_mainMod z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_mainMod x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_addMods :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP___hash_selR_at_ReplState_dot_addMods x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x10
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_addMods x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_addMods x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_addMods z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_addMods x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_addMods :: C_ReplState -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_addMods x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x2 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_addMods x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_addMods x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_addMods z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_addMods x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_prompt :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP___hash_selR_at_ReplState_dot_prompt x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x11
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_prompt x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_prompt x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_prompt z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_prompt x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_prompt :: C_ReplState -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_prompt x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x2 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_prompt x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_prompt x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_prompt z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_prompt x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_optim :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP___hash_selR_at_ReplState_dot_optim x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x12
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_optim x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_optim x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_optim z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_optim x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_optim :: C_ReplState -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_optim x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x2 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_optim x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_optim x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_optim z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_optim x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_ndMode :: C_ReplState -> Cover -> ConstStore -> C_NonDetMode
d_OP___hash_selR_at_ReplState_dot_ndMode x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x13
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_ndMode x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_ndMode x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_ndMode z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_ndMode x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_ndMode :: C_ReplState -> C_NonDetMode -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_ndMode x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x2 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_ndMode x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_ndMode x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_ndMode z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_ndMode x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_firstSol :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP___hash_selR_at_ReplState_dot_firstSol x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x14
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_firstSol x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_firstSol x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_firstSol z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_firstSol x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_firstSol :: C_ReplState -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_firstSol x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x2 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_firstSol x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_firstSol x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_firstSol z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_firstSol x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_interactive :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP___hash_selR_at_ReplState_dot_interactive x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x15
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_interactive x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_interactive x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_interactive z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_interactive x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_interactive :: C_ReplState -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_interactive x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x2 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_interactive x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_interactive x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_interactive z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_interactive x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_showBindings :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP___hash_selR_at_ReplState_dot_showBindings x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x16
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_showBindings x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_showBindings x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_showBindings z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_showBindings x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_showBindings :: C_ReplState -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_showBindings x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x2 x18 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_showBindings x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_showBindings x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_showBindings z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_showBindings x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_showTime :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP___hash_selR_at_ReplState_dot_showTime x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x17
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_showTime x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_showTime x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_showTime z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_showTime x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_showTime :: C_ReplState -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_showTime x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x2 x19 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_showTime x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_showTime x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_showTime z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_showTime x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_useGhci :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP___hash_selR_at_ReplState_dot_useGhci x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x18
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_useGhci x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_useGhci x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_useGhci z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_useGhci x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_useGhci :: C_ReplState -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_useGhci x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x2 x20 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_useGhci x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_useGhci x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_useGhci z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_useGhci x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_cmpOpts :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP___hash_selR_at_ReplState_dot_cmpOpts x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x19
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_cmpOpts x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_cmpOpts x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_cmpOpts z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_cmpOpts x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_cmpOpts :: C_ReplState -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_cmpOpts x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x2 x21 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_cmpOpts x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_cmpOpts x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_cmpOpts z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_cmpOpts x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_ghcOpts :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP___hash_selR_at_ReplState_dot_ghcOpts x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x20
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_ghcOpts x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_ghcOpts x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_ghcOpts z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_ghcOpts x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_ghcOpts :: C_ReplState -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_ghcOpts x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x2 x22 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_ghcOpts x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_ghcOpts x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_ghcOpts z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_ghcOpts x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_rtsOpts :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP___hash_selR_at_ReplState_dot_rtsOpts x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x21
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_rtsOpts x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_rtsOpts x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_rtsOpts z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_rtsOpts x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_rtsOpts :: C_ReplState -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_rtsOpts x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x2 x23 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_rtsOpts x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_rtsOpts x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_rtsOpts z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_rtsOpts x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_rtsArgs :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP___hash_selR_at_ReplState_dot_rtsArgs x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x22
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_rtsArgs x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_rtsArgs x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_rtsArgs z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_rtsArgs x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_rtsArgs :: C_ReplState -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_rtsArgs x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x2 x24 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_rtsArgs x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_rtsArgs x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_rtsArgs z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_rtsArgs x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_quit :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP___hash_selR_at_ReplState_dot_quit x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x23
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_quit x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_quit x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_quit z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_quit x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_quit :: C_ReplState -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_quit x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x2 x25 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_quit x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_quit x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_quit z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_quit x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_sourceguis :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle))
d_OP___hash_selR_at_ReplState_dot_sourceguis x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x24
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_sourceguis x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_sourceguis x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_sourceguis z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_sourceguis x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_sourceguis :: C_ReplState -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle)) -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_sourceguis x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x2 x26
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_sourceguis x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_sourceguis x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_sourceguis z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_sourceguis x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_selR_at_ReplState_dot_ghcicomm :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.C_Maybe Curry_GhciComm.C_GhciComm
d_OP___hash_selR_at_ReplState_dot_ghcicomm x1 x3250 x3500 = case x1 of
     (C_ReplState x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25) -> x25
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_selR_at_ReplState_dot_ghcicomm x1002 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_ghcicomm x1003 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_selR_at_ReplState_dot_ghcicomm z x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_selR_at_ReplState_dot_ghcicomm x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___hash_updR_at_ReplState_dot_ghcicomm :: C_ReplState -> Curry_Prelude.C_Maybe Curry_GhciComm.C_GhciComm -> Cover -> ConstStore -> C_ReplState
d_OP___hash_updR_at_ReplState_dot_ghcicomm x1 x2 x3250 x3500 = case x1 of
     (C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26) -> C_ReplState x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x2
     (Choice_C_ReplState x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___hash_updR_at_ReplState_dot_ghcicomm x1002 x2 x3250 x3500) (d_OP___hash_updR_at_ReplState_dot_ghcicomm x1003 x2 x3250 x3500)
     (Choices_C_ReplState x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___hash_updR_at_ReplState_dot_ghcicomm z x2 x3250 x3500) x1002
     (Guard_C_ReplState x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___hash_updR_at_ReplState_dot_ghcicomm x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ReplState x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_initReplState :: Cover -> ConstStore -> C_ReplState
d_C_initReplState x3250 x3500 = C_ReplState Curry_Prelude.OP_List Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) Curry_Prelude.OP_List))))) (Curry_Prelude.C_Int 1#) Curry_Prelude.OP_List (Curry_Prelude.d_C_map (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Installation.d_C_installDir x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List))) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) Curry_Prelude.OP_List)))) x3250 x3500) Curry_Prelude.OP_List)) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) Curry_Prelude.OP_List))))) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '%'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) Curry_Prelude.C_True C_BFS Curry_Prelude.C_False Curry_Prelude.C_False Curry_Prelude.C_True Curry_Prelude.C_False Curry_Prelude.C_False Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.C_False Curry_Prelude.OP_List Curry_Prelude.C_Nothing

d_C_loadPaths :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_loadPaths x1 x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_OP___hash_selR_at_ReplState_dot_importPaths x1 x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_libPaths x1 x3250 x3500) x3250 x3500)

d_C_mainGoalFile :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_mainGoalFile x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))))))))))))))

d_C_writeVerboseInfo :: C_ReplState -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_writeVerboseInfo x1 x2 x3 x3250 x3500 = Curry_Utils.d_C_unless (Curry_Prelude.d_OP_lt (d_OP___hash_selR_at_ReplState_dot_verbose x1 x3250 x3500) x2 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn x3 x3250 x3500) (Curry_IO.d_C_hFlush (Curry_IO.d_C_stdout x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_C_readInfoFile :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool))
d_C_readInfoFile x1 x3250 x3500 = Curry_ReadShowTerm.d_C_readQTermFile (Curry_Prelude.d_C_apply (Curry_Names.d_C_funcInfoFile (d_OP___hash_selR_at_ReplState_dot_outputSubdir x1 x3250 x3500) x3250 x3500) (d_C_mainGoalFile x3250 x3500) x3250 x3500) x3250 x3500

d_C_getGoalInfo :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Bool Curry_Prelude.C_Bool)
d_C_getGoalInfo x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_readInfoFile x1 x3250 x3500) (d_OP_getGoalInfo_dot___hash_lambda1 x1) x3250 x3500

d_OP_getGoalInfo_dot___hash_lambda1 :: C_ReplState -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Bool Curry_Prelude.C_Bool)
d_OP_getGoalInfo_dot___hash_lambda1 x1 x2 x3250 x3500 = let
     x3 = Curry_Prelude.d_C_apply (Curry_Utils.d_C_notNull x3250 x3500) (Curry_Prelude.d_C_filter d_OP_getGoalInfo_dot___hash_lambda1_dot___hash_lambda2 x2 x3250 x3500) x3250 x3500
     x4 = Curry_Prelude.d_C_snd (Curry_Prelude.d_C_head (Curry_Prelude.d_C_filter (d_OP_getGoalInfo_dot___hash_lambda1_dot___hash_lambda3 x3) x2 x3250 x3500) x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (d_C_writeVerboseInfo x1 (Curry_Prelude.C_Int 3#)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))) (Curry_Prelude.d_OP_plus_plus (d_OP__case_46 x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (d_OP__case_45 x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x3 x4) x3250 x3500) x3250 x3500)

d_OP_getGoalInfo_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_getGoalInfo_dot___hash_lambda1_dot___hash_lambda2 x1 x3250 x3500 = Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_snd (Curry_Prelude.d_C_fst x1 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))))))))))))))) x3250 x3500

d_OP_getGoalInfo_dot___hash_lambda1_dot___hash_lambda3 :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_getGoalInfo_dot___hash_lambda1_dot___hash_lambda3 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_snd (Curry_Prelude.d_C_fst x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP__case_44 x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))))))))))))) x3250 x3500) x3250 x3500

d_C_updateGhcOptions :: C_ReplState -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_ReplState Curry_Prelude.C_Bool)
d_C_updateGhcOptions x1 x3250 x3500 = let
     x2 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))
     x3 = Curry_RCFile.d_C_rcValue (d_OP___hash_selR_at_ReplState_dot_rcvars x1 x3250 x3500) x2 x3250 x3500
     x4 = d_OP___hash_selR_at_ReplState_dot_ghcOpts x1 x3250 x3500
      in (d_OP__case_43 x4 x3 x1 x2 (Curry_Prelude.d_OP_eq_eq x3 x4 x3250 x3500) x3250 x3500)

d_OP_updateGhcOptions_dot___hash_lambda4 :: C_ReplState -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_ReplState Curry_Prelude.C_Bool)
d_OP_updateGhcOptions_dot___hash_lambda4 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (d_OP___hash_updR_at_ReplState_dot_rcvars x1 x2 x3250 x3500) Curry_Prelude.C_True) x3250 x3500

d_C_createAndCompileMain :: C_ReplState -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_ReplState C_MainCompile)
d_C_createAndCompileMain x1 x2 x3 x4 x3250 x3500 = let
     x5 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_outputSubdir x1 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))) x3250 x3500) x3250 x3500
     x6 = Curry_Prelude.d_OP_ampersand_ampersand (d_OP___hash_selR_at_ReplState_dot_useGhci x1 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_not x2 x3250 x3500) (Curry_Prelude.d_C_not (d_OP___hash_selR_at_ReplState_dot_interactive x1 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (d_C_getGoalInfo x1 x3250 x3500) (d_OP_createAndCompileMain_dot___hash_lambda5 x4 x3 x5 x1 x6) x3250 x3500)

d_OP_createAndCompileMain_dot___hash_lambda5 :: Curry_Prelude.C_Maybe Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_ReplState -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Bool Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_ReplState C_MainCompile)
d_OP_createAndCompileMain_dot___hash_lambda5 x1 x2 x3 x4 x5 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> Curry_Prelude.d_OP_gt_gt_eq (d_C_updateGhcOptions x4 x3250 x3500) (d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6 x1 x7 x8 x2 x3 x5) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_createAndCompileMain_dot___hash_lambda5 x1 x2 x3 x4 x5 x1002 x3250 x3500) (d_OP_createAndCompileMain_dot___hash_lambda5 x1 x2 x3 x4 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_createAndCompileMain_dot___hash_lambda5 x1 x2 x3 x4 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_createAndCompileMain_dot___hash_lambda5 x1 x2 x3 x4 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.C_Maybe Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 C_ReplState Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_ReplState C_MainCompile)
d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x4 x5 x6 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_writeFile x5) (d_C_mainModule x8 x2 x3 x1 x3250 x3500) x3250 x3500) (let
          x10 = d_C_ghcCall x8 x6 x9 x5 x3250 x3500
           in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (d_C_writeVerboseInfo x8 (Curry_Prelude.C_Int 2#)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) x10 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_OP__case_42 x8 x10 x4 x6 x3250 x3500) (d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda8 x2 x3) x3250 x3500) x3250 x3500)) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x4 x5 x6 x1002 x3250 x3500) (d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x4 x5 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x4 x5 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x4 x5 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 :: C_ReplState -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_ReplState Curry_Prelude.C_Int)
d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x1 x2) x3250 x3500

d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda8 :: Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 C_ReplState Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_ReplState C_MainCompile)
d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda8 x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x4 (d_OP__case_41 x5 x2 x1 (Curry_Prelude.d_OP_gt x5 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500)) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda8 x1 x2 x1002 x3250 x3500) (d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda8 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda8 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda8 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_compileWithGhci :: C_ReplState -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_ReplState Curry_Prelude.C_Int)
d_C_compileWithGhci x1 x2 x3 x3250 x3500 = let
     x4 = d_OP__case_39 x1 (d_OP___hash_selR_at_ReplState_dot_ghcicomm x1 x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x4 x2 x3250 x3500) (Curry_Prelude.d_OP_gt (d_OP___hash_selR_at_ReplState_dot_verbose x1 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) (d_OP_compileWithGhci_dot___hash_lambda10 x3 x1) x3250 x3500)

d_OP_compileWithGhci_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_ReplState -> Curry_GhciComm.C_GhciComm -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_ReplState Curry_Prelude.C_Int)
d_OP_compileWithGhci_dot___hash_lambda10 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (d_C_writeVerboseInfo x2 (Curry_Prelude.C_Int 1#)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_Prelude.d_C_apply (Curry_Utils.d_C_strip x3250 x3500) x1 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_GhciComm.d_C_evalMainCmd x3 (d_OP___hash_selR_at_ReplState_dot_showTime x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 (d_OP___hash_updR_at_ReplState_dot_ghcicomm x2 (Curry_Prelude.C_Just x3) x3250 x3500) (Curry_Prelude.C_Int 0#)) x3250 x3500) x3250 x3500) x3250 x3500

d_C_ghcCall :: C_ReplState -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_ghcCall x1 x2 x3 x4 x3250 x3500 = let
     x5 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem (d_OP___hash_selR_at_ReplState_dot_idSupply x1 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List)) x3250 x3500
     x6 = d_OP__case_31 x1 (d_OP___hash_selR_at_ReplState_dot_ndMode x1 x3250 x3500) x3250 x3500
     x10 = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_apply (Curry_Utils.d_C_notNull x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_rtsOpts x1 x3250 x3500) x3250 x3500) x6 x3250 x3500
     x11 = d_OP__case_30 x1 (Curry_Installation.d_C_installGlobal x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_unwords (Curry_Prelude.d_C_filter (Curry_Utils.d_C_notNull x3250 x3500)) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Installation.d_C_ghcExec x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Installation.d_C_ghcOptions x3250 x3500) (Curry_Prelude.OP_Cons (d_OP__case_38 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (d_OP___hash_selR_at_ReplState_dot_optim x1 x3250 x3500) (Curry_Prelude.d_C_not x2 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (d_OP__case_37 x2 x3250 x3500) (Curry_Prelude.OP_Cons (d_OP__case_36 x1 (Curry_Prelude.d_OP_lt (d_OP___hash_selR_at_ReplState_dot_verbose x1 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (d_OP__case_35 x5 x3250 x3500) (Curry_Prelude.OP_Cons (d_OP__case_34 x6 x3250 x3500) (Curry_Prelude.OP_Cons (d_OP__case_33 x10 x3250 x3500) (Curry_Prelude.OP_Cons (d_OP__case_32 x3 x3250 x3500) (Curry_Prelude.OP_Cons (d_OP___hash_selR_at_ReplState_dot_ghcOpts x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List)))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) Curry_Prelude.OP_List)) (Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) x11 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons x4 Curry_Prelude.OP_List))))))))))))))) x3250 x3500)

d_C_mainModule :: C_ReplState -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Maybe Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_mainModule x1 x2 x3 x4 x3250 x3500 = let
     x5 = d_OP__case_26 x1 (Curry_RCFile.d_C_rcValue (d_OP___hash_selR_at_ReplState_dot_rcvars x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))) x3250 x3500) x3250 x3500
     x30 = d_OP__case_9 x1 x5 (d_OP___hash_selR_at_ReplState_dot_interactive x1 x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_C_unlines (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.OP_Cons (d_OP__case_28 x1 (d_OP___hash_selR_at_ReplState_dot_interactive x1 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.OP_Cons (d_OP__case_27 x4 (Curry_Prelude.d_OP_eq_eq x4 Curry_Prelude.C_Nothing x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.d_C_apply (Curry_FilePath.d_C_dropExtension x3250 x3500) (d_C_mainGoalFile x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons Curry_Prelude.OP_List (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.OP_Cons (d_C_mainExpr (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))))))))))) x2 x3 (d_OP___hash_selR_at_ReplState_dot_ndMode x1 x3250 x3500) x30 x4 x3250 x3500) Curry_Prelude.OP_List))))))))) x3250 x3500)

d_C_mainExpr :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> C_NonDetMode -> C_EvalMode -> Curry_Prelude.C_Maybe Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_mainExpr x1 x2 x3 x4 x5 x6 x3250 x3500 = let
     x7 = d_OP__case_6 x2 x3250 x3500
     x8 = Curry_Prelude.d_C_maybe (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))) d_OP_mainExpr_dot_printWithBindings_dot_77 x6 x3250 x3500
     x9 = d_OP__case_5 x5 x3250 x3500
     x11 = d_OP__case_4 x2 x3 x4 x9 x8 (Curry_Prelude.d_OP_ampersand_ampersand x3 x2 x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus x11 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.d_OP_plus_plus x7 x1 x3250 x3500)) x3250 x3500) x3250 x3500)

d_OP_mainExpr_dot_printWithBindings_dot_77 :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_mainExpr_dot_printWithBindings_dot_77 x1 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) Curry_Prelude.OP_List))) Curry_Prelude.d_C_show x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_enumFromTo (Curry_Prelude.C_Int 1#) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) Curry_Prelude.OP_List))))))) Curry_Prelude.d_C_show x3250 x3500) (Curry_Prelude.d_C_enumFromTo (Curry_Prelude.C_Int 1#) x1 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_mainExpr_dot_searchExpr_dot_77 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_mainExpr_dot_searchExpr_dot_77 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) x1) x3250 x3500

d_OP__case_4 :: Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> C_NonDetMode -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_4 x2 x3 x4 x9 x8 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) Curry_Prelude.OP_List))))))
     Curry_Prelude.C_False -> d_OP__case_3 x3 x4 x9 x8 x2 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x2 x3 x4 x9 x8 x1002 x3250 x3500) (d_OP__case_4 x2 x3 x4 x9 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x2 x3 x4 x9 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x2 x3 x4 x9 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.C_Bool -> C_NonDetMode -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_3 x3 x4 x9 x8 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) Curry_Prelude.OP_List))))
     Curry_Prelude.C_False -> d_OP__case_2 x4 x9 x8 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x3 x4 x9 x8 x1002 x3250 x3500) (d_OP__case_3 x3 x4 x9 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x3 x4 x9 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x3 x4 x9 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: C_NonDetMode -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_2 x4 x9 x8 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) Curry_Prelude.OP_List)))))
     Curry_Prelude.C_False -> d_OP__case_1 x4 x9 x8 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x4 x9 x8 x1002 x3250 x3500) (d_OP__case_2 x4 x9 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x4 x9 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x4 x9 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: C_NonDetMode -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_1 x4 x9 x8 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> d_OP__case_0 x9 x8 x4 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x4 x9 x8 x1002 x3250 x3500) (d_OP__case_1 x4 x9 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x4 x9 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x4 x9 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_NonDetMode -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x9 x8 x4 x3250 x3500 = case x4 of
     C_PrDFS -> Curry_Prelude.d_OP_dollar (d_OP_mainExpr_dot_searchExpr_dot_77 x8) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) x3250 x3500
     C_DFS -> Curry_Prelude.d_OP_dollar (d_OP_mainExpr_dot_searchExpr_dot_77 x8) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) Curry_Prelude.OP_List)))))))) x9 x3250 x3500) x3250 x3500
     C_BFS -> Curry_Prelude.d_OP_dollar (d_OP_mainExpr_dot_searchExpr_dot_77 x8) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) Curry_Prelude.OP_List)))))))) x9 x3250 x3500) x3250 x3500
     (C_IDS x12) -> Curry_Prelude.d_OP_dollar (d_OP_mainExpr_dot_searchExpr_dot_77 x8) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus x9 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.d_C_show x12 x3250 x3500)) x3250 x3500) x3250 x3500) x3250 x3500
     (C_Par x13) -> Curry_Prelude.d_OP_dollar (d_OP_mainExpr_dot_searchExpr_dot_77 x8) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))) x9 x3250 x3500) x3250 x3500
     (C_PrtChoices x14) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.d_C_show x14 x3250 x3500)) x3250 x3500
     (Choice_C_NonDetMode x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x9 x8 x1002 x3250 x3500) (d_OP__case_0 x9 x8 x1003 x3250 x3500)
     (Choices_C_NonDetMode x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x9 x8 z x3250 x3500) x1002
     (Guard_C_NonDetMode x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x9 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_NonDetMode x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: C_EvalMode -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_5 x5 x3250 x3500 = case x5 of
     C_All -> Curry_Prelude.OP_List
     C_One -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) Curry_Prelude.OP_List
     (C_Interactive x10) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_C_show x10 x3250 x3500) x3250 x3500
     (Choice_C_EvalMode x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1002 x3250 x3500) (d_OP__case_5 x1003 x3250 x3500)
     (Choices_C_EvalMode x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 z x3250 x3500) x1002
     (Guard_C_EvalMode x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_EvalMode x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_6 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List)))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1002 x3250 x3500) (d_OP__case_6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: C_ReplState -> C_MoreDefault -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_EvalMode
d_OP__case_9 x1 x5 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> C_Interactive x5
     Curry_Prelude.C_False -> d_OP__case_8 x1 (d_OP___hash_selR_at_ReplState_dot_firstSol x1 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x5 x1002 x3250 x3500) (d_OP__case_9 x1 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: C_ReplState -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_EvalMode
d_OP__case_8 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> C_One
     Curry_Prelude.C_False -> d_OP__case_7 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x1002 x3250 x3500) (d_OP__case_8 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_EvalMode
d_OP__case_7 x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> C_All
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1002 x3250 x3500) (d_OP__case_7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: C_ReplState -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_26 x1 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x8 = x6
           in (d_OP__case_25 x8 x7 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char 'y'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> C_MoreYes
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x1 x1002 x3250 x3500) (d_OP__case_26 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_25 x8 x7 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_OP__case_24 x7 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_19 x8 x7 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char 'n'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x8 x7 x1002 x3250 x3500) (d_OP__case_25 x8 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x8 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x8 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_19 x8 x7 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_OP__case_18 x7 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_15 x8 x7 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char 'a'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x8 x7 x1002 x3250 x3500) (d_OP__case_19 x8 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x8 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x8 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_15 x8 x7 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_OP__case_14 x7 x3250 x3500
     Curry_Prelude.C_False -> C_MoreYes
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x8 x7 x1002 x3250 x3500) (d_OP__case_15 x8 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x8 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x8 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_14 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x22 x23) -> let
          x24 = x22
           in (d_OP__case_13 x24 x23 (Curry_Prelude.d_OP_eq_eq x24 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> C_MoreYes
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1002 x3250 x3500) (d_OP__case_14 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_13 x24 x23 x25 x3250 x3500 = case x25 of
     Curry_Prelude.C_True -> d_OP__case_12 x23 x3250 x3500
     Curry_Prelude.C_False -> C_MoreYes
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x24 x23 x1002 x3250 x3500) (d_OP__case_13 x24 x23 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x24 x23 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x24 x23 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_12 x23 x3250 x3500 = case x23 of
     (Curry_Prelude.OP_Cons x25 x26) -> let
          x27 = x25
           in (d_OP__case_11 x27 x26 (Curry_Prelude.d_OP_eq_eq x27 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> C_MoreYes
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1002 x3250 x3500) (d_OP__case_12 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_11 x27 x26 x28 x3250 x3500 = case x28 of
     Curry_Prelude.C_True -> d_OP__case_10 x26 x3250 x3500
     Curry_Prelude.C_False -> C_MoreYes
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x27 x26 x1002 x3250 x3500) (d_OP__case_11 x27 x26 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x27 x26 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x27 x26 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_10 x26 x3250 x3500 = case x26 of
     Curry_Prelude.OP_List -> C_MoreAll
     (Curry_Prelude.OP_Cons x28 x29) -> C_MoreYes
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1002 x3250 x3500) (d_OP__case_10 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_18 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x17 x18) -> let
          x19 = x17
           in (d_OP__case_17 x19 x18 (Curry_Prelude.d_OP_eq_eq x19 (Curry_Prelude.C_Char 'o'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> C_MoreYes
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x1002 x3250 x3500) (d_OP__case_18 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_17 x19 x18 x20 x3250 x3500 = case x20 of
     Curry_Prelude.C_True -> d_OP__case_16 x18 x3250 x3500
     Curry_Prelude.C_False -> C_MoreYes
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x19 x18 x1002 x3250 x3500) (d_OP__case_17 x19 x18 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x19 x18 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x19 x18 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_16 x18 x3250 x3500 = case x18 of
     Curry_Prelude.OP_List -> C_MoreNo
     (Curry_Prelude.OP_Cons x20 x21) -> C_MoreYes
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x1002 x3250 x3500) (d_OP__case_16 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_24 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x9 x10) -> let
          x11 = x9
           in (d_OP__case_23 x11 x10 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> C_MoreYes
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x1002 x3250 x3500) (d_OP__case_24 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_23 x11 x10 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> d_OP__case_22 x10 x3250 x3500
     Curry_Prelude.C_False -> C_MoreYes
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x11 x10 x1002 x3250 x3500) (d_OP__case_23 x11 x10 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x11 x10 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x11 x10 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_22 x10 x3250 x3500 = case x10 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x14 = x12
           in (d_OP__case_21 x14 x13 (Curry_Prelude.d_OP_eq_eq x14 (Curry_Prelude.C_Char 's'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> C_MoreYes
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1002 x3250 x3500) (d_OP__case_22 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_21 x14 x13 x15 x3250 x3500 = case x15 of
     Curry_Prelude.C_True -> d_OP__case_20 x13 x3250 x3500
     Curry_Prelude.C_False -> C_MoreYes
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x14 x13 x1002 x3250 x3500) (d_OP__case_21 x14 x13 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x14 x13 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x14 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_MoreDefault
d_OP__case_20 x13 x3250 x3500 = case x13 of
     Curry_Prelude.OP_List -> C_MoreYes
     (Curry_Prelude.OP_Cons x15 x16) -> C_MoreYes
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1002 x3250 x3500) (d_OP__case_20 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.C_Maybe Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_27 x4 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x4 x1002 x3250 x3500) (d_OP__case_27 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: C_ReplState -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_28 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))))))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x1 x1002 x3250 x3500) (d_OP__case_28 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_30 :: C_ReplState -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_30 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_map (Curry_Prelude.d_C_flip (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_outputSubdir x1 x3250 x3500)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) (d_OP___hash_selR_at_ReplState_dot_importPaths x1 x3250 x3500)) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_29 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x1 x1002 x3250 x3500) (d_OP__case_30 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_29 :: C_ReplState -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_29 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_kics2Home x1 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_kics2Home x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))))) (d_OP___hash_selR_at_ReplState_dot_idSupply x1 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) Curry_Prelude.OP_List)) (Curry_Prelude.d_C_map (Curry_Prelude.d_C_flip (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_OP___hash_selR_at_ReplState_dot_outputSubdir x1 x3250 x3500)) (d_C_loadPaths x1 x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x1 x1002 x3250 x3500) (d_OP__case_29 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_31 :: C_ReplState -> C_NonDetMode -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_31 x1 x10 x3250 x3500 = case x10 of
     (C_Par x7) -> Curry_Prelude.C_True
     C_DFS -> Curry_Prelude.C_False
     C_BFS -> Curry_Prelude.C_False
     (C_IDS x8) -> Curry_Prelude.C_False
     C_PrDFS -> Curry_Prelude.C_False
     (C_PrtChoices x9) -> Curry_Prelude.C_False
     (Choice_C_NonDetMode x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x1 x1002 x3250 x3500) (d_OP__case_31 x1 x1003 x3250 x3500)
     (Choices_C_NonDetMode x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x1 z x3250 x3500) x1002
     (Guard_C_NonDetMode x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_NonDetMode x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_32 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_32 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)))))))))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x1002 x3250 x3500) (d_OP__case_32 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_33 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_33 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x1002 x3250 x3500) (d_OP__case_33 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_34 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_34 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x1002 x3250 x3500) (d_OP__case_34 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_35 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_35 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List)))))))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x1002 x3250 x3500) (d_OP__case_35 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_36 :: C_ReplState -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_36 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) Curry_Prelude.OP_List))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) Curry_Prelude.OP_List))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x1 x1002 x3250 x3500) (d_OP__case_36 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_37 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_37 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x1002 x3250 x3500) (d_OP__case_37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_38 :: Curry_Prelude.C_Bool -> C_ReplState -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_38 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) Curry_Prelude.OP_List))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x2 x1 x1002 x3250 x3500) (d_OP__case_38 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_39 :: C_ReplState -> Curry_Prelude.C_Maybe Curry_GhciComm.C_GhciComm -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_GhciComm.C_GhciComm
d_OP__case_39 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_Nothing -> acceptCs id Curry_GhciComm.d_C_initGhciComm
     (Curry_Prelude.C_Just x5) -> acceptCs id (Curry_GhciComm.d_C_restartGhciComm x5)
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x1 x1002 x3250 x3500) (d_OP__case_39 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_39 :: C_ReplState -> Curry_Prelude.C_Maybe Curry_GhciComm.C_GhciComm -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func Curry_Prelude.C_Bool (Curry_Prelude.C_IO Curry_GhciComm.C_GhciComm))
nd_OP__case_39 x1 x6 x3000 x3250 x3500 = case x6 of
     Curry_Prelude.C_Nothing -> wrapDX (wrapDX id) (acceptCs id Curry_GhciComm.d_C_initGhciComm)
     (Curry_Prelude.C_Just x5) -> wrapDX (wrapDX id) (acceptCs id (Curry_GhciComm.d_C_restartGhciComm x5))
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_39 x1 x1002 x3000 x3250 x3500) (nd_OP__case_39 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_39 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_39 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_41 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_MainCompile
d_OP__case_41 x5 x2 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> C_MainError
     Curry_Prelude.C_False -> d_OP__case_40 x2 x1 (Curry_Prelude.d_OP_bar_bar x1 x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x5 x2 x1 x1002 x3250 x3500) (d_OP__case_41 x5 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x5 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x5 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_40 :: Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_MainCompile
d_OP__case_40 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> C_MainDet
     Curry_Prelude.C_False -> C_MainNonDet
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x2 x1 x1002 x3250 x3500) (d_OP__case_40 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_42 :: C_ReplState -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_ReplState Curry_Prelude.C_Int)
d_OP__case_42 x8 x10 x4 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_compileWithGhci x8 x10 x4 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (Curry_System.d_C_system x10 x3250 x3500) (d_OP_createAndCompileMain_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 x8) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x8 x10 x4 x1002 x3250 x3500) (d_OP__case_42 x8 x10 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x8 x10 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x8 x10 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_43 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_ReplState -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 C_ReplState Curry_Prelude.C_Bool)
d_OP__case_43 x4 x3 x1 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x1 Curry_Prelude.C_False) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt (Curry_RCFile.d_C_setRCProperty x2 x4 x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_RCFile.d_C_readRC x3250 x3500) (d_OP_updateGhcOptions_dot___hash_lambda4 x1) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x4 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_43 x4 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x4 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x4 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_44 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_44 x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x1002 x3250 x3500) (d_OP__case_44 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_45 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_45 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x1002 x3250 x3500) (d_OP__case_45 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_46 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_46 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_46 x1002 x3250 x3500) (d_OP__case_46 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_46 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_46 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
