{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Message (d_C_putErrLn, d_C_showStatus, d_C_showAnalysis, d_C_showDetail, d_C_showLevel) where

import Basics
import qualified Curry_CompilerOpts
import qualified Curry_IO
import qualified Curry_Prelude
import qualified Curry_Utils
d_C_putErrLn :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_putErrLn x1 x3500 = Curry_IO.d_C_hPutStrLn (Curry_IO.d_C_stderr x3500) x1 x3500

d_C_showStatus :: Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_showStatus x1 x2 x3500 = d_C_showLevel Curry_CompilerOpts.C_VerbStatus x1 x2 x3500

d_C_showAnalysis :: Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_showAnalysis x1 x2 x3500 = d_C_showLevel Curry_CompilerOpts.C_VerbAnalysis x1 x2 x3500

d_C_showDetail :: Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_showDetail x1 x2 x3500 = d_C_showLevel Curry_CompilerOpts.C_VerbDetails x1 x2 x3500

d_C_showLevel :: Curry_CompilerOpts.C_Verbosity -> Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_showLevel x1 x2 x3 x3500 = Curry_Utils.d_C_unless (Curry_Prelude.d_OP_lt (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optVerbosity x2 x3500) x1 x3500) (Curry_Prelude.d_C_putStrLn x3 x3500) x3500
