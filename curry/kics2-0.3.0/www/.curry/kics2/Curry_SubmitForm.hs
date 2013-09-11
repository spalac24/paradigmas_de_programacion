{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_SubmitForm (d_C_main) where

import Basics
import qualified Curry_HtmlCgi
import qualified Curry_Prelude
d_C_main :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_main x3250 x3500 = Curry_HtmlCgi.d_C_submitForm x3250 x3500
