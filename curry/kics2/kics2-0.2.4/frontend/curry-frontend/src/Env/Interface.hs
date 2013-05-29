{- |
    Module      :  $Header$
    Description :  Environment of imported interfaces
    Copyright   :  (c) 2002 - 2004, Wolfgang Lux
                       2011       , Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    This module provides an environment for imported interfaces.
-}
module Env.Interface where

import qualified Data.Map as Map (Map, empty, lookup)

import Curry.Base.Ident (ModuleIdent)
import Curry.Syntax     (Interface)

type InterfaceEnv = Map.Map ModuleIdent Interface

lookupInterface :: ModuleIdent -> InterfaceEnv -> Maybe Interface
lookupInterface = Map.lookup

initInterfaceEnv :: InterfaceEnv
initInterfaceEnv = Map.empty
