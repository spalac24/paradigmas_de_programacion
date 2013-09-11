{- |
    Module      :  $Header$
    Description :  Code transformations
    Copyright   :  (c) 2011, Björn Peemöller (bjp@informatik.uni-kiel.de)
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    This module subsumes the different transformations of the source code.
-}
module Transformations where

import Curry.Base.Ident
import Curry.Syntax

import Base.Types

import Env.Value
import Env.TypeConstructor

import Transformations.CaseCompletion as CC (completeCase)
import Transformations.CurryToIL      as IL (ilTrans, transType)
import Transformations.Desugar        as DS (desugar)
import Transformations.Lift           as L  (lift)
import Transformations.Qual           as Q  (qual)
import Transformations.Simplify       as S  (simplify)

import CompilerEnv
import CompilerOpts
import Imports (qualifyEnv)
import qualified IL

-- |Add missing case branches
completeCase :: IL.Module -> CompilerEnv -> (IL.Module, CompilerEnv)
completeCase mdl env = (CC.completeCase (interfaceEnv env) mdl, env)

-- |Translate into the intermediate language
ilTrans :: Bool -> Module -> CompilerEnv -> (IL.Module, CompilerEnv)
ilTrans flat mdl env = (il, env)
  where il = IL.ilTrans flat (valueEnv env) (tyConsEnv env) mdl

-- |Translate a type into its representation in the intermediate language
transType :: ModuleIdent -> ValueEnv -> TCEnv -> Type -> IL.Type
transType = IL.transType

-- |Remove syntactic sugar
desugar :: Module -> CompilerEnv -> (Module, CompilerEnv)
desugar mdl env = (mdl', env { valueEnv = tyEnv' })
  where (mdl', tyEnv') = DS.desugar (valueEnv env) (tyConsEnv env) mdl

-- |Lift local declarations
lift :: Module -> CompilerEnv -> (Module, CompilerEnv)
lift mdl env = (mdl', env { valueEnv = tyEnv' })
  where (mdl', tyEnv') = L.lift (valueEnv env) mdl

-- |Fully qualify used constructors and functions
qual :: Options -> CompilerEnv -> Module -> (CompilerEnv, Module)
qual opts env (Module m es is ds) = (qualifyEnv opts env, Module m es is ds')
  where ds' = Q.qual (moduleIdent env) (tyConsEnv env) (valueEnv env) ds

-- |Simplify the source code
simplify :: Bool -> Module -> CompilerEnv -> (Module, CompilerEnv)
simplify flat mdl env = (mdl', env { valueEnv = tyEnv' })
  where (mdl', tyEnv') = S.simplify flat (valueEnv env) mdl
