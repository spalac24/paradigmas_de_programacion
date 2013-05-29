{- |
    Module      :  $Header$
    Description :  Environment of operator precedences
    Copyright   :  (c) 2002 - 2004, Wolfgang Lux
                       2011       , Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    In order to parse infix expressions correctly, the compiler must know
    the precedence and fixity of each operator. Operator precedences are
    associated with entities and will be checked after renaming was
    applied. Nevertheless, we need to save precedences for ambiguous names
    in order to handle them correctly while computing the exported
    interface of a module.

    If no fixity is assigned to an operator, it will be given the default
    precedence 9 and assumed to be a left-associative operator.

    /Note:/ this modified version uses Haskell type 'Integer'
    for representing the precedence. This change had to be done due to the
    introduction of unlimited integer constants in the parser / lexer.
-}

module Env.OpPrec
  ( OpPrecEnv, PrecInfo (..), OpPrec (..)
  , defaultP, bindP, lookupP, qualLookupP, initOpPrecEnv
  ) where

import Curry.Base.Ident
import Curry.Syntax (Infix (..))

import Base.TopEnv

data OpPrec = OpPrec Infix Integer deriving Eq

instance Show OpPrec where
  showsPrec _ (OpPrec fix p) = showString (assoc fix) . shows p
    where assoc InfixL = "left "
          assoc InfixR = "right "
          assoc Infix  = "non-assoc "

defaultP :: OpPrec
defaultP = OpPrec InfixL 9

data PrecInfo = PrecInfo QualIdent OpPrec deriving (Eq, Show)

instance Entity PrecInfo where
  origName (PrecInfo op _) = op

type OpPrecEnv = TopEnv PrecInfo

bindP :: ModuleIdent -> Ident -> OpPrec -> OpPrecEnv -> OpPrecEnv
bindP m op p
  | hasGlobalScope op = bindTopEnv fun op info . qualBindTopEnv fun qop info
  | otherwise         = bindTopEnv fun op info
  where qop  = qualifyWith m op
        info = PrecInfo qop p
        fun  = "Env.OpPrec.bindP"

-- The lookup functions for the environment which maintains the operator
-- precedences are simpler than for the type and value environments
-- because they do not need to handle tuple constructors.

lookupP :: Ident -> OpPrecEnv -> [PrecInfo]
lookupP = lookupTopEnv

qualLookupP :: QualIdent -> OpPrecEnv -> [PrecInfo]
qualLookupP = qualLookupTopEnv

initOpPrecEnv :: OpPrecEnv
initOpPrecEnv = predefTopEnv qConsId consPrec emptyTopEnv

consPrec :: PrecInfo
consPrec = PrecInfo qConsId (OpPrec InfixR 5)
