{- |
    Module      :  $Header$
    Description :  Computation of module dependencies
    Copyright   :  (c) 2002 - 2004 Wolfgang Lux
                       2005        Martin Engelke
                       2007        Sebastian Fischer
                       2011 - 2012 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    This module implements the functions to compute the dependency
    information between Curry modules. This is used to create Makefile
    dependencies and to update programs composed of multiple modules.
-}

-- TODO (bjp): Propagate errors
-- Currently errors during the dependency search (like missing files
-- or errors during parsing a module header) lead to calls of the error
-- function. This dramatically limits the usability as a library.

module CurryDeps
  ( Source (..), flatDeps, deps, flattenDeps, sourceDeps, moduleDeps ) where

import Control.Monad (foldM, liftM, unless)
import Data.List (isSuffixOf, nub)
import qualified Data.Map as Map (Map, empty, insert, lookup, toList)
import Text.PrettyPrint

import Curry.Base.Ident
import Curry.Base.Message (runMsg, Message, message)
import Curry.Files.Filenames
import Curry.Files.PathUtils
import Curry.Syntax (Module (..),  ImportDecl (..), parseHeader, patchModuleId)

import Base.Messages (abortWithMessage, internalError)
import Base.SCC (scc)
import CompilerOpts (Options (..), Extension (..))

-- |Different types of source files
data Source
  = Source FilePath [ModuleIdent] -- ^ A source file with module imports
  | Interface FilePath            -- ^ An interface file
  | Unknown                       -- ^ An unkonwn file
    deriving (Eq, Ord, Show)

type SourceEnv = Map.Map ModuleIdent Source

-- |Retrieve the dependencies of a source file in topological order
-- and possible errors during flattering
flatDeps :: Options -> FilePath -> IO ([(ModuleIdent, Source)], [Message])
flatDeps opts fn = flattenDeps `liftM` deps opts Map.empty fn

-- |Retrieve the dependencies of a source file as a 'SourceEnv'
deps :: Options -> SourceEnv -> FilePath -> IO SourceEnv
deps opts sEnv fn
  | ext   ==   icurryExt  = return sEnv
  | ext `elem` sourceExts = sourceDeps opts sEnv fn
  | otherwise             = targetDeps opts sEnv fn
  where ext = takeExtension fn

-- The following functions are used to lookup files related to a given
-- module. Source files for targets are looked up in the current
-- directory only. Two different search paths are used to look up
-- imported modules, the first is used to find source modules, whereas
-- the library path is used only for finding matching interface files. As
-- the compiler does not distinguish these paths, we actually check for
-- interface files in the source paths as well.

-- In order to compute the dependency graph, source files for each module
-- need to be looked up. When a source module is found, its header is
-- parsed in order to determine the modules that it imports, and
-- dependencies for these modules are computed recursively. The prelude
-- is added implicitly to the list of imported modules except for the
-- prelude itself.

-- |Retrieve the dependencies of a given target file
targetDeps :: Options -> SourceEnv -> FilePath -> IO SourceEnv
targetDeps opts sEnv fn = do
  mFile <- lookupFile [""] sourceExts fn
  case mFile of
    Nothing   -> return $ Map.insert (mkMIdent [fn]) Unknown sEnv
    Just file -> sourceDeps opts sEnv file

-- |Retrieve the dependencies of a given source file
sourceDeps :: Options -> SourceEnv -> FilePath -> IO SourceEnv
sourceDeps opts sEnv fn = readHeader fn >>= moduleDeps opts sEnv fn

-- |Retrieve the dependencies of a given module
moduleDeps :: Options -> SourceEnv -> FilePath -> Module -> IO SourceEnv
moduleDeps opts sEnv fn (Module m _ is _) = case Map.lookup m sEnv of
  Just  _ -> return sEnv
  Nothing -> do
    let imps  = imports opts m is
        sEnv' = Map.insert m (Source fn imps) sEnv
    foldM (moduleIdentDeps opts) sEnv' imps

-- |Retrieve the imported modules and add the import of the Prelude
-- according to the compiler options.
imports :: Options -> ModuleIdent -> [ImportDecl] -> [ModuleIdent]
imports opts m ds = nub $
     [preludeMIdent | m /= preludeMIdent && implicitPrelude]
  ++ [m' | ImportDecl _ m' _ _ _ <- ds]
  where implicitPrelude = NoImplicitPrelude `notElem` optExtensions opts

-- |Retrieve the dependencies for a given 'ModuleIdent'
moduleIdentDeps :: Options -> SourceEnv -> ModuleIdent -> IO SourceEnv
moduleIdentDeps opts sEnv m = case Map.lookup m sEnv of
  Just _  -> return sEnv
  Nothing -> do
    mFile <- lookupCurryModule (optImportPaths opts) (optLibraryPaths opts) m
    case mFile of
      Nothing -> return $ Map.insert m Unknown sEnv
      Just fn
        | icurryExt `isSuffixOf` fn ->
            return $ Map.insert m (Interface fn) sEnv
        | otherwise                 -> do
            hdr@(Module m' _ _ _) <- readHeader fn
            unless (m == m') $ abortWithMessage $ errWrongModule m m'
            moduleDeps opts sEnv fn hdr

readHeader :: FilePath -> IO Module
readHeader fn = do
  mbFile <- readModule fn
  case mbFile of
    Nothing  -> abortWithMessage $ errMissingFile fn
    Just src -> do
      case runMsg $ parseHeader fn src of
        Left  err      -> abortWithMessage err
        Right (hdr, _) -> return $ patchModuleId fn hdr

-- If we want to compile the program instead of generating Makefile
-- dependencies, the environment has to be sorted topologically. Note
-- that the dependency graph should not contain any cycles.
flattenDeps :: SourceEnv -> ([(ModuleIdent, Source)], [Message])
flattenDeps = fdeps . sortDeps
  where
  sortDeps :: SourceEnv -> [[(ModuleIdent, Source)]]
  sortDeps = scc idents imported . Map.toList

  idents (m, _) = [m]

  imported (_, Source _ ms) = ms
  imported (_,           _) = []

  fdeps :: [[(ModuleIdent, Source)]] -> ([(ModuleIdent, Source)], [Message])
  fdeps = foldr checkdep ([], [])

  checkdep []    (srcs, errs) = (srcs      , errs      )
  checkdep [src] (srcs, errs) = (src : srcs, errs      )
  checkdep dep   (srcs, errs) = (srcs      , err : errs)
    where err = errCyclicImport $ map fst dep

errMissingFile :: FilePath -> Message
errMissingFile fn = message $ sep $ map text [ "Missing file:", fn ]

errWrongModule :: ModuleIdent -> ModuleIdent -> Message
errWrongModule m m' = message $ sep $
  [ text "Expected module for", text (moduleName m) <> comma
  , text "but found", text (moduleName m') ]

errCyclicImport :: [ModuleIdent] -> Message
errCyclicImport []  = internalError "CurryDeps.errCyclicImport: empty list"
errCyclicImport [m] = message $ sep $ map text
  [ "Recursive import for module", moduleName m ]
errCyclicImport ms  = message $ sep $
  text "Cylic import dependency between modules" : punctuate comma inits
  ++ [text "and", lastm]
  where
  (inits, lastm)     = splitLast $ map (text . moduleName) ms
  splitLast []       = internalError "CurryDeps.splitLast: empty list"
  splitLast (x : []) = ([]    , x)
  splitLast (x : xs) = (x : ys, y) where (ys, y) = splitLast xs
