module Paths_kics2_runtime (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,2,4], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/spalac24/.cabal/bin"
libdir     = "/home/spalac24/.cabal/lib/kics2-runtime-0.2.4/ghc-7.6.2"
datadir    = "/home/spalac24/.cabal/share/kics2-runtime-0.2.4"
libexecdir = "/home/spalac24/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "kics2_runtime_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "kics2_runtime_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "kics2_runtime_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "kics2_runtime_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
