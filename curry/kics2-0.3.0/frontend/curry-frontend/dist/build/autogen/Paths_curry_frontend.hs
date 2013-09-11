module Paths_curry_frontend (
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
version = Version {versionBranch = [0,3,7], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/spalac24/EAFIT/PP/paradigmas_de_programacion/curry/kics2-0.3.0/bin"
libdir     = "/home/spalac24/EAFIT/PP/paradigmas_de_programacion/curry/kics2-0.3.0/pkg/lib/curry-frontend-0.3.7/ghc-7.4.2"
datadir    = "/home/spalac24/EAFIT/PP/paradigmas_de_programacion/curry/kics2-0.3.0/pkg/share/curry-frontend-0.3.7"
libexecdir = "/home/spalac24/EAFIT/PP/paradigmas_de_programacion/curry/kics2-0.3.0/pkg/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "curry_frontend_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "curry_frontend_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "curry_frontend_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "curry_frontend_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
