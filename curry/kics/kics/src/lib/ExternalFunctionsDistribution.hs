module ExternalFunctionsDistribution where

import Curry
import CurryPrelude 
import qualified InstallDir as ID

curryCompiler :: Result C_String
curryCompiler _ = toCurry "kics"

curryCompilerMajorVersion :: Result C_Int
curryCompilerMajorVersion _ = 0

curryCompilerMinorVersion :: Result C_Int
curryCompilerMinorVersion _ = 9854

installDir :: Result C_String
installDir _ = toCurry (ID.installDir)

curryRuntime :: Result C_String
curryRuntime _ = toCurry "ghc"

curryRuntimeMajorVersion :: Result C_Int
curryRuntimeMajorVersion _ = 6

curryRuntimeMinorVersion :: Result C_Int
curryRuntimeMinorVersion _ = 8

