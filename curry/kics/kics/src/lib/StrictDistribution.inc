
import qualified InstallDir as ID

curryCompiler ::  Debug (List Char) 
curryCompiler = trace0 ("kics")

curryCompilerMajorVersion ::  (Debug Int) 
curryCompilerMajorVersion = trace0 (0::P.Integer)

curryCompilerMinorVersion ::  (Debug Int) 
curryCompilerMinorVersion = trace0 (9854::P.Integer)

curryRuntime :: Debug (List Char)
curryRuntime = trace0 ("ghc")

curryRuntimeMajorVersion ::  (Debug Int) 
curryRuntimeMajorVersion = trace0 (6::P.Integer)

curryRuntimeMinorVersion ::  (Debug Int) 
curryRuntimeMinorVersion = trace0 (8::P.Integer)

installDir ::  Debug (List Char) 
installDir = trace0 (ID.installDir)

