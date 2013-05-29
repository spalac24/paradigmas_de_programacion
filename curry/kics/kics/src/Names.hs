module Names where

import Char
import List
import System.FilePath

import ShowFunctionalProg (isTuple,isInfixOpName)

---------------------------------------------------------------------------
-- generating names to avoid clashes with Haskell
---------------------------------------------------------------------------
-- constructor names

preludeConstructorName "()" = "T0"
preludeConstructorName "[]" = "List"
preludeConstructorName ":"  = ":<"
preludeConstructorName n 
  | isTuple n = "T"++show (1+length (takeWhile (==',') (tail n)))
  | otherwise = 'C':'_':n

constructorName = preludeConstructorName

consName extFuncs (m,n) = 
  case m of
   "Prelude" -> (dataDefMod extFuncs m,preludeConstructorName n)
   ""        -> ("",preludeConstructorName n)
   _         -> (dataDefMod extFuncs m,constructorName n)


{-
extConsName exts (m,n) = case m of
   "Prelude" -> (datamod m,preludeConstructorName n)
   ""        -> ("",preludeConstructorName n)
   _         -> (datamod m,constructorName n)
  where
    datamod = if elem (m,n) exts then extDataModName else dataModName
-}

dataDefMod :: Bool -> String -> String
dataDefMod False = modName
dataDefMod True  = instModName

-- function names
preludeFunctionName s@"share" = s
preludeFunctionName n = functionName n

functionName n | isInfixOpName n = elimInfix n 
               | otherwise = 'c':'_':n

funName (p@"Prelude",n) = (modName p,preludeFunctionName n)
funName (m,n) = (modName m,functionName n)

elimInfix name = "op_"++concat (intersperse "_" (map (show . ord) name))

-----------------------------------------
-- naming conventions for new objects
-----------------------------------------
-- module names

insertName :: String -> FilePath -> FilePath
insertName s xs = replaceFileName xs (s++takeFileName xs)

modName s = insertName "Curry" s

dataMName = "Data"
instMName = "Instances"
funcMName = "Functions"
dbgMName  = "Oracle"

external = insertName "External"

extDataMName = external dataMName
extInstMName = external instMName
extFuncMName = external funcMName

dataModName = insertName dataMName 
instModName = insertName instMName 
funcModName = insertName funcMName 
dbgModName  = insertName dbgMName

extDataModName = insertName extDataMName 
extInstModName = insertName extInstMName 
extFuncModName = insertName extFuncMName 

dataHsName s = replaceExtension (dataModName s) ".hs"
instHsName s = replaceExtension (instModName s) ".hs"
funcHsName s = replaceExtension (modName s)     ".hs"

extDataHsName s = replaceExtension (extDataModName s) ".hs"
extInstHsName s = replaceExtension (extInstModName s) ".hs"
extFuncHsName s = replaceExtension (extFuncModName s) ".hs"

externalSpecName s = replaceExtension (external s) ".spec"

strictPrefix = "S"

mkStrictName = insertName strictPrefix


-- names for new constructors
--addPrefix s _ (p@"Prelude","Int")   = (instModName p,"C_Int"++s)
--addPrefix s _ (p@"Prelude","Float") = (instModName p,"Prim"++s)
addPrefix s (m,n) = (m,n++s)

freeVarName = addPrefix "FreeVar"
failName    = addPrefix "Fail"
orName      = addPrefix "Or"
suspName    = addPrefix "Susp"


