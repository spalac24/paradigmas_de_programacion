import System.Directory
import CurryToHaskell hiding (transOp)
import Names
import MetaProgramming.FlatCurryGoodies
import qualified FunctionalProg as C
import MetaProgramming.FlatCurry
import SafeCalls
import Config
import List
import System.Environment
import System.IO
--import System.Console.SimpleLineEditor
import Char
import Monad
import PreTrans (isIOType)

main = do as <- getArgs
          case as of
            [file] -> makeExternal file
            _      -> error "usage: makeExternalInterfaces <modulename>"

makeExternal file = safe $ do
  (opts0,_) <- safeIO readConfig
  cymake (opts0{filename=file})
  let ext = externalSpecName file
      opts = opts0{verbosity=4}
  prog <- safeIO (readFlatCurry (file++".fcy"))
  exist <- safeIO (doesModuleExist ext)
  if exist then spl "External specification already exists!"
           else analyseProg opts prog

analyseProg :: Options -> Prog -> Safe IO ()
analyseProg opts (Prog name _ types funcs ops) = do
  ps1 <- mkTypes opts name (filter isExternalType types)
  ps2 <- mkInstances opts name 
           (filter (\t -> isDataTypeDecl t && not (isExternalType t)) types)
  ps3 <- mkFuns opts name (filter isExternal funcs) ops
  let ps = ps1++ps2++ps3
  unless (null ps) $ do
    safeIO $ writeFile (externalSpecName name) (show ps)
    spl "external specification written"


mkTypes :: Options -> String -> [TypeDecl] -> Safe IO [Provided]
mkTypes _ _ [] = spl "no external types found" >> return []
mkTypes opts name ts = do
  let tns = map (snd . typeName) ts
  spl $ "external types found: "++show tns
  let fn = extDataHsName name
      mn = extDataModName name
  writeProgram opts (fn,True,C.Prog mn imps [] (map extTDecl ts) [] [] [])
  return (map (flip ForType Nothing) tns)

extTDecl (Type (m,n) _ vs _) = 
  C.TypeSyn (extDataModName m,constructorName n) C.Public 
            (map (varName "t") vs)
            (C.TCons (modName "Prelude","Prim") [C.TCons ("",n) []])

spl = safeIO . putStrLn

mkInstances :: Options -> String -> [TypeDecl] -> Safe IO [Provided]
mkInstances _ _ [] = spl "no userdefined types found" >> return []
mkInstances opts name ts = do
  spl "toggle instance with (S)how (R)ead (B)aseCurry (C)urry, (n) for next type"
  ps0 <- mapM (askType . flip ForType (Just []) . snd . typeName) ts
  let ps = filter (\ (ForType _ (Just xs)) -> not (null xs)) ps0
  unless (null ps) $ do
      let fn = extInstHsName name
          mn = extInstModName name
          lt = zip (map (snd . typeName) ts) ts
      writeProgram opts (fn,True,C.Prog mn imps [] [] (concatMap (extIDecl lt) ps) [] [])
      return ()
  return ps

extIDecl :: [(String,TypeDecl)] -> Provided -> [C.InstanceDecl]
extIDecl lt (ForType n (Just is)) = map inst is
  where
    Just (Type _ _ vs _) = lookup n lt
    vars = map toTVar vs
    inst i = C.Instance (map (tc . (:[])) vars) 
                        (tc [C.TCons ("",n) vars])
                        []
      where
        tc = C.TypeClass ("",show i)

askType :: Provided -> Safe IO Provided
askType p@(ForType n (Just is)) = do 
  c <- readAnswer str
  case toUpper c of
    'S' -> toggle Show
    'R' -> toggle Read
    'B' -> toggle BaseCurry
    'C' -> toggle Curry
    'N' -> spl str >> return p
    _   -> askType p
  where
    str = "for type "++n++": "++show is
    toggle i = askType (ForType n (Just (sort (tog i is))))
    tog i [] = [i]
    tog i (x:xs) = if x==i then xs else x : tog i xs


mkFuns :: Options -> String -> [FuncDecl] -> [OpDecl] -> Safe IO [Provided]
mkFuns _ _ [] _ = spl "no external functions found" >> return []
mkFuns opts name fs ops = do
  let fns = map (snd . funcName) fs
  spl $ "external functions found: "++show fns
  let fn = extFuncHsName name
      mn = extFuncModName name
      newOps = map transOp (filter (\o->elem (snd $ opName o) fns) ops)

  writeProgram opts 
    (fn,True,C.Prog mn imps [] [] [] (map (extFDecl opts) fs) newOps)
  return (map ForFunction fns)

extFDecl :: Options -> FuncDecl -> C.FuncDecl
extFDecl opts (Func name arity _ t _) = 
  C.Func name C.Public (transFType opts arity t)
        (Just [C.Rule [] (noguard extExpr) []])
  where
    extExpr = C.Apply (C.Symbol ("CurryPrelude",
                       (if isIOType (resultType t)
                         then "ioFunc"
                         else "extFunc")++show arity))
                     (C.Symbol ("Prelude","undefined"))

transOp (Op name InfixOp p)  = C.Op name C.InfixOp  p
transOp (Op name InfixlOp p) = C.Op name C.InfixlOp p
transOp (Op name InfixrOp p) = C.Op name C.InfixrOp p

{-
externalInterface (Prog name _ _ funcs ops) =
  C.Prog ("External"++name) imports [] [] [] newFuncs newOps
  where
    extFuncs = filter isExternal funcs
    imports = "Curry":"ExternalPrelude":"CurryTypes":map ("Data"++) usedDataModules
    usedDataModules = nub (concatMap (map fst . allTypeCons . funcType) extFuncs)
    newOps = map trOp (filter (\o->elem (opName o) (map funcName extFuncs)) ops)
    newFuncs = map skelett extFuncs

skelett (Func name arity _ t _) = 
  C.Func name 0 C.Public (Just (transFType t))
        (Just [C.Rule [] (noguard extExpr) []])
  where
    extExpr = C.Apply (C.Symbol ("ExternalPrelude",
                       (if isIOType (resultType t)
                         then "ioFunc"
                         else "extFunc")++show arity))
                     (C.Symbol ("Prelude","undefined"))
  


-}


imps = ["Curry","CurryPrelude"]
-----------------
-- line editing
-----------------

readAnswer :: String -> Safe IO Char
readAnswer w = safeIO (putStr w >> getLine >>= return . head){-safeIO $ do 
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  putStr w 
  hFlush stdout
  c <- hGetChar stdin
  delChars w
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  return c-}