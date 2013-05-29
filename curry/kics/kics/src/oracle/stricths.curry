import FlatCurry
import FlatCurryGoodies
import Integer hiding (max)
import Maybe
import List
import Sort
import System (getArgs)
import FileGoodies (stripSuffix)
import PrettyStrict (showProg,clearName)
import qualified TransTools as TT
import Char
import Make
import Directory
import ReadShowTerm
import FiniteMap
import LiftCases

-------------------------------------
-- constants and naming conventions
-------------------------------------

prefix :: String
prefix = "Strict"

-- Bibliothek fuer Laufzeitsystem des Debuggers
strictLib, prelude, liftedType, liftFunction, traceFunction, extModPrefix :: String
strictLib     = "StrictSteps"
prelude       = "Prelude"
liftedType    = "Debug"
liftedHOType  = "Func"
hoType        = "Func"
liftFunction  = "return"
traceFunction = "traceFunCall"
consTerm      = "consTerm"
debugMonad    = "Debug"
extModPrefix  = "ExternalStrict"
dataType      = (strictLib,"Data")
impPrelude = "Prelude ((.),Eq(..),Show(..),String)\n\
             \import qualified Prelude (IO,return,(>>=),error,(+))\n\
             \import qualified Prelude as P"

applyFuns :: [String]
applyFuns = ["$!","$!!","$#","$##"]

extTypes :: [QName]
extTypes = map ((,) prelude) (["[]","Char","Float","IO"] ++ 
                              map (parens . flip replicate ',') [0..20])
  where
    parens s = '(':s++")"

extCons :: [QName]
extCons = map ((,) prelude) ["[]",":"]

targetName, incName, trustName :: String -> String
targetName s = prefix ++ s ++ ".hs"
incName    s = "../"++prefix ++ s ++ ".inc"
trustName  s = "../"++ s ++ ".trust"

rnm :: QName -> QName
rnm mn@(m,n) = (prefix++m,
  if   elem mn (extTypes++extCons) 
  then clearName n
  else n)

addFcy :: String -> String
addFcy = (++".fcy")

----------------------------------
-- main program and io actions
----------------------------------

main :: IO ()
main = do
  (force, quiet, stFile, progName) <- parseArgs
  transform quiet stFile force progName

type Args = (Bool,    --  force transformation,
             Bool,
             String,  --  name of stepfile
             String)  --  name of module)
           

isForce   s = s=="-f" || s=="--forced"
isQuiet   s = s=="-q" || s=="--quiet"
mStepFile s = case s of
                 '-':'s':fn -> Just fn
                 '-':'-':'s':'t':'e':'p':'f':'i':'l':'e':fn -> Just fn
                 _ -> Nothing

parseArgs :: IO Args
parseArgs = do 
  args <- getArgs
  if (null args) 
    then error "usage: stricths [-f|--force] \
               \[-s<filename>|--stepfile<filename>] <modulename>"
    else return (any isForce args,
                 any isQuiet args,
                 maybe (last args) id 
                       (listToMaybe (catMaybes (map mStepFile args))),
                 last args)
 where last xs = xs !! (length xs-1)

transform :: Bool -> String -> Bool -> String -> IO ()
transform quiet stFile force progName = 
  make quiet progName tester (writeTrans stFile)
  where
    tester = if force then (\ _ _ -> return Nothing) 
                      else obsolete quiet targetName [addFcy,incName,trustName] readTypes
    readTypes fns = do
     prog <- readFile (head fns) 
     let typeString = dropWhile (/='[') $ dropWhile (/=']') $ dropWhile (/='[') prog
         types = fst $ head $ readsTerm typeString
     return $ makeConsTable types

type CNames = FM QName QName

ltQName :: QName -> QName -> Bool
ltQName (m1,n1) (m2,n2) = let cm = cmpString m1 m2
                           in cm==LT || (cm==EQ && cmpString n1 n2==LT)

makeConsTable :: [TypeDecl] -> CNames
makeConsTable ts = 
  listToFM ltQName $ 
  concatMap (fst . foldl mkConsTab ([],(1,1,1,1,1))) $ 
  map typeConsDecls $ 
  filter (not . isTypeSyn) ts
  where
    co s = (strictLib,s)
    mkConsTab (ps,(zero,one,two,three,n)) t = case consArity t of
      0 -> ((consName t,if zero<4 then co $ "C0_"++show zero
                                  else co $ "C0_ "++show zero):ps,
            (zero+1,one,two,three,n))
      1 -> ((consName t,if one<4 then co $ "C1_"++show one
                                 else co $ "C1_ "++show one):ps,
            (zero,one+1,two,three,n))
      2 -> ((consName t,if two<4 then co $ "C2_"++show two
                                 else co $ "C2_ "++show two):ps,
            (zero,one,two+1,three,n))
      3 -> ((consName t,if three<4 then co $ "C3_"++show three
                                   else co $ "C3_ "++show three):ps,
            (zero,one,two,three+1,n))
      _ -> ((consName t,co $ "C "++show n):ps,
            (zero,one,two,three,n+1))
      

writeTrans :: String -> String -> [CNames] -> Prog -> IO CNames
writeTrans stFile path names prog = do
  let fn    = path ++ targetName (progName prog)
      inc   = path ++ incName    (progName prog)
      trust = path ++ trustName  (progName prog)
      nameTable = foldr plusFM (makeConsTable (progTypes prog)) names
  ex <- doesFileExist inc 
  include <- if ex then putStrLn "reading include file" >> readFile inc 
                   else return ""
  ex <- doesFileExist trust 
  trustInfo <- if ex then readFile trust >>= return . Just 
                     else return Nothing
  putStrLn ("generating "++fn)
  let lprog = liftCases False prog
      rprog = transProg stFile (mkTrustData trustInfo) nameTable lprog
  writeFile fn (showProg include rprog)
  return nameTable

----------------------------------
-- the transformation
----------------------------------

transProg :: String -> Trust -> CNames -> Prog -> Prog
transProg stFile trust nameTable prog
  = updProg (prefix++) 
            (((strictLib:) . (impPrelude:) . map (prefix++)))
            (concatMap transType . filter ((/=(prelude,"IO")) . typeName))
            ((concatMap (showDecl nameTable) (progTypes prog)++)
             . concatMap (addFtraceCall stFile trust . transFunc nameTable))
            (filter (not . isApplyName . opName))
            (updQNamesInProg cleanName prog)


transType :: TypeDecl -> [TypeDecl]
transType t = trType (\ (m,n) v a _  -> let (m',n')=rnm (m,n)
                                            name'=(m',clearName n') in
                                      [Type name' v a []])
                      (\ _ _ _ _ -> []) t

transFunc :: CNames -> FuncDecl -> FuncDecl 
transFunc names func = updFuncType (transFuncType (funcArity func)) $
                       updFuncBody (strictExpr callFunc names) func
  where
    callFunc = Comb (FuncPartCall 1) (rnm (funcName func)) 
                    (map Var (woLast (funcArgs func)))
    woLast []  = []
    woLast [_] = []
    woLast (x:xs@(_:_)) = x:woLast xs

transCons :: ConsDecl -> ConsDecl
transCons (Cons mn a v ts) = Cons mn a v (map liftArgType ts)
                              
-- --------------------------------
{-
  transform identifiers that are legal in FlatCurry but illegal
  in Haskell
-}
 
cleanName :: QName -> QName
cleanName (mod,name) = (mod,showIdentifier name)
    where showIdentifier :: String -> String
          showIdentifier n 
              | n == "[]" = "[]"
              | n == "_" = "_"
              | isInfixOpName n = n --"("++n++")"
              | isTuple name = n
              | otherwise = let newName = foldr normChars [] n
                            in if head newName=='\'' then
                                   "c_"++newName
                               else newName
          normChars c cs
              | c == '_' || c == '\'' = c : cs
              | inRange c ('a','z')   = c : cs
              | inRange c ('A','Z')   = c : cs
              | inRange c ('0','9')   = c : cs
              | otherwise = '\'' : show (ord c) ++ cs
          inRange ch (l, u) = ord ch >= ord l && ord ch <= ord u
          isInfixOpName = all (`elem` infixIDs)
          infixIDs =  "~!@#$%^&*+-=<>?./|\\:"
          isTuple [] = False
          isTuple (c:cs) = c=='(' && dropWhile (==',') cs == ")"


-- --------------------------------
{-
  lift type declarations
-}

(~>) :: TypeExpr -> TypeExpr -> TypeExpr
t1 ~> t2 =  TCons (strictLib,hoType) [t1,t2]

liftArgType :: TypeExpr -> TypeExpr
liftArgType var@(TVar _)      = var
liftArgType (TCons name args) = TCons (rnm name) (map liftArgType args)
liftArgType (FuncType texp1 texp2) = liftArgType texp1 ~> liftArgType texp2
 
liftResultType :: Int -> TypeExpr -> TypeExpr
liftResultType _    var@(TVar _)     = addStepType var
liftResultType _    (TCons name args) = 
  addStepType (TCons (rnm name) (map liftArgType args))
liftResultType arity (FuncType texp1 texp2) 
    | arity == 0 = addStepType (liftArgType texp1 ~> liftArgType texp2)
    | otherwise = 
         FuncType (liftArgType texp1) (liftResultType (arity-1) texp2)


addStepType :: TypeExpr -> TypeExpr    
addStepType   = addType liftedType

addType :: String ->  TypeExpr -> TypeExpr    
addType s texp = TCons (strictLib, s) [texp]  

transFuncType :: Int -> TypeExpr -> TypeExpr
transFuncType arity t 
  | isCaseAuxFuncType t = t
  | otherwise = liftResultType arity t


-- -------------------------------
{-
  declare a function showXxx for every data type Xxx
-}

showDecl :: CNames -> TypeDecl -> [FuncDecl]
showDecl _ (TypeSyn _ _ _ _) = []
showDecl names ty@(Type mt@(mname,_) vis'ty tvars consDecls)
    | null consDecls = []
    | otherwise
    = (Func funcName 1 vis'ty typeExpr (rule branches):genDecl ty ++
       reverse (foldl selectors [] consDecls) ++ map constructor consDecls)
      where funcName  = rnm (mname, "showCons" ++ (clearName tname'))
            mt'@(_,tname') = rnm mt
            typeExpr  = FuncType
                        (TCons mt' (map TVar tvars))
                        (TCons (strictLib, "Term") [])
            branches = reverse (snd (foldl consDecl2pattern (1,[]) consDecls))
            rule = Rule [0] . (Case Rigid (toData (Var 0)))
            consDecl2pattern (n,bs) (Cons mn@(_, consname) arity _ _)
                = let expr = snd (complexShowCall (selectorName mn) n [1..arity] 
                                  consname)
                  in (n,branch mn arity expr:bs)
            branch mn arity = Branch $ Pattern (lupName names mn) [1 .. arity] 
            selectors fs (Cons mn arity v ts) =
               fst (foldl selectArg (fs,1) ts)
               where
                 selectArg (fs',m) t =
                   (Func (selectorName mn m) 1 v 
                         (FuncType (TCons mt' (map TVar tvars)) (liftArgType t))
                         (rule [branch mn arity (fromData (Var m))]):fs',m+1)
            constructor (Cons mn arity v ts) =
               Func (constructorName mn) arity v 
                    (foldr (\ t1 -> FuncType (liftArgType t1)) 
                           (TCons mt' (map TVar tvars)) ts)
                    (Rule [1 .. arity] 
                       (fromData (Comb ConsCall (lupName names mn) $
                          if arity < 4 
                          then args
                          else take 4 args ++ [toList (drop 4 args)])))  
                                   
              where
                args = map (toData . Var) [1 .. arity]
            
genDecl :: TypeDecl -> [FuncDecl]
genDecl (Type mt@(mname,_) vis'ty tvars consDecls) =
  [Func funcName 0 vis'ty typeExpr rule]
  where
    funcName  = rnm (mname, "_i_generator")
    mt' = rnm mt
    typeExpr  = TCons (strictLib,"Generator")
                      [TCons mt' (map TVar tvars)]
    rule = Rule [0] (evaL (callMkOrs (foldr bindGen (2,0,id) consDecls)))
    bindGen c (n,m,fgen) = 
      (n+1,max m (consArity c),(genConstructor c >>>>= (Var n)) . fgen)
    

    genConstructor (Cons mn arity _ _) = 
      let args = map Var [2 .. arity+1] in
       evaL $ (snd $ foldr addGen (0,id) args)
              (preturn (Comb FuncCall (constructorName mn) args))

    addGen e (i,f) = (i+1,(generator (Var 1 +. Lit (Intc i)) >>>>= e) . f)
    callMkOrs (n,m,fgen) = (childRef m >>>>= Var 1)
                           (fgen (mkOrs [Var 1,Lit (Intc m),Var 0,
                                         toList (map Var (reverse [2..n-1]))]))


selectorName :: QName -> Int -> QName
selectorName mc n = namer "sel_" (show n) mc

callSelector ::  QName -> Int -> Expr -> Expr
callSelector mn n e = Comb FuncCall (selectorName mn n) [e]

constructorName :: QName -> QName
constructorName = namer "constr_" ""

namer :: String -> String -> QName -> QName 
namer s1 s2 mc = let (m,c) = rnm mc 
                     c' = if c=="[]" then "List" else
                          if c==":"  then "Cons" else c in
  (m,s1++m++"_"++c'++s2)


complexShowCall :: (Int->QName) -> Int -> [Int] -> String -> (Int,Expr)
complexShowCall _ n []     consname = (n,callConsTerm consname [])
complexShowCall f n vs@(_:_) consname = 
  (n',Comb FuncCall (strictLib,consTerm) [string consname,toList (reverse args)])
  where
    (n',args) = foldl select (n,[]) vs
    select (m,cs) _ = (m+1,showTerm (Comb FuncCall (f m) [Var 0]):cs)

-- -------------------------------
{-
  add call to traceFunction to every top level declaration
  add call to traceProgram to main function
-}

addFtraceCall :: String -> Trust -> FuncDecl -> [FuncDecl]
addFtraceCall stFile trust f
    | isExternalOrGlobal f    = [] 
    --| isCaseAuxFuncName (snd $ funcName f) = [f]
    | fname == "expression" = traced iotype (libcall traceFun
                                             [string stFile, funcBody f])
    | otherwise       = traced (funcType f) (expr' (funcBody f))
 where 
   traceFun   = (case resultType (funcType f) of 
                  TCons (_,"Debug") [TCons (_,"IO") _] -> "run"
                  _ -> "trace") ++ "WithStepfile"
   fname      = snd (funcName f)
   ruleVs     = if isExternal f then [1 .. typeArity] 
                                else ruleArgs (funcRule f)
   expr' e    = case trust fname of
     Nothing -> evaL e
     Just ts -> let args _ [] vs@(_:_) = map (showTerm . Var) vs
                    args _ _  []       = []
                    args j (i:is) (v:vs) 
                      | j==i      = trustedArg:args (j+1) is vs
                      | otherwise = showTerm (Var v):args (j+1) (i:is) vs
       in libcall traceFunction 
                  [callToConsTerm fname (args 1 ts ruleVs),e]
   iotype     = TCons (prelude, "IO") [TCons (prelude, "()") []]
   traced t e = [updFuncType (const t) $
                 updFuncRule (\ _ -> Rule ruleVs e) f]
   typeArity  = trTypeExpr (\ _ -> 1) (\ _ _ -> 1) (\ _ n -> 1+n) (funcType f) - 1

-- -------------------------------
{-
  helper functions
-}


libcall fun args = Comb FuncCall (strictLib, fun) args

libcons fun args = Comb ConsCall (strictLib, fun) args

fcall fun args = Comb FuncCall (prelude, fun) args

ccall con args = Comb ConsCall ("", con) args

toList l = foldr colon nil l

nil        = ccall "[]" []

colon x xs = ccall ":" [x,xs]

string = foldr (colon . Lit . Charc) nil

isApplyName :: QName -> Bool
isApplyName (m,n) = m==prelude && elem n applyFuns

isExternalOrGlobal f = 
  isExternal f || case funcType f of
                   FuncType _ (TCons g [_]) -> g==("Global","Global")
                   _ -> False

intToInt Zero = "StrictSteps.C0_1"
intToInt (Pos n) = "(StrictSteps.C1_2 "++natToNat n++")"
intToInt (Neg n) = "(StrictSteps.C1_1 "++natToNat n++")"

natToNat IHi = "StrictSteps.C0_1"
natToNat (I n) = "(StrictSteps.C1_2 "++natToNat n++")"
natToNat (O n) = "(StrictSteps.C1_1 "++natToNat n++")"

(>>>=) :: Expr -> Expr -> Expr
e1 >>>= e2 = fcall ">>=" [e1,e2]
(e1 >>>>= e2) e3 = fcall ">>=" [e1,e2,e3]

preturn :: Expr -> Expr
preturn e1 = fcall "return" [e1]

fromData :: Expr -> Expr
fromData e = libcall "fromData" [e]
toData   e = libcall "toData"   [e]
showTerm e = libcall "showTerm" [e]
unwrap e = case e of
  Comb FuncCall (_,'t':_) _ -> e
  _ -> toData e
mkOr = libcall "mkOr"
genericCase f e = libcall "genericCase" [f,e]
mkOrs e = libcall "mkGenOr" e
childRef n = libcall "childRef" [Lit $ Intc n]
generator e = libcall "generator" [e]
frEE = libcall "free" []
e1 +. e2 = fcall "+" [e1,e2]

pcreturn :: Expr
pcreturn = Comb (FuncPartCall 1) ("Prelude","return") []

callConsTerm :: String -> [Expr] -> Expr
callConsTerm s es = libcall "consTerm" [string s,toList es]

callToConsTerm :: String -> [Expr] -> Expr
callToConsTerm s es = libcall "consTerm" [string s,toList es]

app :: Expr -> Expr -> Expr
app e1 e2 = Comb FuncCall ("StrictPrelude","ap") [e1,e2]

trustedArg :: Expr 
trustedArg = libcall "trusted" []

------------------------------------
-- transforming expressions
------------------------------------

type Env = [(Int,Either Expr Expr)]
type SelEnv = FM Int Expr
type T a = (Int,SelEnv) -> (Int,a)

(>>=.) :: (T a) -> (a -> T b) -> T b
(ta >>=. f) st@(_,selenv) = case ta st of (i',a) -> f a (i',selenv)

ret :: a -> T a
ret a (i,_) = (i,a)

nextIdx :: T Int
nextIdx (i,_) = (i+1,i)

nextIdxs :: Int -> T [Int]
nextIdxs n (i,_) = (i+n,[i .. i+n-1])

selEnv :: T SelEnv
selEnv = id

emptySelEnv :: SelEnv
emptySelEnv = emptyFM (<)

sequence :: [T a] -> T [a]
sequence [] = ret []
sequence (tx:txs) = tx >>=. \x -> sequence txs >>=. \xs -> ret (x:xs)

maxVar :: Expr -> Int
maxVar = trExpr id zero (\ _ _ -> maxs) 
                (\ bs b -> maxs (b:map (uncurry max) bs)) 
                (\ vs v -> maxs (v:vs)) max (\ _ v vs -> maxs (v:vs))
                (\ p v -> max (trPattern (\ _ -> maxs) zero p) v)
  where
    zero _ = 0
    
max :: Int -> Int -> Int    
max x y = if x>y then x else y

maxs :: [Int] -> Int
maxs = foldl max 0 

strictExpr :: Expr -> CNames -> Expr -> Expr
strictExpr callFunc names expr = 
  mend (trExpr var lit comb leT freE oR casE branch expr 
              (maxVar expr + 1,emptySelEnv))
  where
    var i (idx,selenv) = (idx,([],Left (maybe (Var i) id (lookupFM selenv i))))
    lit l = ret ([],Left (case l of
     Intc i   -> ccall "Int"   [ccall (intToInt i) []]
     Floatc f -> ccall "Float" [ccall "PrimFloat" [ccall (show f) []]]
     Charc  c -> ccall "Char"  [ccall "PrimChar"  [ccall (show c) []]]))
    comb ct (m,n) targs = 
      sequence targs >>=. \ args ->
      liftArgs args >>=. \ (env,args') -> 
      case ct of
        FuncCall -> ret (env,Right (Comb ct (rnm (m,n)) args'))
        ConsCall -> ret (env,Left (Comb FuncCall (constructorName (m,n)) args'))
        ConsPartCall j -> 
          let j' = j+length args'
              call = Comb (FuncPartCall j') (constructorName (m,n)) []
              pc = libcall ("pc"++show j') 
                           [prim n [],dot (retDot (j'-1)) call] in
          liftPc pc args' >>=. \ (env',e) ->
          ret (env++env',e)
        FuncPartCall j ->
          let j' = j+length args'
              call = Comb (FuncPartCall j') (rnm (m,n)) []
              pc = libcall ("pc"++show j') 
                           [prim n [],call] in
          liftPc pc args' >>=. \ (env',e) ->
          ret (env++env',e)
    leT tbs te = let (vs,tes) = unzip tbs in
                 sequence tes >>=. \ envses ->
                 te >>=. \ (env,e) -> 
                 liftBinds (zip vs envses) >>=. \ env' -> 
                 countBodyStep env >>=. \ envBody ->
                 ret (env' ++ envBody,e)
    freE vs te = liftFreeVars vs >>=. \ envVars -> 
                 te >>=. \ (env,e) ->
                 ret (envVars++env,e)
    oR te1 te2 = sequence [te1,te2] >>=. \ args ->
                 liftArgs args >>=. \ (env,args') -> 
                 ret (env,Right (mkOr args'))
    casE ct te bs = 
      selEnv >>=. \ sel ->
      te >>=. \ (env,e) -> 
      case e of
        Left  v@(Var i) -> nextIdx >>=. \ idx ->
                   ret (env,Right (Case ct (toData v) 
                                         (map ($(v,idx,sel)) bs++genBr i)))
        Right c -> nextIdx >>=. \i ->
                   nextIdx >>=. \idx ->
                   ret (env++[(i,Right c)],
                        Right (Case ct (toData (Var i)) 
                                     (map ($(Var i,idx,sel)) bs++genBr i)))
                    
    branch p e (e',i,selenv) = case trPattern (cPattern e') lPattern p of
      (sels,p') -> Branch p' (evaL (mend (e (i,addListToFM selenv sels))))

    lPattern l = ([],case l of
             Intc i   -> Pattern ("",intToInt i) []
             Floatc f -> Pattern (strictLib,"PrimFloat "++show f) []
             Charc c  -> Pattern (strictLib,"PrimChar "++show c)  [])

    cPattern e mn args = 
     (zipWith (\ i j -> (j,callSelector mn i e)) [1..] args,Pattern (lup mn) args)

    mend (_,(env,e)) = foldr addBind (either addRet id e) env

    -- a small trick to get lambda lifting in the printer
    addBind (i,Right e) e' = fcall (">>=") [e,Var i,e']
    addBind (i,Left  e) e' = Let [(i,e)] e'
 
    lup = lupName names

    genBr i = [Branch (Pattern ("","_") []) (genericCase callFunc (Var i))]


lupName names nm = maybe (error $ "undefined constructor: "++show nm++" in "
                            ++show (fmToList names))
                   id (lookupFM names nm)


liftArgs :: [(Env,Either Expr Expr)] -> T (Env,[Expr])
liftArgs [] = ret ([],[])
liftArgs ((env,e):es) = liftArgs es >>=. \ (env',es') ->
                  case e of
                    Left  v -> ret (env++env',v:es')
                    Right c -> nextIdx >>=. \i ->
                               ret (env++(i,Right c):env',Var i:es')

liftBinds :: [(Int,(Env,Either Expr Expr))] -> T Env
liftBinds [] = ret [] 
liftBinds ((i,(env,e)):bs) = 
  liftBinds bs >>=. \ env'->
  case e of
    Left  v -> ret (env++(i,Left v):env')
    Right c -> ret (env++(i,Right c):env')

liftFreeVars :: [Int] -> T Env
liftFreeVars [] = ret []
liftFreeVars (v:vs) = 
  liftFreeVars vs >>=. \ env -> 
  ret ((v,Right frEE):env)

liftPc :: Expr -> [Expr] ->  T (Env,Either Expr Expr)
liftPc e []  = ret ([],Left e)
liftPc e [x] = ret ([],Right (app e x))
liftPc e (x:xs@(_:_)) = nextIdx >>=. \ i ->
                        liftPc (Var i) xs >>=. \ (env,e') ->
                        ret ((i,Right (app e x)):env,e')

addInc e  = Right (evaL e)
evaL e    = libcall "eval" [e]   
addRet e  = fcall   "return" [e]


countBodyStep :: Env -> T Env
countBodyStep [] = nextIdx >>=. \ i ->
                   ret [(i,Right (libcall "oneStep" []))]
countBodyStep ((i,e):xs) = ret ((i,either (addInc . addRet) addInc e):xs)



err :: String -> T (Env,Either Expr Expr)
err s = ret ([],Left $ fcall "error"
                            [string $ s++" not supported in b.i.o."])

prim n xs = libcall consTerm [
                  string n,
                  toList (map (libcall "showTerm" . (:[])) xs)]

retDot n | n==0      = Comb (FuncPartCall 1) (prelude,liftFunction) []
         | otherwise = Comb (FuncPartCall 1) (prelude,".") [retDot (n-1)]

dot e1 e2 = fcall "." [e1,e2]

------------------------------------
-- working with trust information
------------------------------------

type Trust = String -> Maybe [Int]

-- a trust file is a list of function names eventually leaded by '!'.
-- a name with a ! is not trusted a name without is trusted
-- the first entry in a trust file descides the default: 
-- if the first has a ! then the default is trusted
-- if the first has no ! then the default is not trusted
-- if there is no first entry (i.e., an empty file) all functions are trusted
-- infix operators have to be written in parenthesis
-- trust settings for external functions are explicitly in the .inc file

mkTrustData :: Maybe String -> Trust
mkTrustData Nothing   = \s->if isCaseAuxFuncName s then Nothing else Just []
mkTrustData (Just info) = case filter (not . null) (map strip (lines info)) of
  [] -> const Nothing
  ls -> testTrust (head (head ls) == '!') (map funArgs ls)
 where
    funArgs :: String -> (String,[Int])
    funArgs line = case break isSpace line of
      (fun,args) -> (fun,trustArgs 1 (words args))

    trustArgs :: Int -> [String] -> [Int]
    trustArgs _ [] = []
    trustArgs i (x:xs) = case x of
     "_" -> i:trustArgs (i+1) xs
     _   -> trustArgs (i+1) xs

testTrust :: Bool -> [(String,[Int])] -> Trust
testTrust mostlyTrusted info fun 
  | isCaseAuxFuncName fun = Nothing
  | mostlyTrusted = lookup ('!':fun) info 
  | otherwise     = case lookup fun info of
      Nothing -> Just []
      Just _  -> Nothing

strip :: String -> String
strip s = reverse (str (reverse (str s)))
  where str = dropWhile isSpace