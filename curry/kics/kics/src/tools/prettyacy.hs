import Language.Haskell.Syntax
import Language.Haskell.Pretty
import System.IO (stdin,hGetContents)
import Char 

import AbstractCurry

main :: IO ()
main = do
  s <- hGetContents stdin
  case reads s of
    [(p,_)] -> do
                  --print (toHaskell p)
                  putStrLn (prettyPrint (toHaskell p))
                  putStrLn (externals p)
    _ -> putStrLn "usage: pipe abstract curry file into prettyacy"
  
newtype QualInfo = QualInfo (QName -> Bool)

mkQualInfo :: CurryProg -> QualInfo
mkQualInfo (CurryProg name _ _ _ _) = QualInfo (\ (m,_) -> m==name)

toHaskell :: CurryProg -> HsModule
toHaskell p@(CurryProg name imps ts fs os)
  = HsModule src (Module name) 
                 (exports qi ts fs) 
                 (map imports imps) 
                 (map (types qi) ts ++ concatMap (funcs qi) fs ++ map ops os)
  where
    qi = mkQualInfo p

src :: SrcLoc
src = SrcLoc {srcFilename = "", srcLine = 0, srcColumn = 0}

qual :: QualInfo -> QName -> HsQName
qual (QualInfo qi) mn@(m,n)
  | n=="[]"       = Special HsListCon
  | n=="()"       = Special HsUnitCon
  | n==":"        = Special HsCons
  | isTupleName n = Special (HsTupleCon (length n - 1))
  | otherwise     = (if qi mn then UnQual else Qual (Module m)) (ident n)

ident :: String -> HsName
ident n | isInfixName n   = HsSymbol n 
        | take 3 n == "x'x" && isRegularName n = HsIdent ('x':n)
        | isRegularName n = HsIdent n
        | null n          = error "empty identifier"
        | otherwise       = HsIdent ("x'x"++normalize n)
  where
    normalize "" = ""
    normalize (c:cs) | isIdentChar c = c:normalize cs
                     | otherwise     = show (ord c)++normalize cs

isInfixName :: String -> Bool
isInfixName = all (`elem` "~!@#$%^&*+-=<>?./|\\:")

isRegularName :: String -> Bool
isRegularName "" = False
isRegularName (c:cs) = isAlpha c && all isIdentChar cs

isIdentChar :: Char -> Bool 
isIdentChar c = isAlphaNum c || elem c "'_"

exports :: QualInfo -> [CTypeDecl] -> [CFuncDecl] -> Maybe [HsExportSpec]
exports qi ts fs = if null specs || allExported then Nothing else Just specs
                
  where
    (allExported,specs) = conc (map exportType ts ++ map exportFunc fs)

    conc = foldr (\ (b,xs) (c,ys) -> (b&&c,xs++ys)) (True,[])
  
    exportType (CType _  Private _ _) = (False,[])
    exportType (CType mn Public  _ cs) 
      | all private cs = (null cs,[HsEAbs $ qual qi mn])
      | any private cs = (False,[HsEThingWith (qual qi mn) 
                                              (concatMap conName cs)])
      | otherwise      = (True,[HsEThingAll $ qual qi mn])
        

    exportType (CTypeSyn _  Private _ _) = (False,[])
    exportType (CTypeSyn mn Public  _ _) = (True,[HsEAbs $ qual qi mn])

    private (CCons _ _ Private _) = True
    private (CCons _ _ Public  _) = False

    conName (CCons _     _ Private _) = []
    conName (CCons (_,n) _ Public  _) = [HsConName (ident n)]
    
    exportFunc (CFunc mn _ vis _ _)     = expF mn vis
    exportFunc (CmtFunc _ mn _ vis _ _) = expF mn vis

    expF _  Private = (False,[])
    expF mn Public  = (True,[HsEVar (qual qi mn)])


   
imports :: String -> HsImportDecl
imports s = HsImportDecl src (Module s) False Nothing Nothing

types :: QualInfo -> CTypeDecl -> HsDecl
types qi (CTypeSyn (_,n) _ vs te) = 
  HsTypeDecl src (ident n) (map (varOrUnderscore True) vs) (typeExp qi te)
types qi (CType (_,n) _ vs cs) = 
  HsDataDecl src [] (ident n) 
             (map (varOrUnderscore True) vs) (map (consDecl qi) cs) []

typeExp :: QualInfo -> CTypeExpr -> HsType
typeExp _ (CTVar v)         = HsTyVar (varOrUnderscore True v)
typeExp m (CFuncType t1 t2) = HsTyFun (typeExp m t1) (typeExp m t2)
typeExp m (CTCons mn tes)
 | isTupleName (snd mn) = HsTyTuple (map (typeExp m) tes)
 | otherwise = foldl HsTyApp (HsTyCon (qual m mn)) (map (typeExp m) tes)

var :: (Int,String) -> HsName
var = varOrUnderscore False

varOrUnderscore :: Bool -> (Int,String) -> HsName
varOrUnderscore True (i,"_") = HsIdent "_"
varOrUnderscore _    (i,"")  = ident ('x':show i)
varOrUnderscore _    (i,v)   = ident v

isTupleName :: String -> Bool
isTupleName ('(':xs) = dropWhile (==',') xs == ")"
isTupleName _ = False

consDecl :: QualInfo -> CConsDecl -> HsConDecl
consDecl m (CCons (_,n) _ _ ts) = 
  HsConDecl src (ident n) (map (HsUnBangedTy . typeExp m) ts)

funcs :: QualInfo -> CFuncDecl -> [HsDecl]
funcs qi (CFunc mn i _ t r)     = func qi mn i t r
funcs qi (CmtFunc _ mn i _ t r) = func qi mn i t r

func :: QualInfo -> QName -> Int -> CTypeExpr -> CRules -> [HsDecl]
func qi (_,n) _ typ r = 
  HsTypeSig src [ident n] (HsQualType [] (typeExp qi typ)):
  case r of CExternal _ -> []; CRules _ rs -> map (rule qi (ident n)) rs

-- all rules for a functions could be put together in one HsFunBind
rule :: QualInfo -> HsName -> CRule -> HsDecl
rule qi fun (CRule [] ges locs) = 
  HsPatBind src (HsPVar fun) (rhs qi ges) (concatMap (loc qi) locs)
rule qi fun (CRule ps ges locs) = 
  HsFunBind [HsMatch src fun (map (mp fun . pattern False qi) ps) 
                             (rhs qi ges) 
                             (concatMap (loc qi) locs)]
  where
    mp (HsSymbol _) = addPParToInfix
    mp (HsIdent  _) = id
  
rhs :: QualInfo -> [(CExpr,CExpr)] -> HsRhs
rhs qi [(CSymbol ("Prelude","success"),e)] = HsUnGuardedRhs (expr False qi e)
rhs qi ges = HsGuardedRhss (map guardedExpr ges)
  where
    guardedExpr (g,e) = HsGuardedRhs src (expr False qi g) (expr False qi e)

expr :: Bool -> QualInfo -> CExpr -> HsExp
expr _ _  (CVar v) = HsVar (UnQual (var v))
expr b _  (CLit (CIntc i)) | i <0 = maybePar b (HsNegApp (HsLit (HsInt (-i))))
expr _ _  (CLit l) = HsLit (lit l)
expr _ qi (CSymbol mn@(_,n))  
  | isConsName n  = HsCon (qual qi mn)
  | otherwise     = HsVar (qual qi mn)
expr b qi e@(CApply _ _) = application b qi (args e)
expr _ qi (CLambda [CPVar v] (CApply (CApply (CSymbol (m,s)) (CVar v')) e)) 
  | v==v' && isInfixName s = HsRightSection (qop qi m s) (expr True qi e)  
expr _ qi (CLambda [CPVar v] (CApply (CApply (CSymbol (m,s)) e) (CVar v')))
  | v==v' && isInfixName s = HsLeftSection (expr True qi e) (qop qi m s)  
expr b qi (CLambda ps e)    = 
  maybePar b (HsLambda src (map (pattern True qi) ps) (expr False qi e))
expr b qi (CLetDecl locs e) = 
  maybePar b (HsLet (concatMap (loc qi) locs) (expr False qi e))
expr b qi (CDoExpr xs)      = maybePar b (HsDo (map (statement qi) xs))
expr _ qi (CListComp e xs)  = 
  HsListComp (expr False qi e) (map (statement qi) xs)
expr b qi (CCase e bs)      = 
  maybePar b (HsCase (expr False qi e) (map (branch qi) bs))

application :: Bool -> QualInfo -> [CExpr] -> HsExp
application _ _  [] = error "nothing to apply"
application _ _  (CApply _ _:_) = error "application assertion violated" 
application _ qi [CSymbol ("Prelude","flip"),CSymbol (m,s),e]
  | isInfixName s = HsRightSection (qop qi m s) (expr True qi e)
application _ qi [CSymbol ("Prelude","enumFrom"),e] = 
  HsEnumFrom (expr False qi e)
application _ qi [CSymbol ("Prelude","enumFromTo"),e1,e2] = 
  HsEnumFromTo (expr False qi e1) (expr False qi e2)
application _ qi [CSymbol ("Prelude","enumFromThen"),e1,e2] = 
  HsEnumFromThen (expr False qi e1) (expr False qi e2)
application _ qi [CSymbol ("Prelude","enumFromThenTo"),e1,e2,e3] = 
  HsEnumFromThenTo (expr False qi e1) (expr False qi e2) (expr False qi e3)
application b qi xs@(sym@(CSymbol ("Prelude",s)):es)
  | s=="negate"       = maybePar b (negApp qi e es)
  | s=="if_then_else" = maybePar b (ite qi e es)
  | isTupleName s     = tuple b qi (length s - 1) e es
  | isClosedList xs   = let ys=listElems xs
                         in if   not (null ys) && all isChar ys 
                            then HsLit (HsString (map char ys))
                            else HsList (map (expr False qi) ys)
  where
    e = expr b qi sym
application b qi (sym@(CSymbol (m,s)):es)
  | isInfixName s       = maybePar b $ 
                          infi qi m s (expr True qi sym) (map (expr True qi) es)
application b qi (x:xs) = maybePar b $
                          foldl HsApp (expr True qi x) (map (expr True qi) xs)

negApp :: QualInfo -> HsExp -> [CExpr] -> HsExp
negApp _  s []     = s
negApp qi _ (e:es) = 
  foldl HsApp (HsNegApp (expr True qi e)) (map (expr True qi) es)

ite :: QualInfo -> HsExp -> [CExpr] -> HsExp
ite qi _ (e:true:false:es) = 
  foldl HsApp (maybePar (not (null es)) $ HsIf (ex e) (ex true) (ex false))
              (map (expr True qi) es)
  where
    ex = expr False qi
ite qi s es = foldl HsApp s (map (expr True qi) es)

tuple :: Bool -> QualInfo -> Int -> HsExp -> [CExpr] -> HsExp
tuple b qi i s es 
  | length es == i = HsTuple (map (expr False qi) es)
  | otherwise      = maybePar b $ foldl HsApp s (map (expr True qi) es)

isClosedList :: [CExpr] -> Bool
isClosedList [CSymbol (_,"[]")]     = True
isClosedList [CSymbol (_,":"),_,xs] = isClosedList (args xs)
isClosedList _                      = False

listElems :: [CExpr] -> [CExpr]
listElems [_]      = []
listElems [_,x,xs] = x:listElems (args xs)
listElems _        = error "unexpected call to listElems"

isChar :: CExpr -> Bool
isChar (CLit (CCharc _)) = True
isChar _                 = False

char :: CExpr -> Char
char (CLit (CCharc c)) = c
char _ = error "unexpected call to char"

infi :: QualInfo -> String -> String -> HsExp -> [HsExp] -> HsExp
infi _  _ _ e []         = e
infi qi m s _ [e']       = HsLeftSection e' (qop qi m s) 
infi qi m s _ (e1:e2:es) = 
  foldl HsApp (maybePar (not (null es)) (HsInfixApp e1 (qop qi m s) e2)) es

qop :: QualInfo -> String -> String -> HsQOp
qop qi m s 
  | isConsName s = HsQConOp (qual qi (m,s))
  | otherwise    = HsQVarOp (qual qi (m,s))
                            

args :: CExpr -> [CExpr]
args (CApply e1 e2) = args e1 ++ [e2]
args e              = [e]

maybePar :: Bool -> HsExp -> HsExp
maybePar True  = HsParen
maybePar False = id

statement :: QualInfo -> CStatement -> HsStmt
statement qi (CSExpr e)   = HsQualifier (expr False qi e)
statement qi (CSPat p e)  = 
  HsGenerator src (pattern False qi p) (expr False qi e)
statement qi (CSLet locs) = HsLetStmt (concatMap (loc qi) locs)

branch :: QualInfo -> CBranchExpr -> HsAlt
branch qi (CBranch p e) = 
  HsAlt src (pattern False qi p) (HsUnGuardedAlt (expr False qi e)) []

isConsName :: String -> Bool
isConsName "[]"    = True
isConsName (':':_) = True
isConsName n       = isTupleName n || isUpper (head n) 

loc :: QualInfo -> CLocalDecl -> [HsDecl]
loc qi (CLocalFunc f) = funcs qi f
loc qi (CLocalPat p e locs) = 
  [HsPatBind src (pattern False qi p) 
                 (HsUnGuardedRhs (expr False qi e)) 
                 (concatMap (loc qi) locs)]
loc qi (CLocalVar v) = loc qi (CLocalPat (CPVar v) unknown [])

unknown :: CExpr
unknown = CSymbol ("Prelude","unknown")

pattern :: Bool -> QualInfo -> CPattern -> HsPat
pattern _ _   (CPVar (_,"_")) = HsPWildCard
pattern _ _   (CPVar v) = HsPVar (var v)
pattern _ _   (CPLit l) = HsPLit (lit l)
pattern par qi p@(CPComb mn@(_,n) ps) 
  | isTupleName n   = HsPTuple (map (pattern False qi) ps)
  | isListPattern p = HsPList  (map (pattern False qi) (plistElems p))
  | isInfixName n &&
    length ps == 2  = let [p1,p2] = ps
                       in maybePPar par $
                          HsPInfixApp (addPParToInfix (pattern False qi p1))
                                      (qual qi mn) 
                                      (addPParToInfix (pattern False qi p2))
  | null ps   = HsPApp (qual qi mn) []
  | otherwise = maybePPar par (HsPApp (qual qi mn) (map (pattern True qi) ps))
pattern par qi (CPAs v p) = 
  HsPAsPat (var v) $ 
           (pattern True qi p)
pattern par qi (CPFuncComb mn ps) = pattern par qi (CPComb mn ps)


maybePPar :: Bool -> HsPat -> HsPat
maybePPar True p@(HsPApp _ _) = HsPParen p
maybePPar True p@(HsPInfixApp _ _ _) = HsPParen p
maybePPar _    p = p

addPParToInfix :: HsPat -> HsPat
addPParToInfix p@(HsPInfixApp _ _ _) = HsPParen p
addPParToInfix p                     = p


lit :: CLiteral -> HsLiteral
lit (CIntc i)   = HsInt i
lit (CFloatc f) = HsFrac f
lit (CCharc c)  = HsChar c

isListPattern :: CPattern -> Bool
isListPattern (CPComb (_,"[]") [])    = True
isListPattern (CPComb (_,":") [_,xs]) = isListPattern xs
isListPattern _                       = False

plistElems :: CPattern -> [CPattern]
plistElems (CPComb _ [])     = []
plistElems (CPComb _ [x,xs]) = x : plistElems xs
plistElems _ = error "unexpected call to plistElems"

isComplexPat :: CPattern -> Bool
isComplexPat (CPVar _)       = False
isComplexPat (CPLit _)       = False
isComplexPat (CPComb _ args) = not (null args)

ops :: COpDecl -> HsDecl
ops (COp (_,n) fix prec) = HsInfixDecl src (fixity fix) prec [op (ident n)]
  where
    fixity CInfixOp  = HsAssocNone
    fixity CInfixlOp = HsAssocLeft
    fixity CInfixrOp = HsAssocRight

    op = if isConsName n then HsConOp else HsVarOp


externals :: CurryProg -> String
externals (CurryProg _ _ _ fs _) = unlines (concatMap extName fs) 
  where
    extName (CFunc (_,n) _ _ _ (CExternal _)) 
      | isInfixName n = ["","("++n++") external"]
      | otherwise     = ["",n++" external"]
    extName _ = []


