module PrettyStrict where

import Char
import FlatCurry
import FlatCurryGoodies
import List (nub)
import Maybe

prelude = "Prelude"
strict  = "StrictSteps"

text s = (s++)
char c = (c:)

arrow  = text "->"
bind   = text ">>="
bar    = char '|'
dcolon = text "::"

isInfixName :: QName -> Bool
isInfixName (_,n) = all (`elem` infixIDs) n

infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

isTupleName :: QName -> Bool
isTupleName (_,name) = elem (take 2 name) ["()","(,"]

type Doc = String -> String

empty, space, nl, dot, comma, semicolon, equals :: Doc
empty  = id
space  = char ' '
nl     = char '\n'
dot    = char '.'
comma  = char ','
semicolon = char ';'
equals = char '='

parens, bquotes, squotes, dquotes, brackets, curlies :: Doc -> Doc
parens d   = char '(' . d . char ')'
bquotes d  = char '`' . d . char '`'
squotes d  = char '\'' . d . char '\''
dquotes d  = char '"' . d . char '"'
brackets d = char '[' . d . char ']'
curlies  d = char '{' . d . char '}'

infixl 1 <>, <+>, <$>

(<$>), (<>), (<+>) :: Doc -> Doc -> Doc
a <$> b = a . char '\n' . b
a <>  b = a . b
a <+> b = a . char ' ' . b 

vcat, list :: [Doc] -> Doc
vcat = foldr (<$>) empty
list = brackets . sep comma

sh :: a -> Doc
sh x = text (show x)

int :: Int -> Doc
int = sh

float :: Float -> Doc
float = sh

sep _ [] = id
sep s xs@(_:_) = foldr1 (\ f g -> f . s . g) xs


printProg :: String -> IO ()
printProg fn = readFlatCurry fn >>= putStrLn . showProg ""

showProg :: String -> Prog -> String
showProg include prog = progDoc include prog ""

def :: Doc -> [Int] -> Doc -> Doc
def name params body = name <> paramDoc params <+> body
  
paramDoc params = if null params then empty 
                  else space <> sep space (map varDoc params)

app :: Doc -> [Doc] -> Doc
app d ds = if null ds then d
            else parens (sep space (d:ds))

qname :: Bool -> QName -> Doc
qname = qname' True

qname' :: Bool -> Bool -> QName -> Doc
qname' b fun mn@(mod,name)
  | name == "[]" || isTupleName mn = text name
  | name ==":" = identifier' b fun mn
  | isInfixName mn = parens (text mod <> dot <> identifier' False fun mn)
  | mod==""   = identifier' b fun mn
  | otherwise = text mod <> dot <> identifier' b fun mn

identifier :: Bool -> QName -> Doc
identifier = identifier' True

identifier' :: Bool -> Bool -> QName -> Doc
identifier' b fun n 
  | isInfixName n = (if b then parens else id) (text (snd (infixId n)))
  | fun           = text (snd (funId n))
  | otherwise     = text (snd (consId n))

funId :: QName -> QName
funId mn@(m,n@(h:_)) 
  | isUpper h = (m,'_':n)
  | otherwise = mn

consId :: QName -> QName
consId (m,n) = (m,toUpper (head n) : tail n)

infixId :: QName -> QName
infixId mn@(m,n@(h:_)) 
  | h==':' && n/=":" = (m,"%::"++n)
  | otherwise        = mn

progDoc :: String -> Prog -> Doc
progDoc include prog@(Prog name imps types funcs ops)
  = moduleHeaderDoc name (exportedNames prog) <$>
    impsDoc imps <+> text include <$> 
    opsDoc ops <$>
    typesDoc name types <$>
    funcsDoc name funcs

exportedNames :: Prog -> [Doc]
exportedNames (Prog _ _ types funcs _)
  = map typeExpDoc (filter ((Public==) . typeVisibility) types)
 ++ map (identifier True . funcName) (filter ((Public==) . funcVisibility) funcs)
 where
  typeExpDoc tdecl =
    let ecs = filter ((Public==) . consVisibility)
                     (trType (\_ _ _ cs -> cs) (\_ _ _ _ -> []) tdecl)
     in identifier False (typeName tdecl) <> if null ecs then empty else text "(..)"

moduleHeaderDoc :: String -> [Doc] -> Doc
moduleHeaderDoc name _ 
  = text "module" <+> text name <+> text "where"

exportsDoc :: [Doc] -> Doc
exportsDoc xs = parens (sep comma xs)

impsDoc :: [String] -> Doc
impsDoc imps = vcat (map ((text "import" <+>) . text) imps)

opsDoc :: [OpDecl] -> Doc
opsDoc ops = vcat (map opDoc ops)

opDoc :: OpDecl -> Doc
opDoc (Op mn@(_,name) fix prec)
  = text "infix" <> fixDoc fix <+> int prec <+> opNameDoc
 where
  fixDoc InfixOp  = empty
  fixDoc InfixlOp = char 'l'
  fixDoc InfixrOp = char 'r'
  opNameDoc = if all (`elem` infixIDs) name 
              then text (snd (infixId mn)) 
              else bquotes (text name)

typesDoc :: String -> [TypeDecl] -> Doc
typesDoc mod = vcat . map (typeDoc mod)

clearName :: String -> String
clearName s = case s of
  '(' : xs -> 'T':show (length xs)
  "[]"     -> "List"
  _        -> s

typeDoc ::String -> TypeDecl -> Doc
typeDoc mod (TypeSyn name _ params syn)
  = def (text "type" <+> identifier False name) 
        params
        (equals <+> typeExprDoc mod False syn)
typeDoc _ (Type name _ params _)
  = (text "newtype" <+> identifier False name) <> paramDoc params  <+> 
      equals <+> identifier False name  <+> text "StrictSteps.Data" <+>
    text "deriving" <+> text "Eq" <$> 
    instanceDoc "ShowTerm" name params <$>
      (text "    " <+> text "showCons"   <+> equals  <+> 
                       (text ("showCons" ++ clearName (snd name)))) <$> 
    instanceDoc "Convert" name params <$>
      (text "    " <+> 
         text "toData" <+> parens (identifier False name <+> text "x")  <+> 
           equals <+> text "x") <$>
      (text "    " <+> 
         text "fromData" <+> equals <+> identifier False name) 

instanceDoc :: String -> QName -> [Int] -> Doc
instanceDoc clas name params = 
    text "instance" <+> 
        classContextDoc clas params <+> 
        text clas <+>  
          (if length params > 0 then parens else id)  
          (qname False name <+> sep space (map varDoc params)) <+> 
    text "where"


varDoc :: Int -> Doc
varDoc i = if i>=0 then text ('x':show i) else text "y"

consDeclsDoc :: String -> [ConsDecl] -> Doc
consDeclsDoc mod cs
  = equals <+> sep (space . bar . space) (map (consDeclDoc mod) cs)
  

consDeclDoc :: String -> ConsDecl -> Doc
consDeclDoc mod (Cons name _ _ args)
  = app (identifier False name) (map (typeExprDoc mod True) args)

typeExprDoc :: String -> Bool -> TypeExpr -> Doc
typeExprDoc _ _ (TVar n) = varDoc n

typeExprDoc mod br (TCons name args)
  | null args = qname False name
  | name == (prelude,"[]") = brackets (typeExprDoc mod False (head args))
  | isTupleName name = parens (sep comma (map (typeExprDoc mod False) args))
  | otherwise 
    = par br $ app (qname False name) (map (typeExprDoc mod True) args)

typeExprDoc mod br typ@(FuncType _ _)
  = par br $ sep (space . arrow . space)
     (map (typeExprDoc mod True) (argTypes typ) ++ 
      [typeExprDoc mod False (resultType typ)])

par br = if br then parens else id

funcsDoc :: String -> [FuncDecl] -> Doc
funcsDoc mod funcs = vcat (map (funcDoc mod) funcs)

funcDoc :: String -> FuncDecl -> Doc
funcDoc mod (Func mn@(m,n) _ _ typ rule)
  | take 3 n == "_i_"
  = text "instance" <+> funcTypeDoc mod typ <+> text "where" <$>
    text "  " <+> ruleDoc mod (m,drop 3 n) rule
  | otherwise
  = funcTypeDeclDoc mod mn typ <$>
    ruleDoc mod mn rule

funcTypeDeclDoc :: String -> QName -> TypeExpr -> Doc
funcTypeDeclDoc mod name typ
  | typ==TVar (-42) = empty
  | otherwise
  = def (identifier True name <+> dcolon) [] (funcTypeDoc mod typ)

classContextDoc :: String -> [Int] -> Doc
classContextDoc _ [] = empty 
classContextDoc s vars@(_:_) = 
  parens (sep comma (map ((text s <+>) . varDoc) vars)) <+>  text "=>"

funcTypeDoc :: String -> TypeExpr -> Doc
funcTypeDoc mod typ
  = let vars = nub $ allVarsInTypeExpr typ 
        inst = classContextDoc "ShowTerm" vars 
        args = argTypes typ
        res =  resultType typ
     in inst <+> 
          (sep  (arrow . space)  
                (map ((<>space) . typeExprDoc mod True) (args++[res])))

ruleDoc :: String -> QName -> Rule -> Doc
ruleDoc mod name (Rule args body)
  = def (identifier True name) args (equals <+> expDoc mod False body)

ruleDoc _ name (External _)
  = identifier True name <+> text "external" -- <+> string decl

expDoc,expDoc2 :: String -> Bool -> Expr -> Doc
expDoc mod br exp =
  maybe (maybe (expDoc2 mod br exp)
               (list . (map (expDoc mod False)))
               (toList exp))
        (\ l -> if null l then brackets empty else dquotes (text l))
        (toString exp)

expDoc2 _ _ (Var n) = varDoc n
expDoc2 _ _ (Lit l) = litDoc l

expDoc2 mod br (Comb ct name args)
  | ct == FuncCall && name == (prelude,"apply")
    = par br $ app (expDoc mod True (args!!0)) [expDoc mod True (args!!1)]
  | name == (prelude,">>=") && length args == 3
    = case args of 
        [e1,Var i,e2] -> parens $ sep space
               [qname' False False name
               ,expDoc mod True e1
               ,parens (text "\\" <> varDoc i <> space <> text "->" <> space <> 
                        expDoc mod True e2)]
        _ -> error "unforseen use of (>>=)"
  | ct == ConsCall && isTupleName name
    = parens (sep comma (map (expDoc mod False) args))
   {-| isInfixName name && length args == 2
    = parens $ sep empty
               [expDoc mod True (args!!0) 
               ,space <> qname' False False name <> space
               ,expDoc mod True (args!!1)]-}
  | otherwise
    = parens $
       app (qname (isFunCT ct) name) (map (expDoc mod True) args)
 
  

expDoc2 mod _ (Let bs e)
  =  parens (text "let" <+> curlies (letBindsDoc mod bs) <+> 
             text "in" <+> expDoc mod False e)

expDoc2 mod br (Free vs e)
    = par br $ 
       text "let" <+> sep comma (map varDoc vs) <+>
       text "=free" <$> text "in" <+> expDoc mod False e

expDoc2 mod br (FlatCurry.Or e1 e2)
  = expDoc mod br (Comb FuncCall (prelude,"?") [e1,e2])

expDoc2 mod br (Case ct e bs)
  = par br $ 
     caseTypeDoc ct <> text "case" <+> expDoc mod False e <+> 
     text "of" <+> curlies (sep semicolon (map (branchDoc mod) bs))

branchDoc :: String -> BranchExpr -> Doc
branchDoc mod (Branch pat e)
  = def (patternDoc pat <+> arrow) [] (expDoc mod False e)

caseTypeDoc :: CaseType -> Doc
caseTypeDoc Rigid = empty
caseTypeDoc Flex  = empty

patternDoc :: Pattern -> Doc
patternDoc (Pattern name args)
  | null args = qname False name
  | isTupleName name = parens (sep comma (map varDoc args))
  | isInfixName name && length args == 2
    = varDoc (args!!0) <> text (snd (infixId name)) <> varDoc (args!!1)
  | otherwise = qname False name <+> sep space (map varDoc (take 4 args)) <+> 
                if length args < 4 
                then empty 
                else brackets (sep comma (map varDoc (drop 4 args)))

patternDoc (LPattern l) = litDoc l

letBindsDoc :: String -> [(Int,Expr)] -> Doc
letBindsDoc mod = sep semicolon . map (letBindDoc mod)

letBindDoc :: String -> (Int,Expr) -> Doc
letBindDoc mod (n,e) = varDoc n <+> equals <+> expDoc mod False e

litDoc :: Literal -> Doc
litDoc (Intc n)   = int n
litDoc (Floatc x) = float x
litDoc (Charc c)  = squotes (text (quoteChar c))

quoteChar c = maybe [c] id (lookup c specialChars)

-- more?
specialChars = [('\\',"\\\\"),('\n',"\\n"),('\r',"\\r"),('\t',"\\t"),('\'',"\\'"),('\"',"\\\"")]

toString :: Expr -> Maybe String
toString exp
  = case exp of
      Comb ConsCall (_,"[]") [] -> Just ""
      Comb ConsCall (_,":") [Lit (Charc c),cs] ->
        toString cs >>- Just . (quoteChar c++)
      _ -> Nothing

toList :: Expr -> Maybe [Expr]
toList exp
  = case exp of
      Comb ConsCall (_,"[]") [] -> Just []
      Comb ConsCall (_,":") [x,xs] -> toList xs >>- Just . (x:)
      _ -> Nothing

-- introduces over-applications on purpose!
elimApp :: Expr -> Expr
elimApp = updCombs elim
 where
  elim ct name args
    | ct == FuncCall && name == (prelude,"apply") && isComb (head args) &&
      combName (head args) /= (prelude,"apply")
      = extend (head args) (args!!1)
    | otherwise = Comb ct name args
  extend (Comb ct name args) arg = Comb ct name (args++[arg])

isFunCT :: CombType -> Bool
isFunCT x = case x of
              FuncCall -> True
              FuncPartCall _ -> True
              _ -> False
