module SQL (

  insert, delete, select, maybeAnd,

  SQLExp, Cond, OrdBy, constant, val, valStr,
  arg, neg, isNull, notNull, 
  isElem, isElemStr, 

  (.*), (./), (.+), (.-), (.*.), (./.), (.+.), (.-.),

  (.==), (./=), (.<), (.<=), (.>), (.>=), (.&&), (.||),

  alias, showSQLExp, renameCols,

  -- imported from JDBC
  close, startTransaction, commitTransaction, rollbackTransaction

  ) where

import Maybe
import ReadShowTerm

import DBSpec
import JDBC


infixl 7 .*, ./, .*., ./.
infixl 6 .+, .-, .+., .-.
infix  4 .==, ./=, .<, .<=, .>, .>=
infixr 3 .&&
infixr 2 .||


type Cond = Maybe (SQLExp Bool)
type OrdBy = Maybe (Int,String)

--- Insert values in a database table.
insert :: String -> [[DBValue]] -> IO ()
insert tab vs = update $ 
  "INSERT INTO " ++ tab ++ " VALUES " ++ 
    listWith "" "," "" (map (listWith "(" "," ")" . map showDBValue) vs)

--- Delete values from database table.
delete :: String -> [String] -> [DBValue] -> Cond -> IO ()
delete tab names vs cond = update $
  "DELETE FROM " ++ tab ++ 
  maybe "" ((" WHERE "++) . showSQLExp False) (maybeAnd cond cond')
 where
  cond' = if null eqs then Nothing else Just (foldr1 (.&&) eqs)
  eqs = concat (zipWith eq vs names)
  eq v n = if v == NULL then []
            else [argerrn n .== constant (showDBValue v)]

argerrn :: String -> SQLExp ()
argerrn n = arg (error "SQL.delete: table index was accessed!",n)

--- Select values from database table.
select :: [String] -> [[String]] -> Cond -> OrdBy -> IO [[DBValue]]
select tabs cols cond ordby = query $
  "SELECT " ++ listWith "" "," "" (zipWith namedCols [0..] cols) ++
  " FROM " ++ listWith "" "," "" (zipWith namedTab [0..] tabs) ++
  maybe "" ((" WHERE "++) . showSQLExp True) cond ++
  maybe "" (\ (tab,col) -> " ORDER BY tab" ++ show tab ++ "." ++ col) ordby
 where
  namedCols n cs = listWith "" "," "" (map ((alias n++".")++) cs)
  namedTab n t = t++" AS "++alias n


maybeAnd :: Cond -> Cond -> Cond
maybeAnd Nothing y = y
maybeAnd x@(Just _) Nothing = x
maybeAnd (Just x) (Just y) = Just (x .&& y)

listWith :: String -> String -> String -> [String] -> String
listWith pre inf post xs = pre ++ foldr1 (\x y -> x ++ inf ++ y) xs ++ post



data SQLExp _ = Exp Exp
data Exp
  = Const String
  | Column Int String
  | Unary PrePost UnOp Exp
  | Binary BinOp Exp Exp

data PrePost = Prefix | Postfix
data UnOp = Not | IsNull | NotNull
data BinOp = Mul | Div | NatDiv | Add | Sub | Eq | LT | LEq | And | Or


type CalcOp a = SQLExp a -> SQLExp a -> SQLExp a
type CompOp a = SQLExp a -> SQLExp a -> SQLExp Bool

wrap :: PrePost -> UnOp -> SQLExp _ -> SQLExp _
wrap fix unop (Exp e) = Exp (Unary fix unop e)

wrap2 :: BinOp -> SQLExp _ -> SQLExp _ -> SQLExp _
wrap2 binop (Exp e1) (Exp e2) = Exp (Binary binop e1 e2)


val :: a -> SQLExp a
val = constant . showQTerm

valStr :: String -> SQLExp String
valStr s = constant (showDBValue (CLOB s))

constant :: String -> SQLExp _
constant = Exp . Const

arg :: (Int,String) -> SQLExp _
arg = Exp . uncurry Column

neg :: SQLExp Bool -> SQLExp Bool
neg = wrap Prefix Not

isNull :: SQLExp _ -> SQLExp Bool
isNull = wrap Postfix IsNull

notNull :: SQLExp _ -> SQLExp Bool
notNull = wrap Postfix NotNull

isElem :: SQLExp a -> [a] -> SQLExp Bool
isElem x xs
  | null xs = val 1 .== val 2
  | otherwise = foldr1 (.||) (map ((x .==) . val) xs)

isElemStr :: SQLExp String -> [String] -> SQLExp Bool
isElemStr x xs
  | null xs = val 1 .== val 2
  | otherwise = foldr1 (.||) (map ((x .==) . valStr) xs)


(.*) :: CalcOp Int
(.*) = wrap2 Mul

(./) :: CalcOp Int
(./) = wrap2 NatDiv

(.+) :: CalcOp Int
(.+) = wrap2 Add

(.-) :: CalcOp Int
(.-) = wrap2 Sub

(.*.) :: CalcOp Float
(.*.) = wrap2 Mul

(./.) :: CalcOp Float
(./.) = wrap2 Div

(.+.) :: CalcOp Float
(.+.) = wrap2 Add

(.-.) :: CalcOp Float
(.-.) = wrap2 Sub

(.==) :: CompOp _
(.==) = wrap2 Eq

(./=) :: CompOp _
e1 ./= e2 = neg (e1 .== e2) 

(.<) :: CompOp _
(.<) = wrap2 LT

(.<=) :: CompOp _
(.<=) = wrap2 LEq

(.>) :: CompOp _
(.>) = flip (.<)

(.>=) :: CompOp _
(.>=) = flip (.<=)

(.&&) :: CalcOp Bool
(.&&) = wrap2 And

(.||) :: CalcOp Bool
(.||) = wrap2 Or


alias :: Int -> String
alias tab = "tab" ++ show tab


foldExp :: (String -> a)
        -> (Int -> String -> a)
        -> (PrePost -> UnOp -> a -> a)
        -> (BinOp -> a -> a -> a)
        -> Exp -> a
foldExp const _ _ _ (Const s) = const s
foldExp _ column _ _ (Column tab col) = column tab col
foldExp const column unary binary (Unary fix op e)
  = unary fix op (foldExp const column unary binary e)
foldExp const column unary binary (Binary op e1 e2) 
  = binary op (fold e1) (fold e2)
 where
  fold = foldExp const column unary binary


showSQLExp :: Bool -> SQLExp _ -> String
showSQLExp b (Exp exp) = foldExp (++) showsColumn showsUnary showsBinary exp ""
 where
  showsColumn tab col     = (if b then (alias tab++) . ('.':) else id). (col++)
  showsUnary Prefix  op e = ('(':) . (showUnOp op++) . (' ':) . e . (')':)
  showsUnary Postfix op e = ('(':) . e . (showUnOp op++) . (')':)
  showsBinary op e1 e2    = ('(':) . e1 . (showBinOp op++) . e2 . (')':)

showUnOp :: UnOp -> String
showUnOp Not = "NOT "
showUnOp IsNull = " IS NULL"
showUnOp NotNull = " IS NOT NULL"

showBinOp :: BinOp -> String
showBinOp Mul = " * "
showBinOp Div = " / "
showBinOp NatDiv = " DIV "
showBinOp Add = " + "
showBinOp Sub = " - "
showBinOp Eq = " = "
showBinOp LT = " < "
showBinOp LEq = " <= "
showBinOp And = " AND "
showBinOp Or = " OR "


renameCols :: (Int -> Int) -> SQLExp a -> SQLExp a
renameCols f (Exp exp) = Exp (foldExp Const rename Unary Binary exp)
 where
  rename tab col = Column (f tab) col


