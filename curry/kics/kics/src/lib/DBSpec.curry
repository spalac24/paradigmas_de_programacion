module DBSpec (

  DBValue(..), showDBValue, plainDBValue, 

  DBSpec, colNames, colTypes, readDB, showDB,

  bool, int, float, string, time, term, adapt,

  seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10

  ) where

import Time ( CalendarTime(..) )
import ReadShowTerm ( readQTerm, showQTerm )
import Unsafe ( isVar )

infixl 1 />=

data DBValue 
  = NULL | BOOLEAN Bool | INT Int | FLOAT Float
  | CLOB String | TIME CalendarTime

showDBValue :: DBValue -> String
showDBValue NULL = "NULL"
showDBValue (BOOLEAN b) = if b then "true" else "false"
showDBValue (INT n) = show n
showDBValue (FLOAT f) = show f
showDBValue (CLOB s) = "'" ++ concatMap quote s ++ "'"
 where
  quote c = (if special c then "\\" else []) ++ [c]
  special = (`elem`"\'\\")
showDBValue (TIME (CalendarTime y mo d h mi s z))
  = "'" ++ show y ++ "-" ++ show mo ++ "-" ++ show d ++ 
    " " ++ show h ++ ":" ++ show mi ++ ":" ++ show s ++
    "+" ++ show (z `div` 3600) ++ "'"
 where
  show n = if n < 10 then '0' : Prelude.show n else Prelude.show n

plainDBValue :: DBValue -> String
plainDBValue db
  = case db of
      CLOB s -> s
      _ -> showDBValue db

data DBSpec a = DBSpec Strings Strings (Reads a) (Shows a)
type Strings = [String] -> [String]
type Reads a = (String -> Bool) -> [DBValue] -> (a,[DBValue])
type Shows a = a -> (String -> Bool) -> [DBValue] -> [DBValue]

(/>=) :: Reads a -> (a -> Reads b) -> Reads b
rd />= f = \p xs -> case rd p xs of (a,ys) -> f a p ys

ret :: a -> Reads a
ret a _ xs = (a,xs)

names :: DBSpec _ -> Strings
names (DBSpec ns _ _ _) = ns

types :: DBSpec _ -> Strings
types (DBSpec _ ts _ _) = ts

reads :: DBSpec a -> Reads a
reads (DBSpec _ _ rd _) = rd

shows :: DBSpec a -> Shows a
shows (DBSpec _ _ _ sh) = sh

colNames :: DBSpec _ -> Strings
colNames = names

colTypes :: DBSpec _ -> Strings
colTypes = types

readDB :: DBSpec a -> Reads a
readDB = reads

showDB :: DBSpec a -> Shows a
showDB = shows

prim :: (a -> DBValue) -> String -> String -> DBSpec a
prim cons typ name = DBSpec ns ts rd sh
 where
  func a = cons a
  ns = (name:)
  ts = (typ:)
  rd p xs = if p name then rd' p xs else ret unknown p xs
  rd' p (NULL : xs) = ret unknown p xs
  rd' p (funca : xs) | func a =:= funca = ret a p xs where a free
  sh a p xs = if p name then sh' a xs else xs
  sh' a xs = (if isVar a then NULL else cons a) : xs

bool :: String -> DBSpec Bool
bool = prim BOOLEAN "BOOLEAN"

int :: String -> DBSpec Int
int = prim INT "INT"

float :: String -> DBSpec Float
float = prim FLOAT "FLOAT"

string :: String -> DBSpec String
string = prim CLOB "CLOB"

time :: String -> DBSpec CalendarTime
time = prim TIME "TIMESTAMP"

adapt :: (a->b,b->a) -> DBSpec a -> DBSpec b
adapt (a2b,b2a) (DBSpec ns ts rda sha) = DBSpec ns ts rd sh
 where
  rd = rda />= \a -> ret (if isVar a then unknown else a2b a)
  sh b = sha (if isVar b then unknown else b2a b)

term :: String -> DBSpec _
term = adapt (readQTerm,showQTerm) . string

seq2 :: (a -> b -> c) -> DBSpec a -> DBSpec b -> DBSpec c
seq2 cons ca cb = DBSpec ns ts rd sh
 where
  func a b = cons a b
  ns = names ca . names cb
  ts = types ca . types cb
  rd = reads ca />= \a ->
       reads cb />= \b ->
       ret (cons a b)
  sh ab p | ab =:= func a b = showDB ca a p . showDB cb b p where a,b free

seq3 cons ca cb cc = seq2 func ca (seq2 (,) cb cc)
 where
  func a (b,c) = cons a b c

seq4 cons ca cb cc cd = seq2 func (seq2 (,) ca cb) (seq2 (,) cc cd)
 where
  func (a,b) (c,d) = cons a b c d

seq5 cons ca cb cc cd ce = seq2 func (seq2 (,) ca cb) (seq3 (,,) cc cd ce)
 where
  func (a,b) (c,d,e) = cons a b c d e

seq6 cons ca cb cc cd ce cf
  = seq2 func (seq3 (,,) ca cb cc) (seq3 (,,) cd ce cf)
 where
  func (a,b,c) (d,e,f) = cons a b c d e f

seq7 cons ca cb cc cd ce cf cg
  = seq2 func (seq3 (,,) ca cb cc) (seq4 (,,,) cd ce cf cg)
 where
  func (a,b,c) (d,e,f,g) = cons a b c d e f g

seq8 cons ca cb cc cd ce cf cg ch
  = seq2 func (seq4 (,,,) ca cb cc cd) (seq4 (,,,) ce cf cg ch)
 where
  func (a,b,c,d) (e,f,g,h) = cons a b c d e f g h

seq9 cons ca cb cc cd ce cf cg ch ci
  = seq2 func (seq4 (,,,) ca cb cc cd) (seq5 (,,,,) ce cf cg ch ci)
 where
  func (a,b,c,d) (e,f,g,h,i) = cons a b c d e f g h i

seq10 cons ca cb cc cd ce cf cg ch ci cj
  = seq2 func (seq5 (,,,,) ca cb cc cd ce) (seq5 (,,,,) cf cg ch ci cj)
 where
  func (a,b,c,d,e) (f,g,h,i,j) = cons a b c d e f g h i j


