--- Library for dynamic predicates.
---
--- Currently, it is still experimental so that its interface might
--- be slightly changed in the future.
--- <p>
--- A dynamic predicate <code>p</code> with arguments of type
--- <code>t1,...,tn</code> must be declared by:
--- <pre>
--- p :: t1 -> ... -> tn -> Dynamic
--- p = dynamic
--- </pre>
---
--- A dynamic predicate where all facts should be persistently stored
--- in the directory DIR must be declared by:
--- <pre>
--- p :: t1 -> ... -> tn -> Dynamic
--- p = persistent "file:DIR"
--- </pre>
--- </p>
---
--- @author Michael Hanus, Sebastian Fischer
--- @version May 2006
module Dynamic (

  Dynamic, (<>), (|>), (|&>), (.|>), (!), Dyn, (.&>), orderBy,

  persistent1, persistent2, persistent3, persistent4, persistent5,

  assert, retract, getDynamicValues, getDynamicSolutions, getDynamicSolution,

  transaction, abortTransaction,

  -- imported from JDBC
  close,

  -- imported from DBSpec
  DBSpec, bool, int, float, string, time, term, adapt,

  seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10,

  -- imported from SQL
  SQLExp, val, valStr, arg, neg, isNull, notNull, isElem, isElemStr,

  (.*), (./), (.+), (.-), (.*.), (./.), (.+.), (.-.),

  (.==), (./=), (.<), (.<=), (.>), (.>=), (.&&), (.||)

  ) where

import Meta
import List hiding ( delete )
import Sort ( leqString )
import Maybe

import DBSpec
import SQL


infixr 2 <>
infixl 1 |>, .|>, |&>, !
infix  0 .&>

----------------------------------------------------------------------

data Dynamic 
  = Dynamic DynSpec                             -- primitive
  | Compound [Dynamic] [DBPred] Bool Cond OrdBy -- compound 

-- The behavior specification of a dynamic predicate (only internally used).
data DynSpec = Temporary  -- a dynamic predicate that exists only during
                          -- a single execution of a program
             | Persistent -- a persistent dynamic predicate

-- Specification of a database predicate
data DBPred 
  = DB String                                        -- table name
       ([String] -> [String])                        -- column names
       ([String] -> [String])                        -- column types
       ((String -> Bool) -> [DBValue] -> [DBValue])  -- reads cols
       ((String -> Bool) -> [DBValue] -> [DBValue])  -- shows cols

table :: DBPred -> String
table (DB tab _ _ _ _) = tab

names :: DBPred -> [String] -> [String]
names (DB _ ns _ _ _) = ns

types :: DBPred -> [String] -> [String]
types (DB _ _ ts _ _) = ts

allNames :: [DBPred] -> [[String]]
allNames dbs = map (($[]) . names) dbs

reads :: DBPred -> (String -> Bool) -> [DBValue] -> [DBValue]
reads (DB _ _ _ rd _) = rd

readsAll :: [DBPred] -> (String -> Bool) -> [DBValue] -> [DBValue]
readsAll [] _ = id
readsAll (db:dbs) p = readsAll dbs p . reads db p

leqSpec :: DBPred -> DBPred -> Bool
leqSpec (DB t _ _ _ _) (DB t' _ _ _ _) = leqString t t'

insertDBPred :: DBPred -> [DBPred] -> [DBPred]
insertDBPred db [] = [db]
insertDBPred db (d:ds)
  | leqSpec db d = db:d:ds
  | otherwise    = d : insertDBPred db ds

sortDBPreds :: [DBPred] -> [DBPred]
sortDBPreds = foldr insertDBPred []

readsV :: DBSpec a -> a -> (String -> Bool) -> [DBValue] -> [DBValue]
readsV spec x p xs | x =:= y = ys
 where
  (y,ys) = readDB spec p xs

persistentDB tab ns ts rd sh 
  = Compound [] [DB tab ns ts rd sh] True Nothing Nothing

persistent1 :: String -> DBSpec a -> a -> Dynamic
persistent1 tab sa a 
  = persistentDB tab
     (colNames sa) (colTypes sa) (readsV sa a) (showDB sa a)

persistent2 :: String -> DBSpec a -> DBSpec b -> a -> b -> Dynamic
persistent2 tab sa sb a b
  = persistentDB tab 
     (colNames sa . colNames sb)
     (colTypes sa . colTypes sb)
     (\p -> readsV sb b p . readsV sa a p)
     (\p -> showDB sa a p . showDB sb b p)

persistent3 
  :: String -> DBSpec a -> DBSpec b -> DBSpec c
  -> a -> b -> c -> Dynamic
persistent3 tab sa sb sc a b c
  = persistentDB tab
     (colNames sa . colNames sb . colNames sc)
     (colTypes sa . colTypes sb . colTypes sc)
     (\p -> readsV sc c p . readsV sb b p . readsV sa a p)
     (\p -> showDB sa a p . showDB sb b p . showDB sc c p)

persistent4 
  :: String -> DBSpec a -> DBSpec b -> DBSpec c -> DBSpec d
  -> a -> b -> c -> d -> Dynamic
persistent4 tab sa sb sc sd a b c d
  = persistentDB tab
     (colNames sa . colNames sb . colNames sc . colNames sd)
     (colTypes sa . colTypes sb . colTypes sc . colTypes sd)
     (\p -> readsV sd d p . readsV sc c p . readsV sb b p . readsV sa a p)
     (\p -> showDB sa a p . showDB sb b p . showDB sc c p . showDB sd d p)

persistent5 
  :: String -> DBSpec a -> DBSpec b -> DBSpec c -> DBSpec d -> DBSpec e
  -> a -> b -> c -> d -> e -> Dynamic
persistent5 tab sa sb sc sd se a b c d e
  = persistentDB tab
     (colNames sa . colNames sb . colNames sc . colNames sd . colNames se)
     (colTypes sa . colTypes sb . colTypes sc . colTypes sd . colTypes se)
     (\p -> readsV sd d p . readsV sc c p . readsV sb b p . readsV sa a p
          . readsV se e p)
     (\p -> showDB sa a p . showDB sb b p . showDB sc c p . showDB sd d p
          . showDB se e p)

----------------------------------------------------------------------

--- Combine two dynamics.
(<>) :: Dynamic -> Dynamic -> Dynamic
d1@(Dynamic _) <> d2@(Dynamic _) = Compound [d1,d2] [] True Nothing Nothing
d@(Dynamic _) <> (Compound ds dbs b c o) = Compound (d:ds) dbs b c o
(Compound ds dbs b c o) <> d@(Dynamic _) = Compound (d:ds) dbs b c o
(Compound ds1 dbs1 b1 c1 o1) <> (Compound ds2 dbs2 b2 c2 o2)
  = Compound (ds1++ds2) (dbs1++dbs2) (b1&&b2) (maybeAnd c1 c2') (combOrd o1 o2)
 where
  c2' = c2 >>- Just . renameCols (+length dbs1)

  combOrd Nothing Nothing = Nothing
  combOrd Nothing (Just (t,c)) = Just (t+length dbs1,c)
  combOrd (Just (t,c)) Nothing = Just (t,c)
  combOrd (Just (t,c)) (Just (t',c')) = 
    error $ "ambigious ORDER BY specification: tab" ++ show t ++ "." ++ c
         ++ ", tab" ++ show t' ++ "." ++ c'

--- Restrict a dynamic with a condition.
(|>) :: Dynamic -> Bool -> Dynamic
d@(Dynamic _) |> b = Compound [d] [] b Nothing Nothing
(Compound ds dbs b1 c o) |> b2 = Compound ds dbs (b1&&b2) c o

(.|>) :: Dynamic -> SQLExp Bool -> Dynamic
d@(Dynamic _) .|> _ = d
(Compound ds dbs b c1 o) .|> c2
  = Compound ds dbs b (maybe (Just c2) (Just . (.&&c2)) c1) o

--- Restrict a dynamic with a constraint.
(|&>) :: Dynamic -> Success -> Dynamic
d |&> c = d |> (c &> True)

--- Pojecting database predicates
(!) :: Dynamic -> [(Int,[String])] -> Dynamic
d@(Dynamic _) ! _ = d
d@(Compound ds dbs b cond o) ! ps
  | null dbs  = d
  | otherwise = Compound ds (project 0 dbs) b cond (updOrd o)
 where
  project _ []     = []
  project n (DB tab ns ts rd sh : xs)
    = DB tab (filter (proj n) . ns) ts rd sh : project (n+1) xs

  proj n x = maybe False (elem x) (lookup n ps)

  updOrd Nothing = Nothing
  updOrd (Just (t,c))
    | t `elem` ts = Just (length (filter (<=t) ts),c)
    | otherwise = error "ORDER BY for unused table not implemented"
   where ts = map fst ps

orderBy :: Dynamic -> (Int,String) -> Dynamic
orderBy d@(Dynamic _) _ = d
orderBy (Compound ds dbs b cond _) oby = Compound ds dbs b cond (Just oby)

--- Asserts new facts (without free variables!) about dynamic predicates.
--- Conditional dynamics are asserted only if the condition holds.
assert :: Dynamic -> IO ()
assert (Compound _ dbs b _ _)
  = if b then assertDB dbs else done

assertDB :: [DBPred] -> IO ()
assertDB = mapIO_ assertTab . groupBy sameTab . sortDBPreds
 where
  sameTab (DB t1 _ _ _ _) (DB t2 _ _ _ _) = t1==t2

assertTab :: [DBPred] -> IO ()
assertTab ds@(DB tab _ _ _ _ : _)
  = insert tab (map values ds)
 where
  values (DB _ _ _ _ sh) = sh (const True) []

--- Deletes facts (without free variables!) about dynamic predicates.
--- Conditional dynamics are retracted only if the condition holds.
--- Returns True if all facts to be retracted exist,
--- otherwise False is returned.
retract :: Dynamic -> IO ()
retract (Compound _ dbs b cond _)
  = if b then mapIO_ (retractDB cond) dbs else done

retractDB :: Cond -> DBPred -> IO ()
retractDB cond (DB tab ns _ _ sh)
  = delete tab xs (sh (`elem` xs) []) cond
 where xs = ns []

data Dyn a = Dyn Dynamic a

(.&>) :: Dynamic -> a -> Dyn a
(.&>) = Dyn

getDynamicSolution :: (a -> Dynamic) -> IO (Maybe a)
getDynamicSolution p = do
  sols <- getDynamicSolutions p
  return (if null sols then Nothing else Just (head sols))

getDynamicSolutions :: (a -> Dynamic) -> IO [a]
getDynamicSolutions p = getDynamicValues (let x free in p x .&> x)

getDynamicValues :: Dyn a -> IO [a]
getDynamicValues (Dyn (Compound _ dbs b cond oby) x) = do
  rs <- select (map table dbs) (allNames dbs) cond oby
  return (concatMap readRow rs)
 where
  readRow cs = allValuesD $ searchTree
    ((b =:= True & [] =:= readsAll dbs (`elem` concat (allNames dbs)) cs) &> x)

--- Perform an action (usually containing updates of various
--- dynamic predicates) as a single transaction.
--- This is the preferred way to execute any changes to persistent
--- dynamic predicates if there might be more than one process
--- that may modify the definition of such predicates in parallel.
---
--- Before the transaction is executed, the access to all persistent
--- predicates is locked (i.e., no other process can perform a
--- transaction in parallel).
--- After the successful transaction, the access is unlocked so that
--- the updates performed in this transaction become persistent and
--- visible to other processes.
--- Otherwise (i.e., in case of a failure or abort of the transaction),
--- the changes of the transaction to persistent predicates are
--- ignored and Nothing is returned.
---
--- In general, a transaction should terminate and all failures inside
--- a transaction should be handled (execept for abortTransaction).
--- If a transaction is externally interrupted (e.g., by killing the process),
--- some locks might never be removed. However, they
--- can be explicitly removed by deleting the corresponding lock files
--- reported at startup time.
---
--- Nested transactions are not supported and lead to a failure.
transaction :: IO a -> IO (Maybe a)
transaction action = do
  SQL.startTransaction
  catchFail performTrans
            (catchFail (rollbackTransaction >> abortTransaction)
            -- we perform an explicit abort in case of run time errors
                       (return Nothing))
 where
  performTrans = do
    result <- action
    SQL.commitTransaction
    return (Just result)

--- Aborts the current transaction. If a transaction is aborted,
--- the remaining actions of the transaction are not executed
--- and all changes to <b>persistent</b> dynamic predicates
--- made in this transaction are ignored.
abortTransaction :: IO _
abortTransaction = failed


