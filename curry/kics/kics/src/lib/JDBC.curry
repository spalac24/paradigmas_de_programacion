--- Provides access to a JDBC database proxy over ports.
---
--- @author Sebastian Fischer
module JDBC (

  query, update, close, 
  startTransaction, commitTransaction, rollbackTransaction
 
  ) where

import IO
import Distribution ( getRcVar )
import ReadShowTerm ( readQTerm, showQTerm )
import ReadNumeric ( readInt )
import Socket ( connectToSocket )
import Global

-- Provides datatype for column values
import DBSpec

-- To retrieve results lazily
import Unsafe ( unsafePerformIO )

type DBHandle = Int
type ResultHandle = Int

getDBProxyName :: IO String
getDBProxyName = getRcVar "DBProxyName" >>= maybe (return "localhost") return

getDBProxyPort :: IO Int
getDBProxyPort =
  getRcVar "DBProxyPort" >>= maybe def (maybe def (return . fst) . readInt)
 where
  def = return 28779

--- messages sent to the database proxy
data Request
  = Open
  | Update            DBHandle  String
  | Query             DBHandle  String
  | EndOfResults      DBHandle  ResultHandle
  | NextRow           DBHandle  ResultHandle
  | StartTransaction  DBHandle
  | Commit            DBHandle
  | Rollback          DBHandle
  | Close             DBHandle

showRequest :: Request -> String
showRequest req =
  case req of
    Open -> "Open"
    _ -> let s = show req in if head s == '(' then s else "(" ++ s ++ ")"

--- messages received from the database proxy
data Response
  = Error String | Ok | Yes | No | Row [DBValue] | DBHandle DBHandle
  | ResultHandle ResultHandle

request :: Handle -> Request -> IO Response
request h req = do
  hPutStrLn h (showRequest req) >> hFlush h
  response <- hGetLine h >>= return . readQTerm
  case response of
    Error msg -> error msg
    _ -> return response

type Connection = (Handle, DBHandle)

connection :: Global (Maybe Connection)
connection = global Nothing Temporary

getConnection :: IO Connection
getConnection = readGlobal connection >>= maybe create return
 where
  create = do
    proxy <- getDBProxyName
    port <- getDBProxyPort
    handle <- connectToSocket proxy port
    DBHandle db <- request handle Open
    let conn = (handle,db)
    writeGlobal connection (Just conn)
    return conn

-----------------------------------------------------------------------------

--- Sends an SQL query and lazily returns the results.
---
--- @param sql SQL query
query :: String -> IO [[DBValue]]
query sql = do
  conn@(h,db) <- getConnection
  ResultHandle res <- request h (Query db sql)
  return (lazyResults conn res)

-- Unsafe function that reads results of a query lazily.
lazyResults :: Connection -> ResultHandle -> [[DBValue]]
lazyResults conn res = unsafePerformIO $ do
  end <- endOfResults conn res
  if end then return [] else do
    row <- nextRow conn res
    return (row : lazyResults conn res)

-- Is result set empty?
endOfResults :: Connection -> ResultHandle -> IO Bool
endOfResults (h,db) res = do
  end <- request h (EndOfResults db res)
  case end of
    Yes -> return True
    No  -> return False

-- Get next row out of the result set.
nextRow :: Connection -> ResultHandle -> IO [DBValue]
nextRow (h,db) res = do
  Row row <- request h (NextRow db res)
  return row

--- Sends an SQL statement that does not return results.
---
--- @param sql SQL statement
update :: String -> IO ()
update sql = do
  (h,db) <- getConnection
  request h (Update db sql)
  done

--- Closes the database session of this process.
close :: IO ()
close = readGlobal connection >>= maybe done closeConnection
 where
  closeConnection (h,db) = do
    request h (Close db)
    writeGlobal connection Nothing

-- Transaction Support

-- Is current process in transaction?
isTransaction :: Global Bool
isTransaction = global False Temporary

--- Starts a transaction.
startTransaction :: IO ()
startTransaction = do
  trans <- readGlobal isTransaction
  if not trans
   then getConnection >>= startTrans
   else error "nested transactions not supported!"
 where
  startTrans (h,db) = do
    request h (StartTransaction db)
    writeGlobal isTransaction True

--- Commits a transaction.
commitTransaction :: IO ()
commitTransaction = do
  trans <- readGlobal isTransaction
  if trans 
   then getConnection >>= commitTrans
   else error "no transaction to commit!"
 where
  commitTrans (h,db) = do
    request h (Commit db)
    writeGlobal isTransaction False

--- Aborts a transaction.
rollbackTransaction :: IO ()
rollbackTransaction = do
  trans <- readGlobal isTransaction
  if trans
   then  getConnection >>= rollbackTrans
   else error "no transaction to rollback"
 where
  rollbackTrans (h,db) = do
    request h (Rollback db)
    writeGlobal isTransaction False



-- are transactions properly implemented?

transTest = do
  startTransaction
  getLine
  [[INT n]] <- query "select max(test) from test"
  getLine
  update $ "insert into test values (" ++ show (n+1) ++ ")"
  getLine
  commitTransaction
  getLine
  query "select * from test" >>= mapIO_ print
  close

