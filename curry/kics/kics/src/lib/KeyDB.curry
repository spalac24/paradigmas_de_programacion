--- This module provides a general interface for databases
--- (persistent predicates) where each entry consists of a key and an info
--- part. The key is an integer and the info is arbitrary.
--- All functions are parameterized with a dynamic predicate that
--- takes an integer key as a first parameter.
---
--- @author Bernd Brassel, Michael Hanus
--- @version Fri Nov 12 15:22:54 MET 2004

module KeyDB(existsDBKey,allDBKeys,getDBInfo,
             deleteDBEntry,updateDBEntry,newDBEntry,cleanDB) where

import Dynamic
import Integer(maxlist)

--- Exists an entry with a given key in the database?
--- @param db - the database (a dynamic predicate)
--- @param key - a key (an integer)
existsDBKey :: (Int -> _ -> Dynamic) -> Int -> IO Bool
existsDBKey db key = seq db $ seq key $ do
  entries <- getDynamicSolution (\info -> db key info)
  return (entries /= Nothing)

--- Returns all keys of entries in the database.
allDBKeys :: (Int -> _ -> Dynamic) -> IO [Int]
allDBKeys db = seq db $ do
  getDynamicSolutions (\key -> let info free in db key info)

--- Gets the information about an entry in the database.
--- @param db - the database (a dynamic predicate)
--- @param key - the key of the entry (an integer)
getDBInfo :: (Int -> a -> Dynamic) -> Int -> IO a
getDBInfo db key = seq db $ seq key $ do
  entries <- getDynamicSolutions (\info -> db key info)
  if null entries
   then error ("getDBInfo: no entry for key '"++show key++"'")
   else return (head entries)

--- Deletes an entry with a given key in the database.
--- @param db - the database (a dynamic predicate)
--- @param key - the key of the entry (an integer)
deleteDBEntry :: (Int -> _ -> Dynamic) -> Int -> IO ()
deleteDBEntry db key = seq db $ seq key $ do
  entries <- getDynamicSolutions (\infos -> db key infos)
  mapIO_ (\infos -> retract (db key infos)) entries

--- Overwrites an existing entry in the database.
--- @param db - the database (a dynamic predicate)
--- @param key - the key of the entry (an integer)
--- @param info - the information to be stored in the updated entry
updateDBEntry :: (Int -> a -> Dynamic) -> Int -> a -> IO ()
updateDBEntry db key info = do
  deleteDBEntry db key
  assert (db key info)

--- Return a new database key.
newDBKey :: (Int -> _ -> Dynamic) -> IO Int
newDBKey db = do
  ids <- getDynamicSolutions (\i -> let info free in db i info)
  return (if null ids then 1 else maxlist ids + 1)

--- Stores a new entry in the database and return the key of the new entry.
--- @param db - the database (a dynamic predicate)
--- @param info - the information to be stored in the new entry
newDBEntry :: (Int -> a -> Dynamic) -> a -> IO Int
newDBEntry db info = do
  i <- newDBKey db
  assert (db i info)
  return i

--- Deletes all entries in the database.
cleanDB :: (Int -> _ -> Dynamic) -> IO ()
cleanDB db = do
  ids <- getDynamicSolutions (\i -> let info free in db i info)
  mapIO_ (\i -> deleteDBEntry db i) ids
