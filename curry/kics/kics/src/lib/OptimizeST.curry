module OptimizeST (
  printInternalST, 
  getGarbageCount,

  optimizeST

  ) where



--------------------------------------------------------
-- functions to analyze how bad the problem is
--------------------------------------------------------

-- print a simple textual presentation of the internal
-- data structure
printInternalST :: a -> IO ()
printInternalST x = putStrLn (showSearchTree x)

-- count ornodes (necessary,superfluous)
-- this gives an idea how good optimization would work
getGarbageCount :: a -> IO (Int,Int)
getGarbageCount x = return (countGarbage x)


---------------------------------------
-- optimizing the internal structure
---------------------------------------

optimizeST :: a -> a
optimizeST external

------------------------------
-- internal functions
------------------------------

showSearchTree :: a -> String
showSearchTree external

countGarbage :: a -> (Int,Int)
countGarbage external