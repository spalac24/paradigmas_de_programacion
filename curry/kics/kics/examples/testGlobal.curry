------------------------------------------------------------------------------
--- Some tests for library Global
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testGlobal"
---
--- @author Michael Hanus
--- @version June 2007
------------------------------------------------------------------------------

import Assertion -- for testing
import System(system)

import Global

points :: Global Int
points = global 1 Temporary

rwglobal = do
  v1 <- readGlobal points
  writeGlobal points 42
  v2 <- readGlobal points
  return (v1,v2)

test1 = AssertIO "simple int readGlobal/writeGlobal" rwglobal (1,42)


nats :: Global [Int]
nats = global [] Temporary

listrwglobal = do
  writeGlobal nats [1..5]
  v1 <- readGlobal nats
  writeGlobal nats (v1++v1)
  v2 <- readGlobal nats
  return (v1,v2)

test2 = AssertIO "simple intlist readGlobal/writeGlobal" listrwglobal
                 ([1..5],[1..5]++[1..5])


ppoints :: Global Int
ppoints = global 7 Temporary --(Persistent "pointsstore")

rwglobalp = do
  v1 <- readGlobal ppoints
  writeGlobal ppoints 42
  v2 <- readGlobal ppoints
  return (v1,v2)

test3 = AssertIO "persistent int readGlobal/writeGlobal" rwglobalp (7,42)


-- finalize: clean
--testFinal = AssertIO "clean up" (system "rm -r pointsstore*") 0

