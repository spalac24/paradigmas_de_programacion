------------------------------------------------------------------------------
--- This module contains a collection of functions for
--- obtaining lists of solutions to constraints.
--- These operations are useful to encapsulate
--- non-deterministic operations between I/O actions in
--- order to connects the worlds of logic and functional programming
--- and to avoid non-determinism failures on the I/O level.
---
--- In contrast the "old" concept of encapsulated search
--- (which could be applied to any subexpression in a computation),
--- the operations to encapsulate search in this module
--- are I/O actions in order to avoid some anomalities
--- in the old concept.
------------------------------------------------------------------------------

module AllSolutions(getAllSolutions,getAllValues,getFirstSolution,
                    getAllFailures) where

--- Gets all solutions to a constraint (currently, via an incomplete
--- depth-first left-to-right strategy). Conceptually, all solutions
--- are computed on a copy of the constraint, i.e., the evaluation
--- of the constraint does not share any results. Moreover, this
--- evaluation suspends if the constraints contain unbound variables.
--- Similar to Prolog's findall.
getAllSolutions :: (a->Success) -> IO [a]
getAllSolutions c = getAllValues (let x free in (x,c x)) >>= return . map fst

--- Gets all values of an expression. Since this is based on
--- <code>getAllSolutions</code>, it inherits the same restrictions.
getAllValues :: a -> IO [a]
getAllValues x = getSearchTree x >>= return . allValuesD

--- Gets the first solution (currently, via an incomplete
--- left-to-right strategy). Returns Nothing if the search space
--- is finitely failed.
getFirstSolution :: (a->Success) -> IO (Maybe a)
getFirstSolution c =
 do sols <- getAllSolutions c
    return (if null sols then Nothing else Just (head sols))


--- Returns a list of values that do not satisfy a given constraint.
--- @param x - an expression (a generator evaluable to various values)
--- @param c - a constraint that should not be satisfied
--- @return A list of all values of e such that (c e) is not provable
getAllFailures :: a -> (a->Success) -> IO [a]
getAllFailures generator test =
 do xs <- getAllSolutions (=:=generator)
    failures <- mapIO (naf test) xs
    return $ concat failures

  -- (naf c x) returns [x] if (c x) fails, and [] otherwise.
naf :: (a->Success) -> a -> IO [a]
naf c x = getFirstSolution (lambda c x) >>= returner x

lambda :: (a->Success) -> a -> b -> Success
lambda c x _ = c x

returner :: a -> Maybe b -> IO [a]
returner x mbl = return (maybe [x] (const []) mbl)


