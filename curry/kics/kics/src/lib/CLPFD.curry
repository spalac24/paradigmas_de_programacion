------------------------------------------------------------------------------
--- Library for finite domain constraint solving.
--- <P>
--- The general structure of a specification of an FD problem is as follows:
--- 
--- <CODE>domain_constraint & fd_constraint & labeling</CODE>
--- 
--- where:
--- 
--- <CODE>domain_constraint</CODE>
--- specifies the possible range of the FD variables (see constraint "domain")
--- 
--- <CODE>fd_constraint</CODE>
--- specifies the constraint to be satisfied by a valid solution
--- (see constraints #+, #-, all_different, etc below)
--- 
--- <CODE>labeling</CODE>
--- is a labeling function to search for a concrete solution
------------------------------------------------------------------------------

module CLPFD(domain, (+#), (-#), (*#), (=#), (/=#), (<#), (<=#), (>#), (>=#),
             all_different, labeling) where

-- The operator declarations are similar to the standard arithmetic operators.

infixl 7 *#
infixl 6 +#, -#
infix  4 =#, /=#, <#, <=#, >#, >=#



--- Constraint to specify the domain of all finite domain variables.
--- @param xs - list of finite domain variables
--- @param min - minimum value for all variables in xs
--- @param max - maximum value for all variables in xs

domain :: [Int] -> Int -> Int -> Success
domain vs l u =  ((prim_domain $!! (ensureSpine vs)) $# l) $# u

prim_domain :: [Int] -> Int -> Int -> Success
prim_domain external

--- Addition of FD variables.

(+#)   :: Int -> Int -> Int
(+#) external

--- Subtraction of FD variables.

(-#)   :: Int -> Int -> Int
(-#) external

--- Multiplication of FD variables.

(*#)   :: Int -> Int -> Int
(*#) external

--- Equality of FD variables.

(=#)   :: Int -> Int -> Success
(=#) external

--- Disequality of FD variables.

(/=#)  :: Int -> Int -> Success
(/=#) external

--- "Less than" constraint on FD variables.

(<#)   :: Int -> Int -> Success
(<#) external

--- "Less than or equal" constraint on FD variables.

(<=#)  :: Int -> Int -> Success
(<=#) external

--- "Greater than" constraint on FD variables.

(>#)   :: Int -> Int -> Success
(>#) external

--- "Greater than or equal" constraint on FD variables.

(>=#)  :: Int -> Int -> Success
(>=#) external

--- "All different" constraint on FD variables.
--- @param xs - list of FD variables
--- @return satisfied if the FD variables in the argument list xs
---         have pairwise different values.

all_different :: [Int] -> Success
all_different vs = prim_all_different $!! (ensureSpine vs)

prim_all_different :: [Int] -> Success
prim_all_different external

--- Instantiate FD variables to their values in the specified domain.
--- @param xs - list of FD variables that are non-deterministically
---             instantiated to their possible values.

labeling :: [Int] -> Success
labeling vs = prim_labeling $!! (ensureSpine vs)

prim_labeling :: [Int] -> Success
prim_labeling external

-- end of program
