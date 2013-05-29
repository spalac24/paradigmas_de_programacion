------------------------------------------------------------------------------
--- Library for constraint programming with arithmetic constraints over reals.
------------------------------------------------------------------------------

module CLPR where

-- The operator declarations are similar to the standard arithmetic operators.

infixl 7 *., /.
infixl 6 +., -.
infix  4 <., >., <=., >=.


--- Addition on floats in arithmetic constraints.

(+.)   :: Float -> Float -> Float
(+.) external

--- Subtraction on floats in arithmetic constraints.

(-.)   :: Float -> Float -> Float
(-.) external

--- Multiplication on floats in arithmetic constraints.

(*.)   :: Float -> Float -> Float
(*.) external

--- Division on floats in arithmetic constraints.

(/.)   :: Float -> Float -> Float
(/.) external

--- "Less than" constraint on floats.

(<.)   :: Float -> Float -> Success
(<.) external

--- "Greater than" constraint on floats.

(>.)   :: Float -> Float -> Success
(>.) external

--- "Less than or equal" constraint on floats.

(<=.)  :: Float -> Float -> Success
(<=.) external

--- "Greater than or equal" constraint on floats.

(>=.)  :: Float -> Float -> Success
(>=.) external

--- Conversion function from integers to floats.
--- Rigid in the first argument, i.e., suspends until the first argument
--- is ground.

i2f    :: Int -> Float
i2f external


--- Computes the minimum with respect to a given constraint.
--- (minimumFor g f) evaluates to x if (g x) is satisfied and
--- (f x) is minimal. The evaluation fails if such a minimal value
--- does not exist. The evaluation suspends if it contains
--- unbound non-local variables.

minimumFor :: (a -> Success) -> (a -> Float) -> a
minimumFor external

--- Minimization constraint.
--- (minimize g f x) is satisfied if (g x) is satisfied and
--- (f x) is minimal. The evaluation suspends if it contains
--- unbound non-local variables.

minimize :: (a -> Success) -> (a -> Float) -> a -> Success
minimize g f x = minimumFor g f =:= x

--- Computes the maximum with respect to a given constraint.
--- (maximumFor g f) evaluates to x if (g x) is satisfied and
--- (f x) is maximal. The evaluation fails if such a maximal value
--- does not exist. The evaluation suspends if it contains
--- unbound non-local variables.

maximumFor :: (a -> Success) -> (a -> Float) -> a
maximumFor external

--- Maximization constraint.
--- (maximize g f x) is satisfied if (g x) is satisfied and
--- (f x) is maximal. The evaluation suspends if it contains
--- unbound non-local variables.

maximize :: (a -> Success) -> (a -> Float) -> a -> Success
maximize g f x = maximumFor g f =:= x
