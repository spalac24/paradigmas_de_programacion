data Success = Success

data Bool = False | True 

--data List a = List | a :< (List a)

data Char = Char 

--data Exception = ErrorCall String
--               | PatternMatchFail String
--               | AssertionFailed String
--               | IOException String

--data Data = Free Int
--          | Data Int String [Data]


data Prim a = PrimValue a

(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

data Four = F0 | F1 | F2 | F3

