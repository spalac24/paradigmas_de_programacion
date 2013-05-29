data Prim a = PrimValue a

data Success = Success

data Bool = True | False

data List a = List | C a (List a)

data Char = Char

data Exception = ErrorCall String
               | PatternMatchFail String
               | AssertionFailed String
               | IOException String

data Term = Free Int 
          | NumConstr  Int        [Term]
          | NameConstr String     [Term]
          | Constr     Int String [Term]

data IO a = IO a

data IOVal a = IOVal a

data Array a = Array a

