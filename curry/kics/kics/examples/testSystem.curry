------------------------------------------------------------------------------
--- Some tests for library System
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testSystem"
--- 
--- @author Michael Hanus
--- @version December 2006
------------------------------------------------------------------------------

import Assertion
import System

-- Testing environment variable handling:

evar = "asd123"

testEnv1 = AssertIO "get undefined environ" (getEnviron evar) ""

testEnv2 = AssertIO "set environ"
                    (setEnviron evar "SET" >> getEnviron evar) "SET"

testEnv3 = AssertIO "unset environ"
                    (unsetEnviron evar >> getEnviron evar) ""

testEnv4 = AssertIO "set shell environ"
            (setEnviron evar "SET" >> system ("test \"$"++evar++"\" = SET")) 0
