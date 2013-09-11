or:: Bool -> Bool -> Bool

or _ True = True
or True _ = True
or False False = False