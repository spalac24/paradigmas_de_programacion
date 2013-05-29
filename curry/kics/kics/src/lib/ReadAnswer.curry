module ReadAnswer (readAnswer, stdinUnbuffered, stdinBuffered) where

--- readAnswer prints the first argument, 
--- waits for a key to be pressed, deletes the printed characters again
--- and returns pressed character
--- (does not work in emacs shell)

readAnswer :: String -> IO Char
readAnswer s = prim_readAnswer $## s


prim_readAnswer :: String -> IO Char
prim_readAnswer external

--- set stdin handle to unbuffered mode
stdinUnbuffered :: IO ()
stdinUnbuffered external


--- set stdin handle to buffered mode (default)
stdinBuffered :: IO ()
stdinBuffered external