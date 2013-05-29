
import qualified Prelude (error,Read,readsPrec)

instance Prelude.Read Handle where 
  readsPrec _ s = [(Handle (toprim (consTerm s []) ()),"")]
    
stdin ::  (Debug Handle) 
stdin = Prelude.error "stdin"

stdout ::  (Debug Handle) 
stdout = Prelude.error "stdout"

stderr ::  (Debug Handle) 
stderr = Prelude.error "stderr"

prim_openFile ::  List Char -> IOMode -> (Debug (IO Handle)) 
prim_openFile = ioFunc2 "prim_openFile" (\_ _ -> getNextExtVal)

prim_hClose ::  Handle -> (Debug (IO T1)) 
prim_hClose = ioFunc1 "prim_hClose" (\ _ -> P.return (coerce ()))

prim_hFlush ::  Handle -> (Debug (IO T1)) 
prim_hFlush = ioFunc1 "prim_hFlush" (\ _ -> P.return (coerce ()))

prim_hIsEOF ::  Handle -> (Debug (IO Bool)) 
prim_hIsEOF = ioFunc1 "prim_hIsEOF" (\_ -> coerce0 (getNextExtVal::Debug P.Bool))

prim_hSeek ::  Handle -> SeekMode -> Int -> (Debug (IO T1)) 
prim_hSeek = ioFunc3 "prim_hSeek" (\ _ _ _ -> P.return (coerce ()))

prim_hWaitForInput ::  Handle -> Int -> (Debug (IO Bool)) 
prim_hWaitForInput = ioFunc2 "prim_hWaitForInput" (\_ _ -> coerce0 (getNextExtVal::Debug P.Bool))

prim_hWaitForInputs ::  List Handle -> Int -> (Debug (IO Int)) 
prim_hWaitForInputs = ioFunc2 "prim_hWaitForInputs" (\_ _ -> coerce0 (getNextExtVal::Debug P.Integer))

prim_hGetChar ::  Handle -> (Debug (IO Char)) 
prim_hGetChar = ioFunc1 "prim_hGetChar" (\_ -> coerce0 (getNextExtVal::Debug P.Char))

prim_hPutChar ::  Handle -> Char -> Debug (IO T1) 
prim_hPutChar = ioFunc2 "prim_hPutChar" (Prelude.error "prim_hPutChar")

prim_hIsReadable ::  Handle -> (Debug (IO Bool)) 
prim_hIsReadable = ioFunc1 "prim_hIsReadable" (\_ -> coerce0 (getNextExtVal::Debug P.Bool))

prim_hIsWritable ::  Handle -> (Debug (IO Bool)) 
prim_hIsWritable = ioFunc1 "prim_hIsWritable" (\_ -> coerce0 (getNextExtVal::Debug P.Bool))

showConsHandle :: Handle -> Term
showConsHandle (Handle t) = showTerm t

instance Generator Handle where
  generator = P.error "free variable of type IO.Handle"