
import qualified Prelude (error)

prim_listenOn ::  Int -> (Debug (IO Socket)) 
prim_listenOn = Prelude.error "prim_listenOn"

listenOnFresh ::  (Debug (IO (T2 Int Socket))) 
listenOnFresh = Prelude.error "listenOnFresh"

prim_socketListen ::  Socket -> Int -> (Debug (IO T1)) 
prim_socketListen = Prelude.error "prim_socketListen"

prim_socketAccept ::  Socket -> (Debug (IO (T2 (List Char) Handle))) 
prim_socketAccept = Prelude.error "prim_socketAccept"

prim_waitForSocketAccept
 ::  Socket -> Int -> (Debug (IO (Maybe (T2 (List Char) Handle)))) 
prim_waitForSocketAccept = Prelude.error "prim_waitForSocketAccept"

prim_connectToSocket ::  (List Char) -> Int -> (Debug (IO Handle)) 
prim_connectToSocket = Prelude.error "prim_connectToSocket"

showConsSocket :: Socket -> Term
showConsSocket (Socket s) = showTerm s

instance Generator Socket where
  generator = P.error "free variable of type Socket"