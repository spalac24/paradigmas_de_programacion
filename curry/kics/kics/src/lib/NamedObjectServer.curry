module NamedObjectServer where

import IO
import IOExts
import Time
import Maybe
import System
import Directory
import ReadShowTerm
import Socket

debug = True

logFile = "server.log"

put s = if debug then appendFile logFile (s++"\n") else done

type Entry a = (Int,ClockTime,a)
data Store a = Store Int [Entry a]
type StoreRef a = IORef (Store a)

--- todo: delete old, clean, obey  maximum no of entries
--- delete all entries for when request served???
lookupStore :: ClockTime -> Int -> [Entry a] -> Maybe (a,[Entry a])
lookupStore _ _ [] = Nothing
lookupStore t0 n (e@(n',_,a):xs)
    {-| let dt = diffClockTimes t0 t in
        ((tdDay dt)>0) || ((tdMonth dt)>0) || ((tdYear dt)>0) = 
        lookupStore t0 n xs-}
    | n==n'     = Just (a,e:xs)
    | otherwise = lookupStore t0 n xs >>-
                  \ (a',ys) -> Just (a',e:ys)

lookupStoreRef :: StoreRef a -> Int -> IO (Maybe a)
lookupStoreRef storeRef n = do
    Store max store <- readIORef storeRef
    time <- getClockTime
    put ("lookup: "++show n)
    case lookupStore time n store of
      Nothing -> put "not found" >> return Nothing
      (Just (a,newStore)) -> do
            put "found"
            writeIORef storeRef (Store max newStore)
            return (Just a)
       
       

addToStore :: Int -> ClockTime -> a -> [Entry a] -> [Entry a]
addToStore n t a s = (n,t,a):s

extendStoreRef :: StoreRef a -> a -> IO Int
extendStoreRef storeRef a = do
   Store max store <- readIORef storeRef
   time <- getClockTime
   writeIORef storeRef (Store (max+1) (addToStore max time a store))
   put ("newmax: "++show max)
   return max

type CgiFun = String -> IO String
type FunStore = IORef (Store CgiFun)

srvFile :: String -> String
srvFile s = reverse (takeWhile (/='/') (reverse s)) ++ ".srv"

-- todo: smaller clients
--request :: (FunStore -> CgiFun) -> String -> IO ()
request cgiFunction reqStr = do
   progName <- getProgName
   let portFile = srvFile progName
   ex <- doesFileExist portFile
   if ex 
     then do
       put "server exists"
       portNoStr <- readFile portFile
       put ("PortNumber from file: "++portNoStr)
       mchannel <- connectToSocket "localhost" (readTerm portNoStr)
       case mchannel of
         --Nothing -> put "server dead" >> startServer cgiFunction reqStr
         {-Just-} 
         channel -> do
           put "writing request"
           writeChannel channel reqStr
       	   put "waiting for answer"
       	   answer <- readChannel channel 
       	   put ("Answer begins with: "++take 300 answer)
       	   putStrLn answer
     else do
       put "no server"
       startServer cgiFunction reqStr 

writeChannel channel s = do
   hPutStrLn channel (show s)
   put "flushing channel"
   hFlush channel
readChannel  channel = hGetLine channel >>= return . readTerm

startServer cgiFunction reqStr = do
   storeRef <- newIORef (Store 0 [])
   progName <- getProgName
   put ("initializing port listener for "++progName)
   (portNo,socket) <- listenOnFresh
   put ("socket: "++show socket)
   writeFile (srvFile progName) (show portNo)
   put "serving first request"
   result <- cgiFunction storeRef reqStr
   put ("Result begins with: "++take 300 result) 
   putStrLn result
   hClose stdout
   loopStore storeRef socket cgiFunction

-- todo: verify that we are correct server: client sends our name
--loopStore :: FunStore -> Socket -> (FunStore -> CgiFun) -> IO ()
loopStore storeRef socket cgiFunction = do
   put "Waiting for request: "
   (_,h) <- socketAccept socket
   put "Accept"
   req <- readChannel h
   put ("Request: "++req)
   Store _ store <- readIORef storeRef
   put ("Store size: "++(show (length store)))
   result <- cgiFunction storeRef req
   put ("Result begins with: "++take 300 result)
   writeChannel h result
   put "Answer written"
   hClose h
   put "Channel closed"
   loopStore storeRef socket cgiFunction

