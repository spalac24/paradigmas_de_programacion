------------------------------------------------------------------------------
--- Library for distributed programming with ports.
--- <A HREF="http://www.informatik.uni-kiel.de/~mh/publications/papers/PPDP99.html">
--- This paper</A> contains a description of the basic ideas
--- behind this library.
---
--- @author Michael Hanus, Bernd Braßel
--- @version March 2006
------------------------------------------------------------------------------

module Ports(Port,openPort,send,doSend,openNamedPort,
             connectPort,connectPortRepeat,connectPortWait,
             ping,timeoutOnStream,openProcessPort,
             SP_Msg(..),
             choiceSPEP,newObject,newNamedObject,runNamedServer,
             CPNS_Message(..),cpns_start,cpns_stop,cpns_show) where

import Unsafe
import Meta
import IOExts
import Time
import System(system,sleep,getPID)

--- The constructors for the port datatype are not visible to the user.

data Port a = Internal (IORef [a])
            | External Socket

unsafe :: IO _ -> Success
unsafe act = unsafePerformIO (act >> return success)

--- Opens an internal port for communication (IO version)
doOpenInternalPort ::  IO (Port _)
doOpenInternalPort = do
  let p free
  r <- newIORef p
  return (Internal r)

--- Opens an internal port for communication (constraint version)
openInternalPort :: Port _ -> Success
openInternalPort p = p=:=unsafePerformIO doOpenInternalPort

--- Opens an external port for communication (IO version)
doOpenExternalPort ::  IO (Port _)
doOpenExternalPort = do
  socket <- listenOnFresh
  return (External socket)

--- Opens an external port for communication (constraint version)
openExternalPort :: Port _ -> Success
openExternalPort p = p=:=unsafePerformIO doOpenInternalPort

doReadPort :: Port a -> IO [a]
doReadPort (Internal r) = 

--- Opens an internal port for communication.
--- @param p - a free variable which will be constrained
---            with the port messages
--- @param s - a free variable which will be instantiated
---            to the stream of incoming messages
openPort    :: Port a -> [a] -> Success
openPort p ms = unsafe (openInternalPort p ms)

createInternalPort :: IO (Port a)
createInternalPort = do
  let p free
  r <- newIORef p
  return (Internal r)



readPort :: Port a -> IO [a]
readPort (Internal r) = do
  msgs <- readIORef r
  return (ensureSpine msgs)

unless :: IO () -> Bool -> IO ()
unless x True  = done
unless x False = x

--- Sends a message to a port.
send :: a -> Port a -> Success
send msg p = unsafe (doSend msg p)

--- I/O action that sends a message to a port.
doSend :: a -> Port a -> IO ()
doSend msg (Internal p) = do
  msgs <- readIORef p 
  isFree msgs >>= unless (error "doSend: internal port already closed")
  let msgs' free 
  doSolve (msgs=:=msg:msgs')
  writeIORef p msgs'

--- Checks whether port p is still reachable.
--- @param n - the time to wait for reachability in milliseconds
--- @param p - a port to be checked for reachability
--- @return Nothing if port p is unreachable within n milliseconds,
---         or (Just m) if port p could be contacted within m milliseconds
ping :: Int -> Port _ -> IO (Maybe Int)
ping _ (Internal _) = return (Just 0) --look mum, how fast it is!!!
ping n (External name socket) = error "send ping message"

--- Checks for instantiation of a stream within some amount of time.
--- @param n - the time to wait for instantiation in milliseconds
--- @param str - the stream to be checked for instantiation
---              (usually the stream of incoming messages at some port)
--- @return (Just str) if str is instantiated within n milliseconds,
---         or Nothing otherwise
timeoutOnStream :: Int -> [a] -> Maybe [a]
timeoutOnStream n str = ensureNotFree (\n' -> prim_timeoutOnStream n' str) n

prim_timeoutOnStream :: Int -> [a] -> Maybe [a]
prim_timeoutOnStream external


--- A "stream port" is an adaption of the port concept to model the
--- communication with bidirectional streams, i.e., a stream port is
--- a port connection to a bidirectional stream (e.g., opened by
--- openProcessPort) where the communication
--- is performed via the following stream port messages.
---
--- @cons SP_Put s     - write the argument s on the output stream
--- @cons SP_GetLine s - unify the argument s with the next text line of the
---                      input stream
--- @cons SP_GetChar c - unify the argument c with the next character of the
---                      input stream
--- @cons SP_EOF b     - unify the argument b with True if we are at the end
---                      of the input stream, otherwise with False
--- @cons SP_Close     - close the input/output streams

data SP_Msg =
     SP_Put     String -- write the argument on the output stream
   | SP_GetLine String -- unify the argument with the next text line of the
                       -- input stream
   | SP_GetChar Char   -- unify the argument with the next character of the
                       -- input stream
   | SP_EOF     Bool   -- unify the argument with True if we are at the end
                       -- of the input stream, otherwise with False
   | SP_Close          -- close the input/output streams


--- Opens a new connection to a process that executes a shell command.
--- @param cmd - the shell command to be executed
--- @return the output/input stream (represented as a stream port)
---         that is connected to the standard input/output of the process
---         performing the execution of cmd.
openProcessPort :: String -> IO (Port SP_Msg)
openProcessPort msg = ensureDetGroundVal msg >>= prim_openProcessPort

prim_openProcessPort :: String -> IO (Port SP_Msg)
prim_openProcessPort external


--- Opens an external port with a symbolic name.
--- @param portname - the symbolic name under which the port is accessible
---                   (any string without occurrences of '@')
--- @return the stream of incoming messages at this port
openNamedPort :: String -> IO [_]
openNamedPort name = do
  stream <- openPortOnSocket socketnr portnr       -- open new port
  pid <- getPID
  -- register port:
  cpns_client "localhost" (CPNS_Put name pid socketnr portnr ack)
  if ack then done
         else putStrLn ("WARNING: Port name '"++name++"' already registered!")
  return stream
 where socketnr,portnr,ack free

--- Waits for connection to an external port.
--- In contrast to <code>connectPortNow</code>, this action waits until
--- the external port has been registered with its symbolic name.
--- @param waittime - the time to wait before retrying (in milliseconds)
--- @param action   - I/O action to be executed before each wait cycle
--- @param retries  - number of retries before giving up (-1 = retry forever)
--- @param portname - the symbolic name of the external port
---                   (must be either of the form "name@machine" or "name"
---                    where the latter is a shorthand for "name@localhost")
--- @return Nothing (if connection is not possible within the given limits)
---         or (Just p) where p is a port with the symbolic name portname
connectPortRepeat :: Int -> IO _ -> Int -> String -> IO (Maybe (Port _))
connectPortRepeat waittime action retries nameAtMachine =
     let (name,atMachine) = break (=='@') nameAtMachine
         machine = if atMachine=="" then "localhost" else tail atMachine in
     -- check whether remote CPNS demon is alive:
     cpns_alive waittime machine >>= \alive ->
     if not alive
       then action >> sleep (ms2s waittime) >>
            if retries==0 then return Nothing else
            connectPortRepeat waittime action (decr retries) nameAtMachine
       else -- get remote socket/port numbers:
         cpns_client machine (CPNS_Get name snr pnr) >>
         if snr==0
           then action >> sleep (ms2s waittime) >>
                if retries==0 then return Nothing else
                connectPortRepeat waittime action (decr retries) nameAtMachine
           else connectPortAtSocket snr pnr machine >>= return . Just
  where
    snr,pnr free

    ms2s n = let mn = n `div` 1000 in if mn==0 then 1 else mn

    decr n = if n<0 then n else n-1


--- Waits for connection to an external port and return the connected port.
--- This action waits (possibly forever) until the external port is
--- registered.
--- @param portname - the symbolic name of the external port
---                   (must be either of the form "name@machine" or "name"
---                    where the latter is a shorthand for "name@localhost")
--- @return a port with the symbolic name portname
connectPortWait :: String -> IO (Port _)
connectPortWait nameAtMachine = do
  Just port <- connectPortRepeat 1000 done (-1) nameAtMachine
  return port


--- Connects to an external port. The external port must be already
--- registered, otherwise an error is reported.
--- @param portname - the symbolic name of the external port
---                   (must be either of the form "name@machine" or "name"
---                    where the latter is a shorthand for "name@localhost")
--- @return a port with the symbolic name portname
connectPort   :: String -> IO (Port _)
connectPort nameAtMachine =
  do let (name,atMachine) = break (=='@') nameAtMachine
         machine = if atMachine=="" then "localhost" else tail atMachine
     -- get remote socket/port numbers:
     cpns_client machine (CPNS_Get name snr pnr)
     if snr==0
       then error ("connectPortNow: Port \""++name++"@"++machine++
                   "\" is not registered!")
       else done
     connectPortAtSocket snr pnr machine
  where snr,pnr free


--- This function implements a committed choice over the receiving
--- of messages via a stream port and an external port.
---
--- <EM>Note that the implementation of choiceSPEP works only with
--- Sicstus-Prolog 3.8.5 or higher (due to a bug in previous versions
--- of Sicstus-Prolog).</EM>
---
--- @param sp - a stream port sp
--- @param ms - a stream of messages received via an external port
--- @return (Left s) if s is an input line received
---                  at the stream port (via SP_GetLine) or
--- 
---         (Right ms) if the stream ms is instantiated
---                    with at least one new message at the head

choiceSPEP :: Port SP_Msg -> [msg] -> Either String [msg]
choiceSPEP p ms = ensureNotFree (\p' -> prim_choiceSPEP p' ms) p

prim_choiceSPEP :: Port SP_Msg -> [msg] -> Either String [msg]
prim_choiceSPEP external


--- Creates a new object (of type <code>State -> [msg] -> Success</code>)
--- with an initial state and a port to which messages for this object
--- can be sent.
---
--- @param object - an object template
--- @param state - the initial state of the object
--- @param port - a free variable which will be constrained to the port
---               for sending messages to the object
newObject :: (state -> [msg] -> Success) -> state -> Port msg -> Success
newObject object state port = let msgs free in
  openPort port msgs &> object state (map (ensureNotFree id) (ensureSpine msgs))


--- Creates a new object (of type <code>State -> [msg] -> Success</code>)
--- with a symbolic port name to which messages for this object can be sent.
--- @param object - an object template
--- @param state - the initial state of the object
--- @param portname - the symbolic name under which the object's port is
---                   accessible (any string without occurrences of '@')
newNamedObject :: (state -> [_] -> Success) -> state -> String -> IO ()
newNamedObject object state portname = do
  msgs <- openNamedPort portname
  doSolve (object state msgs)

--- Runs a new server (of type <code>[msg] -> IO a</code>) on a named port
--- to which messages can be sent.
--- @param server - a server function that processes incoming messages
--- @param portname - the symbolic name under which the server's port is
---                   accessible (any string without occurrences of '@')
runNamedServer :: ([_] -> IO a) -> String -> IO a
runNamedServer server portname = do
  msgs <- openNamedPort portname
  server msgs


------------------------------------------------------------------------------

-- The following predefined actions are not intended for application programs.
-- They are the basis to implement ports with symbolic names
-- via a name server (see below).


-- (openPortOnSocket snr pnr) is an action which opens an external port
-- on socket number snr with internal port number pnr and returns
-- the stream of incoming messages.
-- snr and pnr are allowed to be unbound: in this case they will be bound to the
-- numbers associated to a free port

openPortOnSocket :: Int -> Int -> IO [_]
openPortOnSocket snr pnr = do
  snr' <- ensureDetGroundVal snr
  pnr' <- ensureDetGroundVal pnr
  prim_openPortOnSocket snr' pnr'

prim_openPortOnSocket :: Int -> Int -> IO [_]
prim_openPortOnSocket external


-- The internal function that reads a port stream lazily.
basicServerLoop :: Port a -> [a]
basicServerLoop external


-- (connectPortAtSocket snr pnr host) is an action which returns a port that
-- has been opened at <host> with socket number <snr> and port number <pnr>

connectPortAtSocket :: Int -> Int -> String -> IO (Port _)
connectPortAtSocket snr pnr host = do
  snr'  <- ensureDetGroundVal snr
  pnr'  <- ensureDetGroundVal pnr
  host' <- ensureDetGroundVal host
  prim_connectPortAtSocket snr' pnr' host'

prim_connectPortAtSocket :: Int -> Int -> String -> IO (Port _)
prim_connectPortAtSocket external


-- Using these two primitive actions, we can implement "ports with
-- symbolic names" via a name server, called Curry Port Name Server (CPNS):

-- If we connect to a port with symbolic name pn, we first connect
-- to the CPNS of the host named by pn to get the physical socket
-- number of this port. In order to connect to CPNS from any
-- machine in the world, the CPNS demon always listens at the following
-- port:
-- (Note that this must be identical for all machines running
-- Distributed Curry! If this port is occupied by another process
-- on a host, you cannot run Distributed Curry on it.)

cpns_serversocket = 8766

cpns_startuplockfile = "/tmp/PAKCS_CPNSD.LOCK"

-- Now the implementation of the Curry Port Name Server is straightforward:

--- Type of messages to be processed by the Curry Port Name Server.
--- Although these messages are not intended for public use, this
--- type must be public so that any machine in the world can send
--- these messages.
---
--- @cons CPNS_Put name pid sn pn ack
---       -  assign the values pid, sn, and pn to name
---          (pid is the process number of the registered process
---           (should be 0 if it is unknown) and
---           ack is instantiated to True if registration had no problems)
--- @cons CPNS_Get name sn pn     - instantiate sn and pn with the
---                                 values assigned to name
--- @cons CPNS_Show               - show the current port registrations
--- @cons CPNS_Close              - terminate the CPNS demon

data CPNS_Message = CPNS_Put String Int Int Int Bool
                  | CPNS_Get String Int Int
                  | CPNS_Show
                  | CPNS_Close

--- Starts the "Curry Port Name Server" (CPNS) running on the local machine.
--- The CPNS is responsible to resolve symbolic names for ports
--- into physical socket numbers so that a port can be reached
--- under its symbolic name from any machine in the world.

cpns_start :: IO ()
cpns_start = catchFail startup
                       (putStrLn "FAILURE occurred during startup!" >>
                        delete_startuplockfile)
 where
   delete_startuplockfile = do
     putStrLn ("Removing startup lock file \""++cpns_startuplockfile++"\"...")
     system ("rm -f "++cpns_startuplockfile)
     done

   startup =
     putStrLn "Starting Curry Port Name Server..." >>
     openPortOnSocket cpns_serversocket 0 >>= \msgs ->
     delete_startuplockfile >>
     putStrLn "Curry Port Name Server is ready." >>
     cpns_loop [] msgs

-- The implementation of the CPNS demon is a loop processing the
-- incoming messages.
-- The assignment from names to socket numbers (the local state of the
-- server) is implemented as a function (which results in a primitive
-- linear search for entries, but currently the number of entries
-- is usually very small on each host):

cpns_loop :: [(String,Int,Int,Int)] -> [CPNS_Message] -> IO ()
cpns_loop _    (CPNS_Close : _) = putStrLn "CPNS demon terminated."
cpns_loop regs (CPNS_Show : s) = do
  putStrLn "Currently registered port names:"
  mapIO_ (\(n,pid,sn,pn) -> putStrLn (n++": pid "++show pid++
                            " / socket "++show sn++" / number "++show pn)) regs
  cpns_loop regs s
cpns_loop regs (CPNS_Get name sn pn : s) =
  do getRegisteredPortName regs name sn pn
     cpns_loop regs s
cpns_loop regs (CPNS_Put n pid sn pn ack : s) =
  do new_regs <- tryRegisterPortName regs n pid sn pn ack
     cpns_loop new_regs s

getRegisteredPortName regs name sn pn =
  let nameregs = filter (\(n,_,_,_)->name==n) regs in
  if null nameregs
  then doSolve ((sn,pn)=:=(0,0))
  else let (_,pid,sn',pn') = head nameregs in
       if pid>0
       then doesProcessExists pid >>= \pex ->
            if pex
            then doSolve ((sn,pn)=:=(sn',pn'))
            else --putStrLn ("WARNING: Process "++show pid++" not running!") >>
                 doSolve ((sn,pn)=:=(0,0))
       else doSolve ((sn,pn)=:=(sn',pn'))

tryRegisterPortName regs name pid sn pn ack =
  let nameregs = filter (\(n,_,_,_)->name==n) regs in
  (if null nameregs then doSolve (ack=:=True) else
   let (_,pid',_,_) = head nameregs in
   if pid'>0 && pid'/=pid  -- we allow registration from the same process
   then doesProcessExists pid' >>= \pex -> doSolve(ack =:= not pex)
   else doSolve (ack=:=True)) >>
  getClockTime >>= \time ->
  putStrLn ("Register port \""++name++"\": pid "++show pid++
            " / socket "++show sn++
            " / number "++show pn ++ " at " ++ toDateString time) >>
  return ((name,pid,sn,pn) : filter (\ (n,_,_,_)->name/=n) regs)

-- test whether a process with a given pid is running:
doesProcessExists :: Int -> IO Bool
doesProcessExists pid = do
  status <- system("test -z \"`ps -p "++show pid++" | fgrep "++show pid++"`\"")
  return (status>0)

-- cpns_client sends a message string to the CPNS port of the given host:

cpns_client host msg =
  do alive <- cpns_alive 5000 host
     if not alive
      then error ("Curry port name server at host \""++host++
                  "\" is not reachable!")
      else connectPortAtSocket cpns_serversocket 0 host >>= doSend msg


-- cpns_alive checks whether the CPNS of the given host is reachable
-- within arg1 milliseconds:
cpns_alive :: Int -> String -> IO Bool
cpns_alive pingtime host =
  do port <- connectPortAtSocket cpns_serversocket 0 host
     pingresult <- ping pingtime port
     return $ maybe False (const True) pingresult


--- Terminates the "Curry Port Name Server" running on the local machine.
cpns_stop :: IO ()
cpns_stop = cpns_client "localhost" CPNS_Close

--- Shows the currently registered ports at the "Curry Port Name Server"
--- running on the local machine.
cpns_show :: IO ()
cpns_show = cpns_client "localhost" CPNS_Show


--  end of module Ports
