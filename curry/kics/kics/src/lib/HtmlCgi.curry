------------------------------------------------------------------------------
--- Library to support CGI programming in the HTML library.
--- It is only intended as an auxiliary library to implement dynamic web
--- pages according to the HTML library.
--- It contains a simple script that is installed for a dynamic
--- web page and which sends the user input to the real application
--- server implementing the application.
---
--- @author Michael Hanus
--- @version May 2007
------------------------------------------------------------------------------

module HtmlCgi(CgiServerMsg(..),runCgiServerCmd,cgiServerRegistry,
               readCgiServerMsg)
  where

import System
import Char
import NamedSocket
import CPNS(unregisterPort)
import IO
import ReadNumeric
import ReadShowTerm
import Time

--------------------------------------------------------------------------
-- Should the log messages of the server stored in a log file?
withCgiLogging = True

--- The name of the file to register all cgi servers.
cgiServerRegistry = "/tmp/PAKCS_CGIREGISTRY"

--------------------------------------------------------------------------
--- The messages to comunicate between the cgi script and the server program.
--- CgiSubmit env cgienv nextpage - pass the environment and show next page,
---   where env are the values of the environment variables of the web script
---   (e.g., QUERY_STRING, REMOTE_HOST, REMOTE_ADDR),
---   cgienv are the values in the current form submitted by the client,
---   and nextpage is the answer text to be shown in the next web page
--- @cons SketchStatus - get a sketch of the status of the server
--- @cons ShowStatus - show the status of the server with all event handlers
--- @cons CleanServer - clean up the server (with possible termination)
--- @cons StopCgiServer - stop the server
data CgiServerMsg = CgiSubmit [(String,String)] [(String,String)]
                  | ShowStatus
                  | SketchStatus
                  | CleanServer
                  | StopCgiServer

--- Reads a line from a handle and check whether it is a syntactically
--- correct cgi server message.
readCgiServerMsg :: Handle -> IO (Maybe CgiServerMsg)
readCgiServerMsg handle = do
  line <- hGetLine handle
  case readsQTerm line of
     [(msg,rem)] -> return (if all isSpace rem then Just msg else Nothing)
     _ -> return Nothing

--------------------------------------------------------------------------
-- Read arguments and start script:
-- Optional script arguments:
-- "-servertimeout n": The timeout period for the cgi server in milliseconds.
--                     If the cgi server process does not receive any request
--                     during this period, it will be terminated.
--                     The default value is defined in the library HTML.
--
-- "-multipleservers": If this argument is given, a new server process is
--                     started for each initial call to a cgi script, i.e.,
--                     there might be many servers for each cgi script
--                     (only reasonable with short server timeouts)
main = do
  args <- getArgs
  let (serverargs,multipleservers,rargs) = stripServerArgs "" False args
  case rargs of
    [url,cgikey,serverprog] -> cgiScript url serverargs multipleservers
                                         (cgikey2portname cgikey) serverprog
    [portname] -> cgiInteractiveScript portname -- for interactive execution
    _ -> putStrLn $ "ERROR: cgi script called with illegal arguments!"
 where
  stripServerArgs serverargs multipleservers args = case args of
    ("-servertimeout":tos:rargs) ->
         stripServerArgs (" -servertimeout "++tos) multipleservers rargs
    ("-multipleservers":rargs) -> stripServerArgs serverargs True rargs
    _ -> (serverargs,multipleservers,args)

--- Executes a specific command for a cgi server.
runCgiServerCmd :: String -> CgiServerMsg -> IO ()
runCgiServerCmd portname cmd = case cmd of
  StopCgiServer -> do
    putStrLn $ "Trying to stop server at port " ++ portname ++ "..."
    h <- trySendScriptServerMessage portname StopCgiServer
    hClose h
    unregisterPort portname
  CleanServer -> do
    putStrLn $ "Trying to clean server at port " ++ portname ++ "..."
    h <- trySendScriptServerMessage portname CleanServer
    hClose h
  ShowStatus ->  do
    h <- trySendScriptServerMessage portname ShowStatus
    hPutStrAndClose h
  SketchStatus -> do
    h <- trySendScriptServerMessage portname SketchStatus
    hPutStrAndClose h
  _ -> error "HtmlCgi.runCgiServerCmd: called with illegal command!"

--- Translates a cgi progname and key into a name for a port:
cgikey2portname cgikey =
  concatMap (\c->if isAlphaNum c then [c] else []) cgikey

-- Forward user inputs for interactive execution of cgi scripts:
cgiInteractiveScript :: String -> IO ()
cgiInteractiveScript portname = do
  cgiServerEnvVals <- mapIO getEnviron cgiServerEnvVars
  let cgiServerEnv = zip cgiServerEnvVars cgiServerEnvVals
  formEnv <- getFormVariables
  catchFail (sendToServerAndPrintOrFail cgiServerEnv formEnv)
            (putStrLn errorPage)
 where
  sendToServerAndPrintOrFail cgiEnviron newcenv = do
    h <- trySendScriptServerMessage portname (CgiSubmit cgiEnviron newcenv)
    hPutStrAndClose h

  errorPage =
    "Content-type: text/html\n\n" ++
    "<html>\n<head><title>Server Error</title></head>\n" ++
    "<body>\n<h1>Server Error</h1>\n</body>\n</html>"


-- Forward user inputs to cgi server process:
cgiScript :: String -> String -> Bool -> String -> String -> IO ()
cgiScript url serverargs multipleservers portname serverprog = do
  cgiServerEnvVals <- mapIO getEnviron cgiServerEnvVars
  let cgiServerEnv = zip cgiServerEnvVars cgiServerEnvVals
  let urlparam = head cgiServerEnvVals
  formEnv <- getFormVariables
  ctime <- getClockTime
  if null formEnv
   then do -- call to initial script
     let scriptKey = if multipleservers then show (clockTimeToInt ctime) else ""
         cgiEnviron = ("SCRIPTKEY",scriptKey) : cgiServerEnv
     catchFail (submitToServerOrStart url serverargs portname
                                      scriptKey serverprog cgiEnviron)
               (putStrLn (errorPage urlparam))
   else do -- call to continuation script
     let scriptKey = maybe "" id (lookup "SCRIPTKEY" formEnv)
         cgiEnviron = ("SCRIPTKEY",scriptKey) : cgiServerEnv
         newcenv = filter (\e -> fst e /= "SCRIPTKEY") formEnv
     catchFail (sendToServerAndPrintOrFail scriptKey cgiEnviron newcenv)
               (putStrLn (errorPage urlparam))
 where
  sendToServerAndPrintOrFail scriptKey cgiEnviron newcenv = do
    h <- trySendScriptServerMessage (portname++scriptKey) 
                                    (CgiSubmit cgiEnviron newcenv)
    eof <- hIsEOF h
    if eof then failed else hPutStrAndClose h

  errorPage urlparam =
    "Content-type: text/html\n\n" ++
    "<html>\n<head><title>Server Error</title></head>\n" ++
    "<body>\n<h1>Server Error</h1>\n" ++
    "<p>Your request cannot be processed due to a server error, reboot, or timeout.</p>\n" ++
    "<p><a href=\"" ++ (url ++ if null urlparam then "" else '?':urlparam) ++
    "\">Please click here to restart.</a></p>\n</body>\n</html>"

--- The values of the environment variables of the web script server
--- that are transmitted to the application program.
--- Currently, it contains only a selection of all reasonable variables
--- but this list can be easily extended.
cgiServerEnvVars = ["QUERY_STRING","HTTP_COOKIE","REMOTE_HOST","REMOTE_ADDR"]

-- The timeout (in msec) of the script server.
-- If the port of the application server is not available within the timeout
-- period, we assume that the application server does not exist and we start
-- a new one.
scriptServerTimeOut = 1000

-- send a message to the script server and return the connection handle,
-- or fail:
trySendScriptServerMessage portname msg =
  connectToSocketRepeat scriptServerTimeOut done 0 (portname++"@localhost") >>=
  maybe failed (\h -> hPutStrLn h (showQTerm msg) >> hFlush h >> return h)

-- submit an initial web page request to a server or restart it:
submitToServerOrStart url serverargs pname scriptkey serverprog cgiEnviron = 
  let completeportname = pname++scriptkey++"@localhost"
      cmd = serverprog ++ serverargs ++ " -port \"" ++ pname
                       ++ "\" -scriptkey \"" ++ scriptkey ++ "\""
      errout = if withCgiLogging then " 2>> "++url++".log" else ""
   in connectToSocketRepeat scriptServerTimeOut done 0 completeportname >>=
      maybe (putErr (cmd++errout) >> system (cmd++errout++" &") >> done)
            (\h -> hPutStrLn h (showQTerm (CgiSubmit cgiEnviron [])) >>
                   hFlush h >> hPutStrAndClose h)

hPutStrAndClose h = do
  eof <- hIsEOF h
  if eof
   then hClose h
   else hGetChar h >>= putChar >> hPutStrAndClose h

------------------------------------------------------------------------------
--- Gets the list of variable/value pairs sent from the browser for the
--- current CGI script.
--- Used for the implementation of the HTML event handlers.
getFormVariables :: IO [(String,String)]
getFormVariables = do
  clen <- getEnviron "CONTENT_LENGTH"
  cont <- getNChar (maybe 0 fst (readNat clen))
  return (includeCoordinates (parseCgiEnv cont))

-- translate a string of cgi environment bindings into list of binding pairs:
parseCgiEnv :: String -> [(String,String)]
parseCgiEnv s | s == ""   = []
              | otherwise = map ufield2field
                             (map (\(n,v)->(n,urlencoded2string v))
                                  (map (splitChar '=') (split (=='&') s)))
 where
   ufield2field (n,v) = if take 7 n == "UFIELD_"
                        then (tail n, urlencoded2string v)
                        else (n,v)

   -- split a string at particular character:
   splitChar c xs = let (ys,zs) = break (==c) xs
                    in if zs==[] then (ys,zs) else (ys,tail zs)

   -- split a string at all positions of a particular character:
   split p xs =
    let (ys,zs) = break p xs
    in if zs==[] then [ys]
                 else ys : split p (tail zs)

--- Translates urlencoded string into equivalent ASCII string.
urlencoded2string :: String -> String
urlencoded2string [] = []
urlencoded2string (c:cs)
  | c == '+'  = ' ' : urlencoded2string cs
  | c == '%'  = chr (maybe 0 fst (readHex (take 2 cs)))
                 : urlencoded2string (drop 2 cs)
  | otherwise = c : urlencoded2string cs

includeCoordinates :: [(String,String)] -> [(String,String)]
includeCoordinates [] = []
includeCoordinates ((tag,val):cenv) 
  = case break (=='.') tag of
      (_,[]) -> (tag,val):includeCoordinates cenv
      (event,['.','x']) -> ("x",val):(event,val):includeCoordinates cenv
      (_,['.','y']) -> ("y",val):includeCoordinates cenv
      _ -> error "includeCoordinates: unexpected . in url parameter"
   

-- get n chars from stdin:
getNChar n = if n<=0 then return ""
                     else do c <- getChar
                             cs <- getNChar (n-1)
                             return (c:cs)

------------------------------------------------------------------------------
-- for debugging
------------------------------------------------------------------------------

putErr :: String -> IO ()
putErr s = hPutStrLn stderr s