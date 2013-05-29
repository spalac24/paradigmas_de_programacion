------------------------------------------------------------------------
--- A simple command-based manager for CGI servers.
--- 
--- @author Michael Hanus
--- @version April 2007
------------------------------------------------------------------------

import ReadShowTerm
import System
import IOExts
import HtmlCgi
import Directory(doesFileExist)


main = do
  args <- getArgs 
  case args of
   ["clean"]  -> mainClean
   ["show"]   -> showAllActiveServers
   ["sketch"] -> mainSketch
   ["showAll"]-> mainShowAll
   ["stop"]   -> mainStop
   ["kill"]   -> mainKill
   ["stopServers",cgi] -> stopActiveScriptServers cgi
   _          -> error "usage: registry (sketch|show|showAll|\
                       \clean|stop|kill|stopServers <name of cgi script>)"

mainSketch  = cmdForAllServers "Sketch status of " SketchStatus

mainShowAll = cmdForAllServers "Status of " ShowStatus

mainClean   = cmdForAllServers "Clean status of " CleanServer >>
              getAndCleanRegistry >> done

mainStop    = cmdForAllServers "Stopping cgi server " StopCgiServer >>
              getAndCleanRegistry >> done

mainKill    = doForAllServers "Killing process of cgi server "
                              (\(pid,_,_) -> system ("kill -9 "++show pid)) >>
              getAndCleanRegistry >> done

showAllActiveServers :: IO ()
showAllActiveServers = do
  let header = "Currently active cgi script servers:"
  putStrLn header
  putStrLn (take (length header) (repeat '='))
  doForAllServers "" (const done)

--- Stops the active servers for a particular cgi script by sending them
--- a stop message. This operation is used by the installation script
--- "makecurrycgi" to terminate old versions of a server.
stopActiveScriptServers :: String -> IO ()
stopActiveScriptServers scriptprog = do
  regs <- getAndCleanRegistry
  putStrLn $ "Stop active servers for cgi script: " ++ scriptprog
  mapIO_ stopServer regs
 where
  stopServer (_,progname,port) =
    if progname==scriptprog
    then do putStrLn $ "...on port: " ++ port
            runCgiServerCmd port StopCgiServer
    else done

doForAllServers :: String -> ((Int,String,String) -> IO _) -> IO ()
doForAllServers cmt action = do
  regs <- getAndCleanRegistry
  mapIO_ doForServer regs
 where
  doForServer (pid,progname,port) = do
    putStrLn $ cmt ++ progname++":\n(pid: "++show pid++", port: "++port++")"
    action (pid,progname,port)

cmdForAllServers :: String -> CgiServerMsg -> IO ()
cmdForAllServers cmt servercmd =
  doForAllServers cmt (\ (_,_,port) -> runCgiServerCmd port servercmd)

-- Get the registry with active processes and clean up the registry file.
getAndCleanRegistry :: IO [(Int,String,String)]
getAndCleanRegistry = exclusiveIO (cgiServerRegistry++".lock") $ do
  regexists <- doesFileExist cgiServerRegistry
  regs <- if regexists then readQTermListFile cgiServerRegistry
                       else return []
  aregs <- mapIO (\ (pid,pname,port) -> do
                             pidactive <- isActivePID pid 
                             return (if pidactive 
                                     then [(pid,pname,port)] 
                                     else [])) regs
  let cregs = concat aregs
  if cregs==regs
   then done
   else writeFile cgiServerRegistry (concatMap (\reg->show reg++"\n") cregs)
  return cregs

-- Is an integer the pid of an existing process?
isActivePID :: Int -> IO Bool
isActivePID pid = do
  mypid <- getPID
  let tmp = "/tmp/tmp_pakcs_registry_"++show mypid
  system ("ps -p "++show pid++" | fgrep "++show pid++" > "++tmp)
  pr <- readCompleteFile tmp
  system ("rm "++tmp)
  return (not (null pr))
