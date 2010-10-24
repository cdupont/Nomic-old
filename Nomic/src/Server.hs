{-# LANGUAGE StandaloneDeriving#-}

-- | This server handles asynchronous text connections.
-- It has 4 threads:
-- mainLoop: dispatch messages between every threads (this is the main program thread, not forked)
-- acceptLoop: handles new client connections
-- multiLoop: handles game commands. It contains the whole game state.
-- clientLoop: one per client, handles clients communications.

-- And 5 channels:
-- acceptChan: used by acceptLoop to publish new clients connections to mainLoop.
-- gameChan: used for the communication between mainLoop and multiLoop.
-- clientChan: one per client, use by the clients to communicate with mainLoop.
-- handleFree: one per client, used for upstream comunication with clientLoops, to control usage of the Handle.
-- debugChan: used to pass debug commands

   
module Server where

-- Module Network is the simple networking library, presenting a Handle-based interface.
import Network (listenOn, accept, sClose, Socket,
                withSocketsDo, PortID(..))

import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.STM
import Multi
import Control.Monad.State
import System.IO
import Data.Char
import Control.Applicative
import Data.List
import Network.BSD
import Observable
import Comm
import Interpret
import Language.Haskell.Interpreter.Server --TODO: hide in Interpret
import Control.Concurrent.Process hiding (Handle)
import Commands
import Utils


-- | associate a player number with a handle
data PlayerClient = PlayerClient { cPlayerNumber :: PlayerNumber,
                                   cHandle :: Handle}
                                   deriving (Eq, Show)
   
-- | A structure to hold the active games and players
data Server = Server { multi :: Multi,
                       playerClients :: [PlayerClient],
                       interpreterHandle :: ServerHandle}
                       --deriving (Eq)

-- | A State to pass around active games and players.
-- Furthermore, the output are to be made with Comm to output to the right console.
type ServerState = StateT Server IO ()
                       
-- the channels to pass commands
type CommandChan = TChan String

-- type of the channel to pass commands upstream to the client thread
data ClientCommands = Ready | Quit
type ClientChan = TChan ClientCommands
                                   
-- | communication between clients and server
type ClientComm = (CommandChan, ClientChan, Handle)

-- | a channel where to publish new clients connection
type AcceptChan = TChan ClientComm
      
defaultServer :: ServerHandle -> Server
defaultServer sh = Server defaultMulti [] sh


type DebugServer = (ServerState, TChan Server)

defaultDebugServer = do
   debugChan <- atomically newTChan
   return (debugViewState, debugChan)


-- | An helper function that makes it clear how to use the state transformer Server.
runWithServer :: Server -> ServerState -> IO ()
runWithServer = flip evalStateT


-- | a loop that will handle game commands
runMulti :: AcceptChan -> DebugServer -> IO ()
runMulti acceptChan debug = do
   sh <- sHandle
   runWithServer (defaultServer sh) (mainLoop acceptChan [] debug)

   
-- | Start Nomic in server mode
serverStart port = withSocketsDo $ do
    putStrLn "Starting Nomic Server..."
    servSock <- listenOn $ PortNumber port
    putStrLn $ "listening on: " ++ show port
    host <- getHostName
    putStrLn $ "to connect, try \"telnet " ++ host ++ " 10000\" in a shell window"
    startAll servSock `catch` (\_ -> putStrLn "serverStart: Closing") `finally` sClose servSock


-- | starts every threads
startAll servSock = do
    -- Fork the loop that will handle new client connections along with its channel
    acceptChan <- atomically newTChan
    --forkIO $ acceptLoop servSock acceptChan

    acceptHandle <- (spawn $ makeProcess id (acceptLoop servSock acceptChan))-- `catch` (\_ -> do putStrLn "acceptLoop: Closing"; return ())
    
    -- the multi loop will centralize and dispatch communications
    def <- defaultDebugServer
    runMulti acceptChan def


-- | the loop will handle new client connections and fork a subsequent thread for each client
acceptLoop :: Socket -> AcceptChan -> Process () ()
acceptLoop servSock acceptChan = do -- acceptChan
   (cHandle, _, _) <- lift $ accept servSock
   --hSetEcho cHandle True
   liftIO $ hSetBuffering cHandle LineBuffering
	
	-- Fork a loop that will handle client communication along with its channel
   cc <- liftIO $ newClientComm cHandle
   liftIO $ forkIO $ clientLoop cc `catch` (\_ -> putStrLn "acceptLoop: clientLoop exception")
	-- publish new client connection with its chan and handle on acceptChan
   lift $ atomically $ writeTChan acceptChan cc
   acceptLoop servSock acceptChan


newClientComm :: Handle -> IO ClientComm
newClientComm h = do
   clientChan <- liftIO $ atomically newTChan
   handleFree <- liftIO $ atomically newTChan
   return (clientChan, handleFree, h)


-- | a loop that will handle client communication
clientLoop :: ClientComm -> IO ()
clientLoop cc@(chan, handleFree, h) = do
   --read up-stream command
   upc <- atomically $ readTChan handleFree
   case upc of
      Ready -> do
         --put the line in the chan
         s <- hGetLine h
         atomically $ writeTChan chan s
         clientLoop cc
      Quit -> do
         hPutStrLn h "Goodbye!"
         hClose h
       


-- | the server loop will dispatch messages between threads
mainLoop :: AcceptChan -> [ClientComm] -> DebugServer -> ServerState
mainLoop acceptChan clients d@(debugState, debugChan) = do
   --read on both acceptChan and all client's chans
   r <- lift $ atomically $ (Left `fmap` readTChan acceptChan)
                              `orElse`
                     (Right `fmap` tselect clients)

   case r of
      -- new data on the accept chan
      Left cc@(_,hf,h)    -> do
         --register new client
         newClient h
         lift $ atomically $ writeTChan hf Ready
         --loop
         mainLoop acceptChan (cc:clients) d
               
      -- new data on the clients chan
      Right (l,hf, h) -> do  
         -- do some filtering
         let myLine = filter isPrint l
         --putStrLn $ "data: " ++ myLine ++ " from handle: " ++ show h
         case myLine of
            "debug read" -> do
               s <- get
               --write state into the channel (for external reading).
               lift $ atomically $ writeTChan debugChan s
               --release reading on handle for the next command.
               lift $ atomically $ writeTChan hf Ready
            "debug write" -> do
               --execute the debug monad.
               debugState
               --release reading on handle for the next command.
               lift $ atomically $ writeTChan hf Ready
            "quit" -> do
               playerQuit h
               --ask the client thread to exit
               lift $ atomically $ writeTChan hf Quit
            "" -> do
               --release reading on handle for the next command.
               lift $ atomically $ writeTChan hf Ready
               return ()
            -- every other command is passed through
            _ -> do
               issuePlayerCommand myLine h
               --release reading on handle for the next command.
               lift $ atomically $ writeTChan hf Ready

         --loop
         mainLoop acceptChan clients d


newClient :: Handle -> ServerState
newClient h = do
   (Server multi pcs sh) <- get
   let comm = (Communication h h sh)
   (pn, m) <- liftIO $ evalStateT (runStateT newPlayer multi) comm

   --lift $ putStrLn $ show $ mPlayers m
   modify (\ser -> ser { playerClients = PlayerClient { cPlayerNumber = pn,
                                                        cHandle = h} : pcs,
                         multi = m})


tselect :: [ClientComm] -> STM (String, ClientChan, Handle)
tselect = foldl orElse retry . map (\(ch, fh, ty) -> (\tc -> (tc, fh, ty)) `fmap` (readTChan ch))



-- | issue the player's command with the right Comm and Multi environnement
issuePlayerCommand :: String -> Handle -> ServerState
issuePlayerCommand l h = do
   (Server m pcs sh) <- get
   let comm = (Communication h h sh)
   --issue the player's command 
   case getPlayerNumber h pcs of
      Nothing -> error "issuePlayerCommand: player's handle not found"
      Just pn -> do
         --run the command (with the right Comm and Multi environnement)
         newM <- liftIO $ runWithComm comm (runWithMulti m (runLine l pn))
         --modify server state with the result
         modify ( \ser -> ser {multi=newM})


playerQuit :: Handle -> ServerState
playerQuit h = do
   pcs <- gets playerClients
   -- erase player client
   modify (\ser -> ser { playerClients = filter (\PlayerClient {cHandle = myh} -> myh /= h) pcs})

-- | gives the player's number associated to that handle
getPlayerNumber :: Handle -> [PlayerClient] -> Maybe PlayerNumber
getPlayerNumber h ps = cPlayerNumber <$> find (\PlayerClient {cHandle = myh} -> myh==h) ps

debugViewState :: ServerState
debugViewState = lift . putStrLn . show =<< get  

instance Ord PlayerClient where
   h <= g = (cPlayerNumber h) <= (cPlayerNumber g)

instance Show Server where
   show Server{multi=m, playerClients =pcs} = show m ++ "\n" ++ (show $ sort pcs)
   
