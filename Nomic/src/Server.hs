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
import Data.List hiding (insert)
import Data.Set hiding (filter, null, map)
import Network.BSD
import Observable
import Comm
import Interpret
import Language.Haskell.Interpreter.Server --TODO: hide in Interpret
import Control.Concurrent.Process hiding (Handle)
import Commands


-- | associate a player number with a handle
data PlayerClient = PlayerClient { cplayerNumber :: PlayerNumber,
                                   handle :: Handle}
                                   deriving (Eq, Show)
   
instance Ord PlayerClient where
   h <= g = (cplayerNumber h) <= (cplayerNumber g)
                                 
-- type the channels to pass commands
type CommandChan = TChan String

-- type of the channel to pass commands upstream to the client thread
data ClientCommands = Ready | Quit
type ClientChan = TChan ClientCommands
                                   
-- | communication between clients and server
type Client = (CommandChan, ClientChan, Handle)

-- | Channel to put commands from clients to the game thread
type GameChan = TChan (Handle, ClientChan, String)               

-- | A simple main to parse a port number from the command line, and fire up the server socket.
-- Start Nomic in server mode
serverStart port = withSocketsDo $ do
    putStrLn "Starting Nomic Server..."
    servSock <- listenOn $ PortNumber port
    putStrLn $ "listening on: " ++ show port
    host <- getHostName
    putStrLn $ "to connect, try \"telnet " ++ host ++ " 10000\" in a shell window"
    startAll servSock `finally` sClose servSock


-- | starts every threads
startAll servSock = do
    -- Fork the loop that will handle new client connections along with its channel
    acceptChan <- atomically newTChan
    --forkIO $ acceptLoop servSock acceptChan

    spawn $ makeProcess id (acceptLoop servSock acceptChan)
    
    -- Fork the loop that will handle game commands along with its channel
    gameChan <- atomically newTChan
    debugChan <- atomically newTChan
    forkIO $ runMulti gameChan (return ()) debugChan
    -- the server loop will dispatch messages between threads
    mainLoop servSock acceptChan [] gameChan



-- | the loop will handle new client connections and fork a subsequent thread for each client
acceptLoop :: Socket -> TChan Client -> Process () ()
acceptLoop servSock acceptChan = do -- acceptChan
    (cHandle, _, _) <- lift $ accept servSock
    --hSetEcho cHandle True
    liftIO $ hSetBuffering cHandle LineBuffering
	-- Fork a loop that will handle client communication along with its channel
    clientChan <- lift $ atomically newTChan
    handleFree <- lift $ atomically newTChan
    liftIO $ forkIO $ clientLoop cHandle clientChan handleFree
	-- publish new client connection with its chan and handle on acceptChan
    lift $ atomically $ writeTChan acceptChan (clientChan, handleFree, cHandle)
    acceptLoop servSock acceptChan


-- As before, each client gets a loop which reads from the handle and pumps the data right into a channel.
-- However, this time, exception handling is done per-thread;
-- if a client disconnects we just want the thread to die silently.
-- A more clever implementation might have a more structured channel which allows it to indicate
-- when the client disconnects.


-- | a loop that will handle client communication
clientLoop :: Handle -> CommandChan -> ClientChan -> IO ()
clientLoop h chan handleFree = do
   --put the line in the chan
   s <- hGetLine h
   atomically $ writeTChan chan s
   --read up-stream command
   upc <- atomically $ readTChan handleFree
   case upc of
      Ready -> clientLoop h chan handleFree
      Quit -> do
         hPutStrLn h "Goodbye!"
         hClose h
       


-- | the server loop will dispatch messages between threads
mainLoop :: Socket -> TChan Client -> [Client] -> GameChan -> IO ()
mainLoop servSock acceptChan clients gameChan = do
    --read on both acceptChan and all client's chans
    r <- atomically $ (Left `fmap` readTChan acceptChan)
                      `orElse`
                      (Right `fmap` tselect clients)
    case r of
          -- new data on the accept chan
          Left (ch,hf,h)    -> do  
               atomically $ writeTChan gameChan (h, hf, "newplayer") --TODO fix
               --loop
               mainLoop servSock acceptChan ((ch,hf,h):clients) gameChan
               
          -- new data on the clients chan
          Right (l,hf, h) -> do  
               -- do some filtering
               let myLine = filter isPrint l
               --putStrLn $ "data: " ++ myLine ++ " from handle: " ++ show h
               atomically $ writeTChan gameChan (h, hf, myLine)
               --loop
               mainLoop servSock acceptChan clients gameChan




-- tselect is a function which multiplexes any number of TChans.
-- It will return the data from whichever TChan it can read,
-- along with the "key" value that can be supplied in the pair.
-- This takes advantage of the STM combinators orElse and retry by applying them
-- to a list of actions constructed around the TChans.

tselect :: [Client] -> STM (String, ClientChan, Handle)
tselect = foldl orElse retry . map (\(ch, fh, ty) -> (\tc -> (tc, fh, ty)) `fmap` (readTChan ch))



-- | A structure to hold the active games and players
data Server = Server { multi :: Multi,
                       playerClients :: Set PlayerClient,
                       interpreterHandle :: ServerHandle}
                       --deriving (Eq)
                                             
instance Show Server where
   show Server{multi=m, playerClients =pcs} = show m ++ "\n" ++ (show $ sort $ toList pcs)
   
defaultServer :: ServerHandle -> Server
defaultServer sh = Server defaultMulti (fromList []) sh

-- | A State to pass around active games and players.
-- Furthermore, the output are to be made with Comm to output to the right console.
type ServerState = StateT Server IO ()

-- | An helper function that makes it very clear how to use the state transformer Server.
runWithServer :: Server -> ServerState -> IO ()
runWithServer = flip evalStateT


-- | a loop that will handle game commands
runMulti :: GameChan -> ServerState -> TChan Server -> IO ()
runMulti gc debugState debugChan = do
   sh <- sHandle
   runWithServer (defaultServer sh) (multiLoop gc debugState debugChan)


-- | a loop that will handle game commands (passed through a chan).
-- the debugState parameter is a monad that allows you to do whatever you want (check the state of the game, modify it etc.) for debugging purpose.
-- debugChan allows you to read the server's state from outside.
multiLoop :: GameChan -> ServerState -> TChan Server -> ServerState    --TODO: use Cont monad?
multiLoop gc debugState debugChan = do
     --read the channel (blocking)
     (h, hf, l) <- liftIO $ atomically $ readTChan gc
     (Server _ pc _) <- get
     case l of
        "newplayer" -> do
           --add the new player to the server's list
           modify (\ser -> ser { playerClients = addNewPlayer h pc})
           --verify
           mypc <- gets playerClients
           lift $ putStrLn $ "clients list:" ++ (show (toList mypc))
           issuePlayerCommand l h
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
           --ask the client thread to exit
           lift $ atomically $ writeTChan hf Quit
        "" -> do
           --release reading on handle for the next command.
           lift $ atomically $ writeTChan hf Ready
           return ()
        -- every other command is passed through
        _ -> do
           issuePlayerCommand l h
           --release reading on handle for the next command.
           lift $ atomically $ writeTChan hf Ready
                 

     --re-launch for the next command
     multiLoop gc debugState debugChan

-- | issue the player's command with the right Comm and Multi environnement
issuePlayerCommand :: String -> Handle -> ServerState
issuePlayerCommand l h = do
   (Server m newPC sh) <- get
   let comm = (Communication h h sh)
   --issue the player's command 
   case getPlayerNumber h newPC of
      Nothing -> error "player's handle not found"
      Just pn -> do
         --run the command (with the right Comm and Multi environnement)
         newM <- liftIO $ runWithComm comm (runWithMulti m (runLine l pn))
         --modify server state with the result
         modify ( \ser -> ser {multi=newM})

           
-- | gives the player's number associated the that handle
getPlayerNumber :: Handle -> Set PlayerClient -> Maybe PlayerNumber
getPlayerNumber h ps = cplayerNumber <$> find (\PlayerClient {handle = myh} -> myh==h) (toList ps)


-- | find an available PN
newPlayerNumber :: Set PlayerClient -> PlayerNumber
newPlayerNumber ps = findFirstFree (toList ps) 1 where
   findFirstFree pc i = case find (\PlayerClient {cplayerNumber = cpn} -> cpn==i) pc of
                 Just _  -> findFirstFree pc (i+1)
                 Nothing -> i

-- | add a new player
addNewPlayer :: Handle -> Set PlayerClient -> Set PlayerClient
addNewPlayer h sp = insert (PlayerClient (newPlayerNumber sp) h) sp
