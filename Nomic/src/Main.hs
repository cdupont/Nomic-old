-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (main) where

--ifdef mingw32_BUILD_OS
--define WINDOWS
--endif

import System.Console.GetOpt 
import System.Environment 
--import Server hiding (Server)
import Web
import Multi
import Control.Concurrent
import Interpret
import System.Posix.Signals
import Control.Concurrent.STM

-- | Entry point of the program.
main :: IO Bool
main = do
   putStrLn "Welcome to Haskell Nomic"
   serverCommandUsage
   args <- getArgs 
   (flags, _) <- nomicOpts args
   --parseActions flags
   --let verbose = Verbose `elem` flags
   if Test `elem` flags
      then do
         --start the haskell interpreter
         -- sh <- startInterpreter
         --at <- allTests
         --putStrLn $ "Test result: " ++ show at
         return True --at
      else do
         multi <- newTVarIO defaultMulti
         --start the haskell interpreter
         sh <- startInterpreter
         installHandler sigINT (Catch handler) Nothing
         --start the web server
         forkIO $ launchWebServer sh multi
         --loop
         serverLoop multi--c `finally` createCheckpointAndShutdown c
         return True





handler :: IO ()
handler = putStrLn " Signals disabled, press q <Enter> to quit"

--createCheckpointAndShutdown :: MVar TxControl -> IO ()
--createCheckpointAndShutdown control = do
--   putStrLn "Creating checkpoint and quiting..."
--   createCheckpoint control
--   shutdownSystem control

--localsaver :: IO Saver
--localsaver = do
--   pn <- getProgName
--   d <- getDataDir
--   return $ Queue (FileSaver (d ++ "_local/" ++pn++"_state"))
--
--localStartSystemState :: (Methods a, Component a) => Proxy a -> IO (MVar TxControl)
--localStartSystemState proxy = do
--   saver <- localsaver
--   runTxSystem saver proxy


-- | a loop that will handle server commands
serverLoop :: TVar Multi -> IO ()
serverLoop tm = do
   s <- getLine
   case s of
      "d" -> do
         m <- atomically $ readTVar tm
         putStrLn $ show m
         serverLoop tm
      "s" -> do
         putStrLn "saving state..."
         --createCheckpoint c
         serverLoop tm
      "q" -> return ()
      _ -> do
         putStrLn "command not recognized"
         serverLoop tm

serverCommandUsage :: IO ()
serverCommandUsage = do
   putStrLn "Server commands:"
   putStrLn "s -> save state"
   putStrLn "q -> quit"

   

-- | Launch mode 
data Flag 
     = Verbose | Version | Solo | Test 
       deriving (Show, Eq)

-- | launch options description
options :: [OptDescr Flag]
options =
     [ Option ['v']     ["verbose"] (NoArg Verbose)       "chatty output on stderr"
     , Option ['V','?'] ["version"] (NoArg Version)       "show version number"
     , Option ['s']     ["solo"]    (NoArg Solo)          "start solo"
     , Option ['t']     ["tests"]   (NoArg Test)          "perform routine check"
     ]

    
nomicOpts :: [String] -> IO ([Flag], [String])
nomicOpts argv = 
       case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: nomic [OPTION...]"







