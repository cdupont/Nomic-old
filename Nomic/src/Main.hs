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

module Main (

main

) where

--ifdef mingw32_BUILD_OS
--define WINDOWS
--endif

import Mono
import System.Console.GetOpt 
import System.Environment 
import Data.Maybe
import Server hiding (Server)
-- import Test
import Web
import Happstack.State
import Multi
import Control.Concurrent

-- | Entry point of the program.
main :: IO Bool
main = do
   putStrLn "Welcome to Haskell Nomic"
   args <- getArgs 
   (flags, _) <- nomicOpts args --startNomic 
   --parseActions flags
   --let verbose = Verbose `elem` flags
   if Test `elem` flags
      then do
         --at <- allTests
         --putStrLn $ "Test result: " ++ show at
         return False --at
      else do
         if Solo `elem` flags
            then putStrLn "out" --runWithStdIO sHandle startNomicMono
            else do
               --start MACID system state containning the Multi
               c <- localStartSystemState (Proxy :: Proxy Multi)
               forkIO $ serverStart 10000
               forkIO $ launchWebServer
               (serverLoop c) `finally` (createCheckpointAndShutdown c)

         return True

createCheckpointAndShutdown :: MVar TxControl -> IO ()
createCheckpointAndShutdown control = do
   putStrLn "Creating checkpoint and quiting..."
   createCheckpoint control
   shutdownSystem control

localsaver :: IO Saver
localsaver = do
   pn <- getProgName
   d <- getDataDir
   return $ Queue (FileSaver (d ++ "_local/" ++pn++"_state"))

localStartSystemState :: (Methods a, Component a) => Proxy a -> IO (MVar TxControl)
localStartSystemState proxy = do
   saver <- localsaver
   runTxSystem saver proxy


   -- | a loop that will handle server commands
serverLoop :: MVar TxControl -> IO ()
serverLoop c = do
   s <- getLine
   case s of
      "s" -> do
         putStrLn "saving state..."
         createCheckpoint c
         serverLoop c
      "q" -> return ()
      _ -> do
         putStrLn "command not recognized"
         serverLoop c

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

parseActions :: [Flag] -> IO ()
parseActions fs = sequence_ $ catMaybes $ map lookupFlag fs where
   lookupFlag f = lookup f actions 

actions :: [(Flag, IO ())]
actions = [(Version, putStrLn "Nomic V0.1\n"),
           (Test, putStrLn "Tests OK\n")] 
    
nomicOpts :: [String] -> IO ([Flag], [String])
nomicOpts argv = 
       case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: nomic [OPTION...]"







