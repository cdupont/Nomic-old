
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | This module defines an abstraction to be able to make input output to each clients
module Comm where

import Prelude hiding (catch)
import Control.Monad.State
import System.IO.Error hiding (catch)
import Control.Monad.CatchIO
import Language.Haskell.Interpreter.Server
import Control.Concurrent.STM
import Happstack.State (update, query)


-- | A data type to hide away communication functions.
data Communication = Communication {cin :: TChan String, cout :: TChan String, hserver :: ServerHandle}

-- | A State monad used to avoid passing all around a Handle on which performing IO.
-- Comm must be used in replacement for IO in return types.
type Comm = StateT Communication IO

-- | An helper function that makes it very clear how to use the state transformer Comm.
runWithComm :: Communication -> Comm a -> IO a
runWithComm = flip evalStateT

-- | run a Comm beeing StdIO
--runWithStdIO :: IO ServerHandle -> Comm a -> IO a
--runWithStdIO sh c = do
--   mysh <- sh
--   runWithComm (defaultComm mysh) c



-- helper functions to avoid nesting too much lifts.

-- | Equivalent to putStr but with the handle contained in Comm.
putCom :: String -> Comm ()
putCom s = do
   cout <- gets cout
   lift $ atomically $ writeTChan cout (s ++ "\r\n")


-- | Equivalent to getLine but with the handle contained in Comm.
getCom :: Comm String
getCom = do
   cin <- gets cin
   lift $ atomically $ readTChan cin


-- | issue a message before input.
putGetComm :: String -> Comm String
putGetComm s = putCom s >> getCom


-- | Equivalent to readLn but with the handle contained in Comm.
-- it will raise an exception if reading is not possible.  
readLnComm :: Read a => Comm a
readLnComm = do
   a <- getCom
   r <- lift $ readIO a
   return r

-- | this function will ask you to re-enter your data if it cannot cast it to an a.
safeRead :: (Read a) => String -> Comm a
safeRead p = do
   putCom p
   catch readLnComm eHandler where
      eHandler e 
         | isUserError e = (putCom $ "Read error. Please try again.\n") >> safeRead p
         | otherwise     = lift $ ioError e


getComm h = Communication h h

-- | generic function to say things on transformers like GameState, ServerState etc.
say :: String -> StateT a Comm ()
say = lift . putCom

queryComm = lift $ query
updateComm = lift $ update
