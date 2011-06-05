-- | This module starts a Interpreter server that will read our strings representing rules to convert them to plain Rules.
module Interpret where

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Server
import Control.Monad()
import Paths_Nomic
import Rule
import Comm
import Control.Monad.State
import NamedRule


-- | the server handle
startInterpreter :: IO ServerHandle
startInterpreter = do
   h <- start
   ir <- runIn h initializeInterpreter
   case ir of
      Right r -> do
         putStrLn "Interpreter Loaded"
         return $ Just r
      Left e -> error $ "sHandle: initialization error:\n" ++ show e
   return h

-- | initializes the interpreter by loading some modules.
initializeInterpreter :: Interpreter ()
initializeInterpreter = do
   dataDir <- liftIO getDataDir
   set [searchPath := [dataDir]]
   --loadModules ["Rule", "Observable"]
   setImports ["Rule", "Observable", "GHC.Base"]

   return ()

-- | reads maybe a Rule out of a string.
interpretRule :: String -> Comm (Either InterpreterError Rule)
interpretRule s = do
   sh <- gets hserver
   lift $ runIn sh (interpret s (as :: Rule))

-- | maybe reads a Rule.
maybeReadRule :: String -> Comm (Maybe Rule)
maybeReadRule sr = do
   ir <- interpretRule sr
   case ir of
      Right r -> return $ Just r
      Left e -> do
         putCom $ "Your rule is ill-formed\n" ++ show e
         return Nothing

-- | reads a Rule out of a string with possibly an error.
--readRule :: String -> Rule
--readRule expr = maybe (error "Rule ill-formed. This shouldn't have happened at this stage.") id (maybeReadRule expr)

-- | maybe reads a Rule out of a NamedRule.
maybeReadNamedRule :: NamedRule -> Comm (Maybe Rule)
maybeReadNamedRule = maybeReadRule . rRule

-- | reads a Rule. May produce an error if badly formed.
readRule :: String -> Comm Rule
readRule sr = do
   ir <- interpretRule sr
   case ir of
      Right r -> return r
      Left e -> error $ "errReadRule: Rule is ill-formed. Shouldn't have happened.\n" ++ show e

-- | reads a NamedRule. May produce an error if badly formed.
readNamedRule :: NamedRule -> Comm Rule
readNamedRule = readRule . rRule



--safeReadRule :: String -> Comm Rule
--safeReadRule p = do
--   putCom p
--   text <- getCom
--   mRule <- liftIO $ getRule text
--   case mRule of
--      Just rule  -> return rule 
--      Nothing -> (putCom $ "Votre Regle est mal formee. Veuillez recommencer votre saisie.\n") >> safeReadRule p
