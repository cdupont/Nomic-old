-- | This module starts a Interpreter server that will read our strings representing rules to convert them to plain Rules.
module Interpret(startInterpreter, readNamedRule, maybeReadRule) where

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Server
import Control.Monad()
import Paths_Nomic

import Control.Monad.State
import Language.Nomic.Expression
import Comm

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
   setImports ["Language.Nomic.Rule", "Language.Nomic.Expression", "Test", "Examples", "GHC.Base", "Data.Maybe"]
   return ()

-- | reads maybe a Rule out of a string.
interpretRule :: String -> ServerHandle -> IO (Either InterpreterError RuleFunc)
interpretRule s sh = do
   liftIO $ runIn sh (interpret s (as :: RuleFunc))

-- | maybe reads a Rule.
maybeReadRule :: String -> ServerHandle -> IO (Maybe RuleFunc)
maybeReadRule sr sh = do
   ir <- interpretRule sr sh
   case ir of
      Right r -> return $ Just r
      Left e -> do
         putStrLn $ "Your rule is not well formed\n" ++ show e
         return Nothing

-- | reads a Rule out of a string with possibly an error.
--readRule :: String -> Rule
--readRule expr = maybe (error "Rule ill-formed. This shouldn't have happened at this stage.") id (maybeReadRule expr)

-- | maybe reads a Rule function out of a Rule.
maybeReadNamedRule :: Rule -> ServerHandle -> IO (Maybe RuleFunc)
maybeReadNamedRule r sh = maybeReadRule (rRuleCode r) sh

-- | reads a Rule. May produce an error if badly formed.
readRule :: String -> ServerHandle -> IO RuleFunc
readRule sr sh = do
   ir <- interpretRule sr sh
   case ir of
      Right r -> return r
      Left e -> error $ "errReadRule: Rule is ill-formed. Shouldn't have happened.\n" ++ show e

-- | reads a NamedRule. May produce an error if badly formed.
readNamedRule :: Rule -> ServerHandle -> IO RuleFunc
readNamedRule r sh = readRule (rRuleCode r) sh



--safeReadRule :: String -> Comm Rule
--safeReadRule p = do
--   putCom p
--   text <- getCom
--   mRule <- liftIO $ getRule text
--   case mRule of
--      Just rule  -> return rule 
--      Nothing -> (putCom $ "Votre Regle est mal formee. Veuillez recommencer votre saisie.\n") >> safeReadRule p
