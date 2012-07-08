{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, GADTs #-}

module Evaluation where

import Expression
import Control.Monad
import Control.Monad.State.Class
import Data.Maybe
import Control.Monad.State.Lazy
import Data.List
import Data.Typeable
import Data.Maybe


evalExp :: Exp a -> RuleNumber -> State Game a
evalExp (NewVar name def) rn = do
    vars <- gets variables
    case find (\(Var a myName b) -> myName == name) vars of
       Nothing -> do
          modify (\game -> game { variables = (Var rn name def) : vars})
	  return True
       Just _ -> return False


evalExp (DelVar name) _ = do
    vars <- gets variables
    case find (\(Var a myName b) -> myName == name) vars of
       Nothing -> return False
       Just _ -> do
          modify (\g -> g { variables = filter (\(Var a myName b) -> myName /= name) vars})
	  return True

evalExp (ReadVar name) rn = do
    vars <- gets variables
    let var = find (\(Var _ myName val) -> myName == name) vars
    case var of
       Nothing -> return Nothing
       Just (Var _ _ val) -> case cast val of
		   Just v -> return $ Just v
		   Nothing -> return Nothing


evalExp (WriteVar name val) rn = do
    vars <- gets variables
    let newVars = replaceWith (\(Var rn n v) -> n == name) (Var rn name val) vars
    case find (\(Var a myName b) -> myName == name) vars of
       Nothing -> return False
       Just _ -> do
          modify (\game -> game { variables = newVars})
          return True

evalExp (OnEvent event handler) rn = do
    evs <- gets events
    modify (\game -> game { events = (EH rn event handler) : evs})
    return ()

--send a message to another rule.
evalExp (SendMessage id myData) rn = do
    triggerEvent (Message id) (MessageData myData)
    return ()

--send a message to another rule.
evalExp (Output pn string) rn = do
    modify (\game -> game { outputs = (pn, string) : (outputs game)})
    return ()

evalExp (ActivateRule rule) rn = do
    rs <- gets rules
    case find (\Rule { rNumber = myrn} -> myrn == rule) rs of
       Nothing -> return False
       Just r -> do
          let newrules = replaceWith (\Rule { rNumber = myrn, rStatus = Pending} -> myrn == rule) r {rStatus = Active} rs
          modify (\g -> g { rules = newrules})
          triggerEvent RuleAdded (RuleAddedData r)
          return True

evalExp (ProposeRule rule) rn = do
    rs <- gets rules
    case find (\Rule { rNumber = rn} -> rn == rNumber rule) rs of
       Nothing -> do
          modify (\game -> game { rules = rule : rs})
          triggerEvent RuleProposed (RuleProposedData rule)
          return True
       Just _ -> return False

evalExp (AddRule rule) rn = do
    rs <- gets rules
    case find (\Rule { rNumber = rn} -> rn == rNumber rule) rs of
       Nothing -> do
          modify (\game -> game { rules = rule : rs})
          triggerEvent RuleAdded (RuleAddedData rule)
          return True
       Just _ -> return False

evalExp (DelRule del) rn = do
    rs <- gets rules
    case find (\Rule { rNumber = myrn} -> myrn == del) rs of
       Nothing -> return False
       Just r -> do
          let newrules = replaceWith (\Rule { rNumber = myrn} -> myrn == del) r {rStatus = Suppressed, rejectedBy = Just rn} rs
          modify (\g -> g { rules = newrules})
          triggerEvent RuleSuppressed (RuleSuppressedData r)
          return True

--TODO: check the rn inside the rule? trigger an event?
evalExp (ModifyRule mod rule) rn = do
    rs <- gets rules
    let newRules = replaceWith (\Rule { rNumber = myrn} -> myrn == mod) rule rs
    case find (\Rule { rNumber = myrn} -> myrn == mod) rs of
       Nothing -> return False
       Just r ->  do
          modify (\game -> game { rules = newRules})
          triggerEvent RuleModified (RuleModifiedData r)
          return True

evalExp GetRules rn = gets rules
evalExp GetPlayers rn = gets players

evalExp (SetVictory ps) rn = do
    modify (\game -> game { victory = ps})
    pls <- gets players
    let victorious = filter (\pl -> playerNumber pl `elem` ps) pls
    triggerEvent Victory (VictoryData victorious)
    return ()

evalExp (Const a) _ = return a

evalExp (Bind exp f) rn = do
   a <- evalExp exp rn
   evalExp (f a) rn
{-

--TODO: verify if there is no ambiguity in the search for action result
--evalExp (InputChoice or opn ocs) rn = do
--   g <- get
--   eAction <- liftE3 ActionType (evalExp or rn) (evalExp opn rn) (evalExp ocs rn)
--   return $ case eAction of
--      Right action ->
--         case findActionResult action rn (actionResults g) of  --TODO: vérifications d'usage: nb players etc.
--            Just r -> Right $ fromMaybe (error "evalObs: Action result should be fulfilled at this stage.") (result r)
--            Nothing -> (Left $ [Action rn action Nothing])
--      Left a -> Left a

-}
--execute all the handlers of the specified event with the given data
triggerEvent :: (Event e) => e -> (EventData e) -> State Game ()
triggerEvent e dat = do
    evs <- gets events
    let filtered = filter (\(EH _ ev _) -> cast e == Just ev) evs
    mapM_ f filtered where
        f (EH rn _ h) = case cast h of
            Just castedH -> evalExp (castedH dat) rn
            Nothing -> return ()


-- | find the result of an action (the Exp) in the list.
findActionResult :: ActionType -> RuleNumber -> [Action] -> Maybe Action
findActionResult a ruleNumber as = find (\Action { aRuleNumber = rn,
                                                   action = action} -> ruleNumber == rn &&
                                                                       action == a) as

-- instances


--instance Monoid a => Applicative (Either a) where
--        pure x = Right x
--        (Right f) <*> (Right x) = Right $ f x
--        (Right _) <*> (Left u) = Left u
--        (Left u) <*> (Right _) = Left u
--        (Left u) <*> (Left v) = Left $ u `mappend` v

instance Monad (Either [Action]) where
        return = Right
        (Right x) >>= f = f x
        (Left u) >>= _ = Left u
