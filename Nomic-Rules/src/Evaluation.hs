{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, GADTs #-}

module Evaluation where

import Expression
import Control.Monad
import Control.Applicative
import Control.Monad.State.Class
import Data.Maybe
import Control.Monad.State.Lazy
import Data.List

-- Evaluator on rules
-- | This type is usefull for rules evaluation:
-- it allows modifications of a game
-- it allows IO on players handles
-- its return type is either the value calculated, or a list of remaining actions.
type Evaluator a = StateT Game Comm (Either [Action] a)

-- | Combined lifters for Evaluator
liftE  = liftM  . liftA
liftE2 = liftM2 . liftA2
liftE3 = liftM3 . liftA3

evalExp :: Exp a -> RuleNumber -> Evaluator a
evalExp (NewVar name def) rn = do
    vars <- gets variables
    case find (\(a, myName, b) -> myName == name) vars of
       Nothing -> do
          modify (\game -> game { variables = (rn, name, def) : vars})
          return $ pure True
       Just _ -> return $ pure False

evalExp (DelVar name) _ = do
    vars <- gets variables
    case find (\(a, myName, b) -> myName == name) vars of
       Nothing -> return $ pure False
       Just _ -> do
          modify (\g -> g { variables = filter (\(a, myName, b) -> myName /= name) vars})
          return $ pure True

evalExp (ReadVar name) rn = do
    vars <- gets variables
    let var = find (\(_, myName, val) -> myName == name) vars
    case var of
       Nothing -> return $ pure Nothing
       Just (_, _, val) -> return $ pure $ Just val


evalExp (WriteVar name val) rn = do
    vars <- gets variables
    let newVars = replaceWith (\(rn, n, v) -> n == name) (rn, name, val) vars
    case find (\(a, myName, b) -> myName == name) vars of
       Nothing -> return $ pure False
       Just _ -> do
          modify (\game -> game { variables = newVars})
          return $ pure True

evalExp (OnEvent event exp) rn = do
    evs <- gets events
    modify (\game -> game { events = (rn, event, exp) : evs})
    return $ pure ()

--send a message to another rule.
evalExp (SendMessage string) rn = do
    triggerEvent $ Message string
    return $ pure ()

--send a message to another rule.
evalExp (Output pn string) rn = do
    modify (\game -> game { outputs = (pn, string) : (outputs game)})
    return $ pure ()

--TODO: more sanity check? rule in active?
evalExp (AddRule rule) rn = do
    rs <- gets rules
    case find (\Rule { rNumber = rn} -> rn == rNumber rule) rs of
       Nothing -> do
          modify (\game -> game { rules = rule : rs})
          triggerEvent $ RuleAdded $ rNumber rule
          return $ pure True
       Just _ -> return $ pure False

evalExp (DelRule del) rn = do
    rs <- gets rules
    case find (\Rule { rNumber = myrn} -> myrn == del) rs of
       Nothing -> return $ pure False
       Just r -> do
          let newrules = replaceWith (\Rule { rNumber = myrn} -> myrn == del) r {rStatus = Suppressed, rejectedBy = Just rn} rs
          modify (\g -> g { rules = newrules})
          triggerEvent $ RuleSuppressed $ rNumber r
          return $ pure True

--TODO: check the rn inside the rule? trigger an event?
evalExp (ModifyRule mod rule) rn = do
    rs <- gets rules
    let newRules = replaceWith (\Rule { rNumber = myrn} -> myrn == mod) rule rs
    case find (\Rule { rNumber = myrn} -> myrn == mod) rs of
       Nothing -> return $ pure False
       Just r ->  do
          modify (\game -> game { rules = newRules})
          triggerEvent $ RuleModified $ rNumber r
          return $ pure True

evalExp GetRules rn = return . pure =<< gets rules
evalExp GetPlayers rn = return . pure =<< gets players

evalExp (SetVictory ps) rn = do
    modify (\game -> game { victory = ps})
    triggerEvent $ Victory ps
    return $ pure ()

evalExp (Expression.Const a)  _ = return $ pure a

evalExp (Bind exp f) rn = do
   eitherA <- evalExp exp rn
   case eitherA of
      Right a -> evalExp (f a) rn
      Left as -> return $ Left as


--TODO: verify if there is no ambiguity in the search for action result
evalExp (InputChoice or opn ocs) rn = do
   g <- get
   eAction <- liftE3 ActionType (evalExp or rn) (evalExp opn rn) (evalExp ocs rn)
   return $ case eAction of
      Right action ->
         case findActionResult action rn (actionResults g) of  --TODO: vérifications d'usage: nb players etc.
            Just r -> Right $ fromMaybe (error "evalObs: Action result should be fulfilled at this stage.") (result r)
            Nothing -> (Left $ [Action rn action Nothing])
      Left a -> Left a


triggerEvent :: EventEnum -> Evaluator ()
triggerEvent e = do
    evs <- gets events
    let filtered = filter (\(_, ev, _) -> ev == e) evs
    mapM_ (\(rn, ev, exp) -> evalExp exp rn) filtered
    return $ pure ()


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
