{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, GADTs, NamedFieldPuns #-}

module Evaluation where

import Expression
import Control.Monad
import Control.Monad.State.Class
import Data.Maybe
import Control.Monad.State.Lazy
import Data.List
import Data.Typeable
import Data.Maybe
import Data.Function
import Data.Time

evalExp :: Exp a -> RuleNumber -> State Game a
evalExp (NewVar name def) rn = do
    vars <- gets variables
    case find (\(Var _ myName _) -> myName == name) vars of
       Nothing -> do
          modify (\game -> game { variables = (Var rn name def) : vars})
          return $ Just (V name)
       Just _ -> return Nothing


evalExp (DelVar (V name)) _ = do
    vars <- gets variables
    case find (\(Var a myName b) -> myName == name) vars of
       Nothing -> return False
       Just _ -> do
          modify (\g -> g { variables = filter (\(Var a myName b) -> myName /= name) vars})
          return True

evalExp (ReadVar (V name)) rn = do
    vars <- gets variables
    let var = find (\(Var _ myName val) -> myName == name) vars
    case var of
       Nothing -> return Nothing
       Just (Var _ _ val) -> case cast val of
           Just v -> return $ Just v
           Nothing -> return Nothing


evalExp (WriteVar (V name) val) rn = do
    vars <- gets variables
    let newVars = replaceWith (\(Var rn n v) -> n == name) (Var rn name val) vars
    case find (\(Var a myName b) -> myName == name) vars of
       Nothing -> return False
       Just _ -> do
          modify (\game -> game { variables = newVars})
          return True

evalExp (OnEvent event handler) rn = do
    evs <- gets events
    let en = getFreeNumber (map eventNumber evs)
    modify (\game -> game { events = (EH en rn event handler) : evs})
    return en

evalExp (DelEvent en) _ = do
    evs <- gets events
    case find (\EH {eventNumber} -> eventNumber == en) evs of
       Nothing -> return False
       Just _ -> do
          modify (\g -> g { events = filter (\EH {eventNumber} -> eventNumber /= en) evs})
          return True

--send a message to another rule.
evalExp (SendMessage (Message id) myData) rn = do
    triggerEvent (Message id) (MessageData myData)
    return ()

--send a message to another rule.
evalExp (Output pn string) rn = outputS pn string >> return ()

evalExp (ActivateRule rule) rn = evActivateRule rule rn

evalExp (RejectRule rule) rn = evRejectRule rule rn

evalExp (ProposeRule rule) _ = evProposeRule rule

evalExp (AddRule rule) _ = evAddRule rule

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
    --outputS 1 ("trigger event " ++ (show e))
    evs <- gets events
    let filtered = filter (\(EH {event}) -> e === event) evs
    mapM_ f filtered where
        f (EH {ruleNumber, eventNumber, handler}) = case cast handler of
            Just castedH -> do
                --outputS 1 ("event found " ++ (show e))
                evalExp (castedH (eventNumber, dat)) ruleNumber
            Nothing -> outputS 1 ("failed " ++ (show $ typeOf handler))


-- | find the result of an action (the Exp) in the list.
findActionResult :: ActionType -> RuleNumber -> [Action] -> Maybe Action
findActionResult a ruleNumber as = find (\Action { aRuleNumber = rn,
                                                   action = action} -> ruleNumber == rn &&
                                                                       action == a) as


outputS :: PlayerNumber -> String -> State Game ()
outputS pn s = modify (\game -> game { outputs = (pn, s) : (outputs game)})


getFreeNumber :: (Num a, Enum a) => [a] -> a
getFreeNumber l = head [a| a <- [1..], not $ a `elem` l]


evProposeRule :: Rule -> State Game Bool
evProposeRule rule = do
    rs <- gets rules
    case find (\Rule { rNumber = rn} -> rn == rNumber rule) rs of
       Nothing -> do
          modify (\game -> game { rules = rule : rs})
          triggerEvent RuleProposed (RuleProposedData rule)
          return True
       Just _ -> return False


evAddRule :: Rule -> State Game Bool
evAddRule rule = do
    rs <- gets rules
    case find (\Rule { rNumber = rn} -> rn == rNumber rule) rs of
       Nothing -> do
          modify (\game -> game { rules = rule : rs})
          triggerEvent RuleAdded (RuleAddedData rule)
          return True
       Just _ -> return False


evActivateRule :: RuleNumber -> RuleNumber -> State Game Bool
evActivateRule rn by = do
    rs <- gets rules
    case find (\Rule { rNumber = myrn} -> myrn == rn) rs of
       Nothing -> return False
       Just r -> do
          let newrules = replaceWith (\Rule { rNumber = myrn} -> myrn == rn) r {rStatus = Active} rs
          modify (\g -> g { rules = newrules})
          --execute the rule
          case rRuleFunc r of
             (VoidRule vr) -> evalExp vr rn
             _ -> return ()
          triggerEvent RuleModified (RuleModifiedData r)
          return True

evRejectRule :: RuleNumber -> RuleNumber -> State Game Bool
evRejectRule rule by = do
    rs <- gets rules
    case find (\Rule { rNumber = myrn} -> myrn == rule) rs of
       Nothing -> return False
       Just r -> do
          let newrules = replaceWith (\Rule { rNumber = myrn} -> myrn == rule) r {rStatus = Rejected, rejectedBy = Just by} rs
          modify (\g -> g { rules = newrules})
          triggerEvent RuleModified (RuleModifiedData r)
          return True


addPlayer :: PlayerInfo -> State Game Bool
addPlayer pi@(PlayerInfo {playerNumber = pn}) = do
    pls <- gets players
    let exists = any (((==) `on` playerNumber) pi) pls
    when (not exists) $ do
        modify (\game -> game { players = pi : pls})
        triggerEvent PlayerArrive (PlayerArriveData pi)
    return $ not exists


evInputChoice :: (Enum d, Typeable d) => InputChoice d -> d -> State Game ()
evInputChoice ic d = triggerEvent ic (InputChoiceData d)

evTriggerTime :: UTCTime -> State Game ()
evTriggerTime t = triggerEvent (Time t) (TimeData t)



instance Monad (Either [Action]) where
        return = Right
        (Right x) >>= f = f x
        (Left u) >>= _ = Left u

-- | Replaces all instances of a value in a list by another value.
replaceWith :: (a -> Bool)   -- ^ Value to search
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replaceWith f y = map (\z -> if f z then y else z)
