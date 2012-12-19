{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, GADTs, NamedFieldPuns, ScopedTypeVariables #-}

module Language.Nomyx.Evaluation where

import Language.Nomyx.Expression
import Control.Monad
import Control.Monad.State.Class
import Data.Maybe
import Control.Monad.State.Lazy
import Data.List
import Data.Typeable
import Data.Maybe
import Data.Function
import Data.Time
import Debug.Trace
import Unsafe.Coerce

-- | evaluate an expression.
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

evalExp (DelAllEvents e) _ = do
    evs <- gets events
    modify (\g -> g { events = filter (\EH {event} -> not $ event === e) evs})


evalExp (SendMessage (Message id) myData) rn = do
    triggerEvent (Message id) (MessageData myData)
    return ()

evalExp (Output pn string) rn = outputS pn string >> return ()
evalExp (ProposeRule rule) _ = evProposeRule rule
evalExp (ActivateRule rule) rn = evActivateRule rule rn
evalExp (RejectRule rule) rn = evRejectRule rule rn
evalExp (AddRule rule) _ = evAddRule rule
evalExp (DelRule del) _ = evDelRule del
evalExp (ModifyRule mod rule) rn = evModifyRule mod rule
evalExp GetRules rn = gets rules
evalExp GetPlayers rn = gets players
evalExp SelfRuleNumber rn = return rn
evalExp (SetVictory ps) rn = do
    modify (\game -> game { victory = ps})
    pls <- gets players
    let victorious = filter (\pl -> playerNumber pl `elem` ps) pls
    triggerEvent Victory (VictoryData victorious)
    return ()

evalExp (CurrentTime) _ = gets currentTime
evalExp (Const a) _ = return a
evalExp (Bind exp f) rn = do
   a <- evalExp exp rn
   evalExp (f a) rn


--execute all the handlers of the specified event with the given data
triggerEvent :: (Typeable e, Show e, Eq e) => Event e -> EventData e -> State Game ()
triggerEvent e dat = do
    --traceState ("trigger event " ++ (show e))
    --traceState ("EventData " ++ (show dat))
    evs <- gets events
    let filtered = filter (\(EH {event}) -> e === event) evs
    mapM_ f filtered where
        f (EH {ruleNumber, eventNumber, handler}) = case cast handler of
            Just castedH -> do
                traceState ("event found " ++ (show e))
                evalExp (castedH (eventNumber, dat)) ruleNumber
            Nothing -> outputS 1 ("failed " ++ (show $ typeOf handler))

triggerChoice :: Int -> Int -> State Game ()
triggerChoice myEventNumber choiceIndex = do
    evs <- gets events
    let filtered = filter (\(EH {eventNumber}) -> eventNumber == myEventNumber) evs
    mapM_ (execChoiceHandler myEventNumber choiceIndex) filtered
    return ()

execChoiceHandler :: EventNumber -> Int -> EventHandler -> State Game ()
execChoiceHandler eventNumber choiceIndex (EH _ _ (InputChoice ruleNumber _ cs _) handler) = evalExp (handler (eventNumber, InputChoiceData (cs!!choiceIndex))) ruleNumber
execChoiceHandler _ _ _ = return ()


--execute all the handlers of the specified event with the given data
findEvent ::EventNumber -> [EventHandler] -> Maybe (EventHandler)
findEvent en evs = find (\(EH {eventNumber}) -> en == eventNumber) evs


findChoice :: (Eq a, Read a) => String -> Event (InputChoice a) -> a
findChoice s (InputChoice _ _ choices _) = fromJust $ find (== (read s)) choices

outputS :: PlayerNumber -> String -> State Game ()
outputS pn s = modify (\game -> game { outputs = (pn, s) : (outputs game)})

getFreeNumber :: (Eq a, Num a, Enum a) => [a] -> a
getFreeNumber l = head [a| a <- [1..], not $ a `elem` l]


evProposeRule :: Rule -> State Game Bool
evProposeRule rule = do
    rs <- gets rules
    case find (\Rule { rNumber = rn} -> rn == rNumber rule) rs of
       Nothing -> do
          modify (\game -> game { rules = rule : rs})
          triggerEvent (RuleEv Proposed) (RuleData rule)
          return True
       Just _ -> return False

--Sets the rule status to Active and execute it if possible
evActivateRule :: RuleNumber -> RuleNumber -> State Game Bool
evActivateRule rn by = do
    rs <- gets rules
    case find (\Rule { rNumber = myrn} -> myrn == rn) rs of
       Nothing -> return False
       Just r -> do
          let newrules = replaceWith (\Rule { rNumber = myrn} -> myrn == rn) r {rStatus = Active, rAssessedBy = Just by} rs
          modify (\g -> g { rules = newrules})
          --execute the rule
          case rRuleFunc r of
             (VoidRule vr) -> evalExp vr rn
             _ -> return ()
          triggerEvent (RuleEv Activated) (RuleData r)
          return True

evRejectRule :: RuleNumber -> RuleNumber -> State Game Bool
evRejectRule rule by = do
    rs <- gets rules
    case find (\Rule { rNumber = myrn} -> myrn == rule) rs of
       Nothing -> return False
       Just r -> do
          let newrules = replaceWith (\Rule { rNumber = myrn} -> myrn == rule) r {rStatus = Reject, rAssessedBy = Just by} rs
          modify (\g -> g { rules = newrules})
          triggerEvent (RuleEv Rejected) (RuleData r)
          delVarsRule rule
          delEventsRule rule
          return True

evAddRule :: Rule -> State Game Bool
evAddRule rule = do
    rs <- gets rules
    case find (\Rule { rNumber = rn} -> rn == rNumber rule) rs of
       Nothing -> do
          modify (\game -> game { rules = rule : rs})
          triggerEvent (RuleEv Added) (RuleData rule)
          return True
       Just _ -> return False

evDelRule :: RuleNumber -> State Game Bool
evDelRule del = do
    rs <- gets rules
    case find (\Rule { rNumber = myrn} -> myrn == del) rs of
       Nothing -> return False
       Just r -> do
          let newrules = filter (\Rule {rNumber} -> rNumber /= del) rs
          modify (\g -> g { rules = newrules})
          delVarsRule del
          delEventsRule del
          triggerEvent (RuleEv Deleted) (RuleData r)
          return True

--TODO: clean and execute new rule
evModifyRule :: RuleNumber -> Rule -> State Game Bool
evModifyRule mod rule = do
    rs <- gets rules
    let newRules = replaceWith (\Rule { rNumber = myrn} -> myrn == mod) rule rs
    case find (\Rule { rNumber = myrn} -> myrn == mod) rs of
       Nothing -> return False
       Just r ->  do
          modify (\game -> game { rules = newRules})
          triggerEvent (RuleEv Modified) (RuleData r)
          return True

addPlayer :: PlayerInfo -> State Game Bool
addPlayer pi@(PlayerInfo {playerNumber = pn}) = do
    pls <- gets players
    let exists = any (((==) `on` playerNumber) pi) pls
    when (not exists) $ do
        modify (\game -> game { players = pi : pls})
        triggerEvent (Player Arrive) (PlayerData pi)
    return $ not exists

delPlayer :: PlayerInfo -> State Game Bool
delPlayer pi@(PlayerInfo {playerNumber = pn}) = do
    pls <- gets players
    let exists = any (((==) `on` playerNumber) pi) pls
    when exists $ do
        modify (\game -> game { players = filter ((/= pn) . playerNumber) pls})
        triggerEvent (Player Leave) (PlayerData pi)
    return exists

evInputChoice :: (Eq d, Show d, Typeable d, Read d) => Event(InputChoice d) -> d -> State Game ()
evInputChoice ic d = triggerEvent ic (InputChoiceData d)

evTriggerTime :: UTCTime -> State Game ()
evTriggerTime t = do
    triggerEvent (Time t) (TimeData t)


--delete all variables of a rule
delVarsRule :: RuleNumber -> State Game ()
delVarsRule rn = do
    vars <- gets variables
    modify (\g -> g { variables = filter (\(Var myrn _ _) -> myrn /= rn) vars})

--delete all events of a rule
delEventsRule :: RuleNumber -> State Game ()
delEventsRule rn = do
    evs <- gets events
    modify (\g -> g { events = filter (\(EH {ruleNumber = myrn}) -> myrn /= rn) evs})

-- | Replaces all instances of a value in a list by another value.
replaceWith :: (a -> Bool)   -- ^ Value to search
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replaceWith f y = map (\z -> if f z then y else z)

traceState :: String -> State s String
traceState x = state (\s -> trace ("trace: " ++ x) (x, s))

