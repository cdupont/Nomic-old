{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, GADTs, NamedFieldPuns, ScopedTypeVariables, DoAndIfThenElse #-}

module Language.Nomyx.Evaluation where

import Prelude hiding ((.))
import Language.Nomyx.Expression
import Control.Monad
import Control.Monad.State.Class
import Data.Maybe
import Control.Monad.State.Lazy
import Data.List
import Data.Typeable
import Data.Function hiding ((.))
import Data.Time
import Debug.Trace
import Debug.Trace.Helpers
import Data.Lens
import Control.Category

-- | evaluate an expression.
-- The rule number passed is the number of the rule containing the expression.
evalExp :: Nomex a -> RuleNumber -> State Game a
evalExp (NewVar name def) rn = do
    vars <- access variables
    case find ((== name) . getL vName) vars of
       Nothing -> do
          variables %= ((Var rn name def) : )
          return $ Just (V name)
       Just _ -> return Nothing


evalExp (DelVar (V name)) _ = do
    vars <- access variables
    case find ((== name) . getL vName) vars of
       Nothing -> return False
       Just _ -> do
          variables %= filter ((/= name) . getL vName)
          return True

evalExp (ReadVar (V name)) _ = do
    vars <- access variables
    let var = find ((== name) . getL vName) vars
    case var of
       Nothing -> return Nothing
       Just (Var _ _ val) -> case cast val of
           Just v -> return $ Just v
           Nothing -> return Nothing


evalExp (WriteVar (V name) val) rn = do
    vars <- access variables
    let newVars = replaceWith ((== name) . getL vName) (Var rn name val) vars
    case find (\(Var _ myName _) -> myName == name) vars of
       Nothing -> return False
       Just _ -> do
          variables ~= newVars
          return True

evalExp (OnEvent event handler) rn = do
    evs <- access events
    let en = getFreeNumber (map _eventNumber evs)
    events %= ((EH en rn event handler) : )
    return en

evalExp (DelEvent en) rn = do
    evs <- access events
    case find ((== en) . getL eventNumber) evs of
       Nothing -> return False
       Just eh -> do
          if ((ruleNumber ^$) eh == rn) then do
             events %= filter ((/= en) . getL eventNumber)
             return True
          else return False

evalExp (SendMessage (Message id) myData) _ = do
    triggerEvent (Message id) (MessageData myData)
    return ()

evalExp (DelAllEvents e)      _  = void $ events %= filter (\EH {event} -> not $ event === e)
evalExp (Output pn string)    _  = outputS pn string >> return ()
evalExp (ProposeRule rule)    _  = evProposeRule rule
evalExp (ActivateRule rule)   rn = evActivateRule rule rn
evalExp (RejectRule rule)     rn = evRejectRule rule rn
evalExp (AddRule rule)        _  = evAddRule rule
evalExp (DelRule del)         _  = evDelRule del
evalExp (ModifyRule mod rule) _  = evModifyRule mod rule
evalExp GetRules              _  = access rules
evalExp GetPlayers            _  = access players
evalExp SelfRuleNumber        rn = return rn
evalExp (CurrentTime)         _  = access currentTime
evalExp (Const a)             _  = return a
evalExp (SetVictory ps)       _  = do
    victory ~= ps
    pls <- access players
    let victorious = filter (\pl -> _playerNumber pl `elem` ps) pls
    triggerEvent Victory (VictoryData victorious)
    return ()


evalExp (Bind exp f) rn = do
   a <- evalExp exp rn
   evalExp (f a) rn


--execute all the handlers of the specified event with the given data
triggerEvent :: (Typeable e, Show e, Eq e) => Event e -> EventData e -> State Game ()
triggerEvent e dat = do
    --traceState ("trigger event " ++ (show e))
    --traceState ("EventData " ++ (show dat))
    evs <- access events
    let filtered = filter (\(EH {event}) -> e === event) evs
    mapM_ f filtered where
        f (EH {_ruleNumber, _eventNumber, handler}) = case cast handler of
            Just castedH -> do
                --traceState ("event found " ++ (show e))
                evalExp (castedH (_eventNumber, dat)) _ruleNumber
            Nothing -> outputS 1 ("failed " ++ (show $ typeOf handler))

triggerChoice :: Int -> Int -> State Game ()
triggerChoice myEventNumber choiceIndex = do
    evs <- access events
    let filtered = filter ((== myEventNumber) . getL eventNumber) evs
    mapM_ (execChoiceHandler myEventNumber choiceIndex) filtered


execChoiceHandler :: EventNumber -> Int -> EventHandler -> State Game ()
execChoiceHandler eventNumber choiceIndex (EH _ _ (InputChoice ruleNumber _ cs _) handler) = evalExp (handler (eventNumber, InputChoiceData (cs!!choiceIndex))) ruleNumber
execChoiceHandler _ _ _ = return ()


--execute all the handlers of the specified event with the given data
findEvent :: EventNumber -> [EventHandler] -> Maybe (EventHandler)
findEvent en evs = find ((== en) . getL eventNumber) evs


findChoice :: (Eq a, Read a) => String -> Event (InputChoice a) -> a
findChoice s (InputChoice _ _ choices _) = fromJust $ find (== (read s)) choices

outputS :: PlayerNumber -> String -> State Game ()
outputS pn s = void $ outputs %= ((pn, s) : )

getFreeNumber :: (Eq a, Num a, Enum a) => [a] -> a
getFreeNumber l = head [a| a <- [1..], not $ a `elem` l]


evProposeRule :: Rule -> State Game Bool
evProposeRule rule = do
    rs <- access rules
    case find ((== (rNumber ^$ rule)) . getL rNumber) rs of
       Nothing -> do
          rules %= (rule:)
          triggerEvent (RuleEv Proposed) (RuleData rule)
          return True
       Just _ -> return False

--Sets the rule status to Active and execute it if possible
evActivateRule :: RuleNumber -> RuleNumber -> State Game Bool
evActivateRule rn by = do
    rs <- access rules
    case find ((== rn) . getL rNumber) rs of
       Nothing -> return False
       Just r -> do
          let newrules = replaceWith ((== rn) . getL rNumber) r{_rStatus = Active, _rAssessedBy = Just by} rs
          rules ~= newrules
          --execute the rule
          evalExp (_rRuleFunc r) rn
          triggerEvent (RuleEv Activated) (RuleData r)
          return True

evRejectRule :: RuleNumber -> RuleNumber -> State Game Bool
evRejectRule rn by = do
    rs <- access rules
    case find ((== rn) . getL rNumber) rs of
       Nothing -> return False
       Just r -> do
          let newrules = replaceWith ((== rn) . getL rNumber) r{_rStatus = Reject, _rAssessedBy = Just by} rs
          rules ~= newrules
          triggerEvent (RuleEv Rejected) (RuleData r)
          delVarsRule rn
          delEventsRule rn
          return True

evAddRule :: Rule -> State Game Bool
evAddRule rule = do
    rs <- access rules
    case find ((== (rNumber ^$ rule)) . getL rNumber) rs of
       Nothing -> do
          rules %= (rule:)
          triggerEvent (RuleEv Added) (RuleData rule)
          return True
       Just _ -> return False

evDelRule :: RuleNumber -> State Game Bool
evDelRule del = do
    rs <- access rules
    case find ((== del) . getL rNumber) rs of
       Nothing -> return False
       Just r -> do
          let newRules = filter ((/= del) . getL rNumber) rs
          rules ~= newRules
          delVarsRule del
          delEventsRule del
          triggerEvent (RuleEv Deleted) (RuleData r)
          return True

--TODO: clean and execute new rule
evModifyRule :: RuleNumber -> Rule -> State Game Bool
evModifyRule mod rule = do
    rs <- access rules
    let newRules = replaceWith ((== mod) . getL rNumber) rule rs
    case find ((== mod) . getL rNumber) rs of
       Nothing -> return False
       Just r ->  do
          rules ~= newRules
          triggerEvent (RuleEv Modified) (RuleData r)
          return True

addPlayer :: PlayerInfo -> State Game Bool
addPlayer pi = do
    pls <- access players
    let exists = any (((==) `on` _playerNumber) pi) pls
    when (not exists) $ do
        players %= (pi:)
        triggerEvent (Player Arrive) (PlayerData pi)
    return $ not exists

delPlayer :: PlayerInfo -> State Game Bool
delPlayer pi = do
    pls <- access players
    let exists = any (((==) `on` _playerNumber) pi) pls
    when exists $ do
        players %= filter ((/= (playerNumber ^$ pi)) . getL playerNumber)
        triggerEvent (Player Leave) (PlayerData pi)
    return exists

evInputChoice :: (Eq d, Show d, Typeable d, Read d) => Event(InputChoice d) -> d -> State Game ()
evInputChoice ic d = triggerEvent ic (InputChoiceData d)

evTriggerTime :: UTCTime -> State Game ()
evTriggerTime t = triggerEvent (Time t) (TimeData t)


--delete all variables of a rule
delVarsRule :: RuleNumber -> State Game ()
delVarsRule rn = void $ variables %= filter ((/= rn) . getL vRuleNumber)

--delete all events of a rule
delEventsRule :: RuleNumber -> State Game ()
delEventsRule rn = void $ events %= filter ((/= rn) . getL ruleNumber)

traceState :: String -> State s String
traceState x = state (\s -> trace ("trace: " ++ x) (x, s))

