{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, GADTs, NamedFieldPuns, ScopedTypeVariables, DoAndIfThenElse #-}

module Language.Nomyx.Evaluation where

import Prelude hiding ((.), log)
import Language.Nomyx.Utils
import Control.Monad
import Control.Monad.State.Lazy
import Data.List
import Data.Typeable
import Data.Function hiding ((.))
import Data.Time
import Data.Lens
import Control.Category
import Control.Monad.Error (ErrorT(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Applicative ((<$>))
import Language.Nomyx.Expression

type Evaluate a = ErrorT String (State Game) a

-- | evaluate an expression.
-- The rule number passed is the number of the rule containing the expression.
evalExp :: Nomex a -> RuleNumber -> Evaluate a
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

evalExp (WriteVar (V name) val) _ = do
   vars <- access variables
   case find (\(Var _ myName _) -> myName == name) vars of
      Nothing -> return False
      Just (Var rn myName _) -> do
         variables %= replaceWith ((== name) . getL vName) (Var rn myName val)
         return True

evalExp (OnEvent event handler) rn = do
   evs <- access events
   let en = getFreeNumber (map _eventNumber evs)
   events %= ((EH en rn event handler SActive) : )
   return en

evalExp (DelEvent en) _ = evDelEvent en

evalExp (DelAllEvents e) _ = do
   evs <- access events
   let filtered = filter (\EH {event} -> event === e) evs
   --traceM ("DelAllEvents: deleting " ++ show filtered)
   mapM_ (\e -> evDelEvent e) (_eventNumber <$> filtered)

evalExp (SendMessage (Message id) myData) _ = triggerEvent_ (Message id) (MessageData myData)

evalExp (NewOutput pn s) _       = evNewOutput pn s
evalExp (UpdateOutput on s) _    = evUpdateOutput on s
evalExp (DelOutput on) _         = evDelOutput on
evalExp (ProposeRule rule)    _  = evProposeRule rule
evalExp (ActivateRule rule)   rn = evActivateRule rule rn
evalExp (RejectRule rule)     rn = evRejectRule rule rn
evalExp (AddRule rule)        _  = evAddRule rule
evalExp (ModifyRule mod rule) _  = evModifyRule mod rule
evalExp GetRules              _  = access rules
evalExp GetPlayers            _  = access players
evalExp (SetPlayerName pn n)  _  = evChangeName pn n
evalExp (DelPlayer pn)        _  = evDelPlayer pn
evalExp SelfRuleNumber        rn = return rn
evalExp (CurrentTime)         _  = access currentTime
evalExp (Return a)            _  = return a
evalExp (ThrowError s)        _  = throwError s
evalExp (CatchError n h)      rn = catchError (evalExp n rn) (\a -> evalExp (h a) rn)
evalExp (SetVictory ps)       _  = do
   victory ~= ps
   pls <- access players
   let victorious = filter (\pl -> _playerNumber pl `elem` ps) pls
   triggerEvent_ Victory (VictoryData victorious)

evalExp (Bind exp f) rn = do
   a <- evalExp exp rn
   evalExp (f a) rn


--execute all the handlers of the specified event with the given data
triggerEvent :: (Typeable e, Show e, Eq e) => Event e -> EventData e -> Evaluate Bool
triggerEvent e dat = do
   evs <- access events
   let filtered = filter (\(EH {event, _evStatus}) -> e === event && _evStatus == SActive) evs
   case filtered of
      [] -> do
         --traceM $ "triggerEvent: Event not found:" ++ (show e)
         return False
      xs -> do
         mapM_ f xs
         return True where
            f (EH {_ruleNumber, _eventNumber, handler}) = case cast handler of
               Just castedH -> do
                  let (exp :: Nomex ()) = castedH (_eventNumber, dat)
                  (evalExp exp _ruleNumber) `catchError` (errorHandler _ruleNumber _eventNumber)
               Nothing -> logAll ("failed " ++ (show $ typeOf handler))

triggerEvent_ :: (Typeable e, Show e, Eq e) => Event e -> EventData e -> Evaluate ()
triggerEvent_ e ed = void $ triggerEvent e ed

errorHandler :: RuleNumber -> EventNumber -> String -> Evaluate ()
errorHandler rn en s = logAll $ "Error in rule " ++ (show rn) ++ " (triggered by event " ++ (show en) ++ "): " ++ s

-- trigger the input event with the input data
triggerInput :: EventNumber -> UInputData -> Evaluate ()
triggerInput en ir = do
   evs <- access events
   let filtered = filter ((== en) . getL eventNumber) evs
   mapM_ (execInputHandler ir) filtered

-- execute the event handler using the data received from user
execInputHandler :: UInputData -> EventHandler -> Evaluate ()
execInputHandler (UTextData s)      (EH en rn (InputEv (Input _ _ Text))          h SActive) = evalExp (h (en, InputData $ TextData s)) rn
execInputHandler (UTextAreaData s)  (EH en rn (InputEv (Input _ _ TextArea))      h SActive) = evalExp (h (en, InputData $ TextAreaData s)) rn
execInputHandler (UButtonData)      (EH en rn (InputEv (Input _ _ Button))        h SActive) = evalExp (h (en, InputData $ ButtonData)) rn
execInputHandler (URadioData i)     (EH en rn (InputEv (Input _ _ (Radio cs)))    h SActive) = evalExp (h (en, InputData $ RadioData $ fst $ cs!!i)) rn
execInputHandler (UCheckboxData is) (EH en rn (InputEv (Input _ _ (Checkbox cs))) h SActive) = evalExp (h (en, InputData $ CheckboxData $ fst <$> cs `sel` is)) rn
execInputHandler _ _ = return ()

findEvent :: EventNumber -> [EventHandler] -> Maybe (EventHandler)
findEvent en evs = find ((== en) . getL eventNumber) evs

getChoiceEvents :: Evaluate [EventNumber]
getChoiceEvents = do
   evs <- access events
   return $ map _eventNumber $ filter choiceEvent evs
   where choiceEvent (EH _ _ (InputEv (Input _ _ (Radio _))) _ _) = True
         choiceEvent _ = False


evProposeRule :: Rule -> Evaluate Bool
evProposeRule rule = do
   rs <- access rules
   case find ((== (rNumber ^$ rule)) . getL rNumber) rs of
      Nothing -> do
         rules %= (rule:)
         triggerEvent_ (RuleEv Proposed) (RuleData rule)
         return True
      Just _ -> return False

--Sets the rule status to Active and execute it if possible
evActivateRule :: RuleNumber -> RuleNumber -> Evaluate Bool
evActivateRule rn by = do
   rs <- access rules
   case find ((== rn) . getL rNumber) rs of
      Nothing -> return False
      Just r -> do
         let newrules = replaceWith ((== rn) . getL rNumber) r{_rStatus = Active, _rAssessedBy = Just by} rs
         rules ~= newrules
         --execute the rule
         evalExp (_rRuleFunc r) rn
         triggerEvent_ (RuleEv Activated) (RuleData r)
         return True

evRejectRule :: RuleNumber -> RuleNumber -> Evaluate Bool
evRejectRule rn by = do
   rs <- access rules
   case find ((== rn) . getL rNumber) rs of
      Nothing -> return False
      Just r -> do
         let newrules = replaceWith ((== rn) . getL rNumber) r{_rStatus = Reject, _rAssessedBy = Just by} rs
         rules ~= newrules
         triggerEvent_ (RuleEv Rejected) (RuleData r)
         delVarsRule rn
         delEventsRule rn
         return True

evAddRule :: Rule -> Evaluate Bool
evAddRule rule = do
   rs <- access rules
   case find ((== (rNumber ^$ rule)) . getL rNumber) rs of
      Nothing -> do
         rules %= (rule:)
         triggerEvent_ (RuleEv Added) (RuleData rule)
         return True
      Just _ -> return False


--TODO: clean and execute new rule
evModifyRule :: RuleNumber -> Rule -> Evaluate Bool
evModifyRule mod rule = do
   rs <- access rules
   let newRules = replaceWith ((== mod) . getL rNumber) rule rs
   case find ((== mod) . getL rNumber) rs of
      Nothing -> return False
      Just r ->  do
         rules ~= newRules
         triggerEvent_ (RuleEv Modified) (RuleData r)
         return True

addPlayer :: PlayerInfo -> Evaluate Bool
addPlayer pi = do
   pls <- access players
   let exists = any (((==) `on` _playerNumber) pi) pls
   when (not exists) $ do
       players %= (pi:)
       triggerEvent_ (Player Arrive) (PlayerData pi)
   return $ not exists

evDelPlayer :: PlayerNumber -> Evaluate Bool
evDelPlayer pn = do
   g <- get
   case find ((== pn) . getL playerNumber) (_players g) of
      Nothing -> do
         tracePN pn "not in game!"
         return False
      Just pi -> do
         players %= filter ((/= pn) . getL playerNumber)
         triggerEvent_ (Player Leave) (PlayerData pi)
         tracePN pn $ "leaving the game: " ++ (_gameName g)
         return True

evChangeName :: PlayerNumber -> PlayerName -> Evaluate Bool
evChangeName pn name = do
   pls <- access players
   case find ((== pn) . getL playerNumber) pls of
      Nothing -> return False
      Just _ -> do
         players ~= replaceWith ((== pn) . getL playerNumber) (PlayerInfo pn name) pls
         return True

evDelEvent :: EventNumber -> Evaluate Bool
evDelEvent en = do
   --traceM ("DelEvent: called with en=" ++ (show en))
   evs <- access events
   case find ((== en) . getL eventNumber) evs of
      Nothing -> do
         --traceM ("DelEvent: event number not found! en=" ++ (show en) ++ " rn=" ++ (show rn))
         return False
      Just eh -> do
         case (_evStatus eh) of
            SActive -> do
               --traceM ("DelEvent: deleting event en=" ++ (show en) ++ " rn=" ++ (show rn))
               let newEvents = replaceWith ((== en) . getL eventNumber) eh{_evStatus = SDeleted} evs
               events ~= newEvents
               return True
            SDeleted -> do
               --traceM ("DelEvent: Event already deleted! en=" ++ (show en) ++ "rn event=" ++ (show $ _ruleNumber eh) ++ " rn=" ++ (show rn))
               return False


evTriggerTime :: UTCTime -> Evaluate Bool
evTriggerTime t = triggerEvent (Time t) (TimeData t)


--delete all variables of a rule
delVarsRule :: RuleNumber -> Evaluate ()
delVarsRule rn = void $ variables %= filter ((/= rn) . getL vRuleNumber)

--delete all events of a rule
delEventsRule :: RuleNumber -> Evaluate ()
delEventsRule rn = do
   evs <- access events
   let toDelete = filter ((== rn) . getL ruleNumber) evs
   mapM_ (evDelEvent . _eventNumber) toDelete


evNewOutput :: PlayerNumber -> String -> Evaluate OutputNumber
evNewOutput pn s = do
   ops <- access outputs
   let on = getFreeNumber (map _outputNumber ops)
   outputs %= ((Output on pn s SActive) : )
   return on

--TODO: test sur output deleted
evUpdateOutput :: OutputNumber -> String -> Evaluate Bool
evUpdateOutput on s = do
   ops <- access outputs
   case find (\(Output myOn _ _ SActive) -> myOn == on) ops of
      Nothing -> return False
      Just (Output _ pn _ _) -> do
         outputs %= replaceWith ((== on) . getL outputNumber) (Output on pn s SActive)
         return True

evDelOutput :: OutputNumber -> Evaluate Bool
evDelOutput on = do
   ops <- access outputs
   case find ((== on) . getL outputNumber) ops of
      Nothing -> do
         return False
      Just o -> do
         case (_oStatus o) of
            SActive -> do
               let newOutputs = replaceWith ((== on) . getL outputNumber) o{_oStatus = SDeleted} ops
               outputs ~= newOutputs
               return True
            SDeleted -> do
               return False

logPlayer :: PlayerNumber -> String -> Evaluate ()
logPlayer pn s = void $ log %= ((Just pn, s) : )

logAll :: String -> Evaluate ()
logAll s = void $ log %= ((Nothing, s) : )

runEvalError :: PlayerNumber -> Evaluate () -> State Game ()
runEvalError pn egs = do
   e <- runErrorT egs
   case e of
      Right gs -> return gs
      Left e -> do
         tracePN pn $ "Error: " ++ e
         void $ log %= ((Just pn, "Error: " ++ e) : )
