{-# LANGUAGE DeriveDataTypeable, GADTs, ScopedTypeVariables, TupleSections, QuasiQuotes, FlexibleInstances #-}

-- | All the building blocks to build rules.
module Language.Nomyx.Definition where

import Language.Nomyx.Expression
import Data.Typeable
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Time hiding (getCurrentTime)
import qualified Data.Map as M
import Data.Map hiding (map, filter, insert, mapMaybe, null)
import Data.Time.Recurrence hiding (filter)
import Safe
import Data.Lens
import Control.Applicative
import Data.Boolean
import Control.Monad.Error
import Language.Nomyx.Utils

-- * Variables
-- | variable creation
newVar :: (Typeable a, Show a, Eq a) => VarName -> a -> Nomex (Maybe (V a))
newVar = NewVar

newVar_ :: (Typeable a, Show a, Eq a) => VarName -> a -> Nomex (V a)
newVar_ s a = do
    mv <- NewVar s a
    case mv of
        Just var -> return var
        Nothing -> throwError "newVar_: Variable existing"

-- | variable reading
readVar :: (Typeable a, Show a, Eq a) => (V a) -> Nomex (Maybe a)
readVar = ReadVar

readVar_ :: forall a. (Typeable a, Show a, Eq a) => (V a) -> Nomex a
readVar_ v@(V a) = do
    ma <- ReadVar v
    case ma of
        Just (val:: a) -> return val
        Nothing -> throwError $ "readVar_: Variable \"" ++ a ++ "\" with type \"" ++ (show $ typeOf v) ++ "\" not existing"

-- | variable writing
writeVar :: (Typeable a, Show a, Eq a) => (V a) -> a -> Nomex Bool
writeVar = WriteVar

writeVar_ :: (Typeable a, Show a, Eq a) => (V a) -> a -> Nomex ()
writeVar_ var val = do
    ma <- WriteVar var val
    case ma of
       True -> return ()
       False -> throwError "writeVar_: Variable not existing"

-- | modify a variable using the provided function
modifyVar :: (Typeable a, Show a, Eq a) => (V a) -> (a -> a) -> Nomex ()
modifyVar v f = writeVar_ v . f =<< readVar_ v

-- | delete variable
delVar :: (V a) -> Nomex Bool
delVar = DelVar

delVar_ :: (V a) -> Nomex ()
delVar_ v = DelVar v >> return ()

-- * Variable arrays
-- | ArrayVar is an indexed array with a signal attached to warn when the array is filled.
-- | each indexed elements starts empty (value=Nothing), and when the array is full, the signal is triggered.
-- | This is useful to wait for a serie of events to happen, and trigger a computation on the collected results.
data ArrayVar i a = ArrayVar (Msg [(i, Maybe a)]) (V (Map i (Maybe a)))

-- | initialize an empty ArrayVar
newArrayVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => VarName -> [i] -> Nomex (ArrayVar i a)
newArrayVar name l = do
    let list = map (\i -> (i, Nothing)) l
    v <- newVar_ name (fromList list)
    return $ ArrayVar (Message name) v

-- | initialize an empty ArrayVar, registering a callback that will be triggered when the array is filled
newArrayVar' :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => VarName -> [i] -> ([(i,Maybe a)] -> Nomex ()) -> Nomex (ArrayVar i a)
newArrayVar' name l f = do
    av@(ArrayVar m _) <- newArrayVar name l
    onMessage m $ f . messageData
    return av

-- | initialize an empty ArrayVar, registering a callback.
--the callback will be triggered when the array is filled, and then the ArrayVar will be deleted
newArrayVarOnce :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => VarName -> [i] -> ([(i, Maybe a)] -> Nomex ()) -> Nomex (ArrayVar i a)
newArrayVarOnce name l f = do
   av@(ArrayVar m _) <- newArrayVar name l
   onMessage m $ \a -> do
      f $ messageData a
      full <- (isFullArrayVar av)
      when full $ delArrayVar av
   return av where


isFullArrayVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> Nomex (Bool)
isFullArrayVar av = do
   d <- getArrayVarData av
   let full = and $ map isJust $ map snd d
   return full
   
-- | store one value and the given index. If this is the last filled element, the registered callbacks are triggered.
putArrayVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> i -> a -> Nomex ()
putArrayVar (ArrayVar m v) i a = do
    ar <- readVar_ v
    let ar2 = M.insert i (Just a) ar
    writeVar_ v ar2
    sendMessage m (toList ar2)

-- | get the messsage triggered when the array is filled
getArrayVarMessage :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> Nomex (Msg [(i, Maybe a)])
getArrayVarMessage (ArrayVar m _) = return m

--TODO: use total function readVar instead of readVar_
-- | get the association array
getArrayVarData :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> Nomex ([(i, Maybe a)])
getArrayVarData (ArrayVar _ v) = toList <$> (readVar_ v)

-- | get the association array with only the filled values
getArrayVarData' :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> Nomex ([(i, a)])
getArrayVarData' v = catMaybes . map sndMaybe <$> (getArrayVarData v)

delArrayVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> Nomex ()
delArrayVar (ArrayVar m v) = delAllEvents m >> delVar_ v

-- * Events

-- | register a callback on an event
onEvent :: (Typeable e, Show e, Eq e) => Event e -> ((EventNumber, EventData e) -> Nomex ()) -> Nomex EventNumber
onEvent = OnEvent

-- | register a callback on an event, disregard the event number
onEvent_ :: forall e. (Typeable e, Show e, Eq e) => Event e -> (EventData e -> Nomex ()) -> Nomex ()
onEvent_ e h = do
    OnEvent e (\(_, d) -> h d)
    return ()

-- | set an handler for an event that will be triggered only once
onEventOnce :: (Typeable e, Show e, Eq e) => Event e -> (EventData e -> Nomex ()) -> Nomex EventNumber
onEventOnce e h = do
    let handler (en, ed) = delEvent_ en >> h ed
    n <- OnEvent e handler
    return n

-- | set an handler for an event that will be triggered only once
onEventOnce_ :: (Typeable e, Show e, Eq e) => Event e -> (EventData e -> Nomex ()) -> Nomex ()
onEventOnce_ e h = do
    let handler (en, ed) = delEvent_ en >> h ed
    OnEvent e handler
    return ()

delEvent :: EventNumber -> Nomex Bool
delEvent = DelEvent

delEvent_ :: EventNumber -> Nomex ()
delEvent_ e = delEvent e >> return ()

delAllEvents :: (Typeable e, Show e, Eq e) => Event e -> Nomex ()
delAllEvents = DelAllEvents

-- | broadcast a message that can be catched by another rule
sendMessage :: (Typeable a, Show a, Eq a) => Msg a -> a -> Nomex ()
sendMessage = SendMessage

sendMessage_ :: Msg () -> Nomex ()
sendMessage_ m = SendMessage m ()

-- | subscribe on a message 
onMessage :: (Typeable m, Show m) => Msg m -> (MsgData m -> Nomex ()) -> Nomex ()
onMessage m f = onEvent_ m f

onMessageOnce :: (Typeable m, Show m) => Msg m -> (MsgData m -> Nomex ()) -> Nomex ()
onMessageOnce m f = onEventOnce_ m f

-- | on the provided schedule, the supplied function will be called
schedule :: (Schedule Freq) -> (UTCTime -> Nomex ()) -> Nomex ()
schedule sched f = do
    now <- getCurrentTime
    let next = head $ starting now $ sched
    if (next == now) then executeAndScheduleNext (f . timeData) sched (TimeData now)
                     else onEventOnce_ (Time next) $ executeAndScheduleNext (f . timeData) sched where

executeAndScheduleNext :: (EventData Time -> Nomex ()) -> (Schedule Freq) -> (EventData Time) -> Nomex ()
executeAndScheduleNext f sched now = do
   f now
   let rest = drop 1 $ starting (timeData now) $ sched
   when (rest /= []) $ onEventOnce_ (Time $ head rest) $ executeAndScheduleNext f sched


schedule_ :: (Schedule Freq) -> Nomex () -> Nomex ()
schedule_ ts f = schedule ts (\_-> f)

--at each time provided, the supplied function will be called
schedule' :: [UTCTime] -> (UTCTime -> Nomex ()) -> Nomex ()
schedule' sched f = do
    let sched' = sort sched
    now <- getCurrentTime
    let nextMay = headMay $ filter (>=now) $ sched'
    case nextMay of
        Just next -> do
           if (next == now) then executeAndScheduleNext' (f . timeData) sched' (TimeData now)
                     else onEventOnce_ (Time next) $ executeAndScheduleNext' (f . timeData) sched'
        Nothing -> return ()
            

executeAndScheduleNext' :: (EventData Time -> Nomex ()) -> [UTCTime] -> (EventData Time) -> Nomex ()
executeAndScheduleNext' f sched now = do
   f now
   let rest = drop 1 $ sched
   when (rest /= []) $ onEventOnce_ (Time $ head rest) $ executeAndScheduleNext' f sched
   

schedule'_ :: [UTCTime] -> Nomex () -> Nomex ()
schedule'_ ts f = schedule' ts (\_-> f)

-- * Rule management

-- | activate a rule: change its state to Active and execute it
activateRule :: RuleNumber -> Nomex Bool
activateRule = ActivateRule

activateRule_ :: RuleNumber -> Nomex ()
activateRule_ r = activateRule r >> return ()

-- | reject a rule: change its state to Suppressed and suppresses all its environment (events, variables, inputs)
-- the rule can be activated again later
rejectRule :: RuleNumber -> Nomex Bool
rejectRule = RejectRule

rejectRule_ :: RuleNumber -> Nomex ()
rejectRule_ r = void $ rejectRule r

getRules :: Nomex [Rule]
getRules = GetRules

getActiveRules :: Nomex [Rule]
getActiveRules = return . (filter ((== Active) . _rStatus) ) =<< getRules

getRule :: RuleNumber -> Nomex (Maybe Rule)
getRule rn = do
   rs <- GetRules
   return $ find ((== rn) . getL rNumber) rs

getRulesByNumbers :: [RuleNumber] -> Nomex [Rule]
getRulesByNumbers rns = mapMaybeM getRule rns

getRuleFuncs :: Nomex [RuleFunc]
getRuleFuncs = return . (map _rRuleFunc) =<< getRules

-- | add a rule to the game, it will have to be activated 
addRule :: Rule -> Nomex Bool
addRule r = AddRule r

addRule_ :: Rule -> Nomex ()
addRule_ r = void $ AddRule r

addRuleParams :: RuleName -> RuleFunc -> RuleCode -> String -> Nomex RuleNumber
addRuleParams name func code desc = do
   number <- getFreeRuleNumber
   res <- addRule $ defaultRule {_rName = name, _rRuleFunc = func, _rRuleCode = code, _rNumber = number, _rDescription = desc}
   return $ if res then number else error "addRuleParams: cannot add rule"
   

getFreeRuleNumber :: Nomex RuleNumber
getFreeRuleNumber = do
   rs <- getRules
   return $ getFreeNumber $ map _rNumber rs


--suppresses completly a rule and its environment from the system
suppressRule :: RuleNumber -> Nomex Bool
suppressRule rn = RejectRule rn

suppressRule_ :: RuleNumber -> Nomex ()
suppressRule_ rn = void $ RejectRule rn

suppressAllRules :: Nomex Bool
suppressAllRules = do
    rs <- getRules
    res <- mapM (suppressRule . _rNumber) rs
    return $ and res

modifyRule :: RuleNumber -> Rule -> Nomex Bool
modifyRule rn r = ModifyRule rn r


-- * Inputs


-- ** Radio inputs

inputRadio :: (Eq c, Show c, Typeable c) => PlayerNumber -> String -> [c] -> c -> Event (Input c)
inputRadio pn title cs _ = InputEv (Input pn title (Radio (zip cs (show <$> cs))))

inputRadioHead :: (Eq c, Show c, Typeable c) => PlayerNumber -> String -> [c] -> Event (Input c)
inputRadioHead pn title choices = inputRadio pn title choices (head choices)

inputRadioEnum :: forall c. (Enum c, Bounded c, Typeable c, Eq c,  Show c) => PlayerNumber -> String -> c -> Event (Input c)
inputRadioEnum pn title defaultChoice = inputRadio pn title (enumFrom (minBound::c)) defaultChoice

inputRadioData :: (Show c) => EventData (Input c) -> c
inputRadioData (InputData (RadioData a)) = a
inputRadioData a = error $ "Not a Radio Data: " ++ (show a)

-- | triggers a choice input to the user. The result will be sent to the callback
onInputRadio :: (Typeable a, Eq a,  Show a) => String -> [a] -> (EventNumber -> a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputRadio title choices handler pn = onEvent (inputRadioHead pn title choices) (\(en, InputData (RadioData a)) -> handler en a)

-- | the same, disregard the event number
onInputRadio_ :: (Typeable a, Eq a, Show a) => String -> [a] -> (a -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputRadio_ title choices handler pn = onEvent_ (inputRadioHead pn title choices) (handler . inputRadioData)

-- | the same, suppress the event after first trigger
onInputRadioOnce :: (Typeable a, Eq a, Show a) => String -> [a] -> (a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputRadioOnce title choices handler pn = onEventOnce (inputRadioHead pn title choices) (handler . inputRadioData)

-- | the same, disregard the event number
onInputRadioOnce_ :: (Typeable a, Eq a, Show a) => String -> [a] -> (a -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputRadioOnce_ title choices handler pn = onEventOnce_ (inputRadioHead pn title choices) (handler . inputRadioData)

-- | triggers a choice input to the user, using an enumerate as input
onInputRadioEnum :: forall a. (Enum a, Bounded a, Typeable a, Eq a,  Show a) => String -> a -> (EventNumber -> a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputRadioEnum title defaultChoice handler pn = onEvent (inputRadioEnum pn title defaultChoice) (\(en, a) -> handler en (inputRadioData a))

-- | the same, disregard the event number
onInputRadioEnum_ :: forall a. (Enum a, Bounded a, Typeable a, Eq a,  Show a) => String -> a -> (a -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputRadioEnum_ title defaultChoice handler pn = onEvent_ (inputRadioEnum pn title defaultChoice) (handler . inputRadioData)

-- | the same, suppress the event after first trigger
onInputRadioEnumOnce_ :: forall a. (Enum a, Bounded a, Typeable a, Eq a,  Show a) => String -> a -> (a -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputRadioEnumOnce_ title defaultChoice handler pn = onEventOnce_ (inputRadioEnum pn title defaultChoice) (handler . inputRadioData)

-- ** Text inputs

inputText :: PlayerNumber -> String -> Event (Input String)
inputText pn title = InputEv (Input pn title Text)

inputTextData :: EventData (Input String) -> String
inputTextData (InputData (TextData a)) = a
inputTextData a = error $ "Not a Text Data: " ++ (show a)

-- | triggers a string input to the user. The result will be sent to the callback
onInputText :: String -> (EventNumber -> String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputText title handler pn = onEvent (inputText pn title) (\(en, a) -> handler en (inputTextData a))

-- | asks the player pn to answer a question, and feed the callback with this data.
onInputText_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputText_ title handler pn = onEvent_ (inputText pn title) (handler . inputTextData)

-- | asks the player pn to answer a question, and feed the callback with this data.
onInputTextOnce_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputTextOnce_ title handler pn = onEventOnce_ (inputText pn title) (handler . inputTextData)

-- ** Checkbox inputs

inputCheckboxData :: (Show c) => EventData (Input c) -> [c]
inputCheckboxData (InputData (CheckboxData a)) = a
inputCheckboxData a = error $ "Not a Checkbox Data: " ++ (show a)

inputCheckbox :: (Eq c, Show c, Typeable c) => PlayerNumber -> String -> [(c, String)] -> Event (Input c)
inputCheckbox pn title cs = InputEv (Input pn title (Checkbox cs))

onInputCheckbox :: (Typeable a, Eq a,  Show a) => String -> [(a, String)] -> (EventNumber -> [a] -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputCheckbox title choices handler pn = onEvent (inputCheckbox pn title choices) (\(en, InputData (CheckboxData a)) -> handler en a)

onInputCheckbox_ :: (Typeable a, Eq a,  Show a) => String -> [(a, String)] -> ([a] -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputCheckbox_ title choices handler pn = onEvent_ (inputCheckbox pn title choices) (handler . inputCheckboxData)

-- ** Button inputs

inputButtonData :: EventData (Input ()) -> ()
inputButtonData (InputData ButtonData) = ()
inputButtonData a = error $ "Not a Button Data: " ++ (show a)

inputButton :: PlayerNumber -> String -> Event (Input ())
inputButton pn title = InputEv (Input pn title Button)

onInputButton :: String -> (EventNumber -> () -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputButton title handler pn = onEvent (inputButton pn title) (\(en, InputData ButtonData) -> handler en ())

onInputButton_ :: String -> (() -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputButton_ title handler pn = onEvent_ (inputButton pn title) (handler . inputButtonData)

-- ** Textarea inputs

inputTextareaData :: EventData (Input String) -> String
inputTextareaData (InputData (TextAreaData a)) = a
inputTextareaData a = error $ "Not a Textarea Data: " ++ (show a)

inputTextarea :: PlayerNumber -> String -> Event (Input String)
inputTextarea pn title = InputEv (Input pn title TextArea)

onInputTextarea :: String -> (EventNumber -> String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextarea title handler pn = onEvent (inputTextarea pn title) (\(en, a) -> handler en (inputTextareaData a))

onInputTextarea_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputTextarea_ title handler pn = onEvent_ (inputTextarea pn title) (handler . inputTextareaData)

onInputTextareaOnce_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputTextareaOnce_ title handler pn = onEventOnce_ (inputTextarea pn title) (handler . inputTextareaData)

-- * Victory, players

-- | get all the players
getPlayers :: Nomex [PlayerInfo]
getPlayers = GetPlayers

-- | Get a specific player
getPlayer :: PlayerNumber -> Nomex (Maybe PlayerInfo)
getPlayer pn = do
   pls <- GetPlayers
   return $ find ((== pn) . getL playerNumber) pls

-- | Set the name of a player
getPlayerName :: PlayerNumber -> Nomex (Maybe PlayerName)
getPlayerName pn = do
  p <- getPlayer pn
  return $ _playerName <$> p

-- | Set the name of a player
setPlayerName :: PlayerNumber -> PlayerName -> Nomex Bool
setPlayerName = SetPlayerName

modifyPlayerName :: PlayerNumber -> (PlayerName -> PlayerName) -> Nomex Bool
modifyPlayerName pn f = do
   mn <- getPlayerName pn
   case mn of
      Just name -> setPlayerName pn (f name)
      Nothing -> return False


-- | Get the total number of playersgetPlayersNumber :: Nomex Int
getPlayersNumber = length <$> getPlayers

-- | Get all the players number
getAllPlayerNumbers :: Nomex [PlayerNumber]
getAllPlayerNumbers = map _playerNumber <$> getPlayers

-- | Remove the player from the game (kick)
delPlayer :: PlayerNumber -> Nomex Bool
delPlayer = DelPlayer



-- * Victory, output, time and self-number

-- | set victory to a list of players
setVictory :: [PlayerNumber] -> Nomex ()
setVictory = SetVictory

-- | give victory to one player
giveVictory :: PlayerNumber -> Nomex ()
giveVictory pn = SetVictory [pn]

-- | outputs a message to one player
output :: String -> PlayerNumber -> Nomex ()
output s pn = Output pn s

outputAll :: String -> Nomex ()
outputAll s = getPlayers >>= mapM_ ((output s) . _playerNumber)

getCurrentTime :: Nomex UTCTime
getCurrentTime = CurrentTime

-- | allows a rule to retrieve its self number (for auto-deleting for example)
getSelfRuleNumber :: Nomex RuleNumber
getSelfRuleNumber = SelfRuleNumber

getSelfRule :: Nomex Rule
getSelfRule  = do
   srn <- getSelfRuleNumber
   rs:[] <- getRulesByNumbers [srn]
   return rs

getSelfProposedByPlayer :: Nomex PlayerNumber
getSelfProposedByPlayer = getSelfRule >>= return . _rProposedBy

-- * Miscellaneous



voidRule :: Nomex a -> Nomex RuleResp
voidRule e = e >> return Void


instance Boolean (Nomex BoolResp) where
  true  = return $ BoolResp True
  false = return $ BoolResp False
  notB  = undefined
  (||*) = undefined
  (&&*) na nb = do
     a <- na
     b <- nb
     case a of
        (BoolResp a') -> case b of
           (BoolResp b') -> return $ BoolResp $ a' && b'
           (MsgResp  b') -> andMsgBool a' b' >>= (return . MsgResp) 
        (MsgResp a') -> case b of
           (BoolResp b') -> andMsgBool b' a' >>= (return . MsgResp)
           (MsgResp  b') -> andMsgMsg  a' b' >>= (return . MsgResp)

andMsgBool :: Bool -> (Msg Bool) -> Nomex (Msg Bool)
andMsgBool a b = do
   let m = Message ((show a) ++ " &&* " ++ (show b))
   onMessageOnce b (f m)
   return m where
        f m (MessageData b1) = sendMessage m $ a && b1

andMsgMsg :: Msg Bool -> Msg Bool -> Nomex (Msg Bool)
andMsgMsg a b = do
   let m = Message ((show a) ++ " &&* " ++ (show b))
   newArrayVarOnce ((show a) ++ ", " ++ (show b)) [1::Integer, 2] (f m)
   return m where
        f m ((_, Just a):(_, Just b):[]) = sendMessage m $ a && b
        f _ _ = return ()


-- | a default rule
defaultRule = Rule  {
    _rNumber       = 1,
    _rName         = "",
    _rDescription  = "",
    _rProposedBy   = 0,
    _rRuleCode     = "",
    _rRuleFunc     = return Void,
    _rStatus       = Pending,
    _rAssessedBy   = Nothing}
