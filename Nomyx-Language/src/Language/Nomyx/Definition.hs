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

addRuleParams_ :: RuleName -> RuleFunc -> RuleCode -> RuleNumber -> String -> Nomex ()
addRuleParams_ name func code number desc = addRule_ $ defaultRule {_rName = name, _rRuleFunc = func, _rRuleCode = code, _rNumber = number, _rDescription = desc}

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

inputChoice :: (Eq c, Show c) => PlayerNumber -> String -> [c] -> c -> Event (InputChoice c)
inputChoice = InputChoice

inputChoiceHead :: (Eq c, Show c) => PlayerNumber -> String -> [c] -> Event (InputChoice c)
inputChoiceHead pn title choices = inputChoice pn title choices (head choices)

inputChoiceEnum :: forall c. (Enum c, Bounded c, Typeable c, Eq c,  Show c) => PlayerNumber -> String -> c -> Event (InputChoice c)
inputChoiceEnum pn title defaultChoice = inputChoice pn title (enumFrom (minBound::c)) defaultChoice

inputString :: PlayerNumber -> String -> Event InputString
inputString = InputString

-- | triggers a choice input to the user. The result will be sent to the callback
onInputChoice :: (Typeable a, Eq a,  Show a) => String -> [a] -> (EventNumber -> a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputChoice title choices handler pn = onEvent (inputChoiceHead pn title choices) (\(en, a) -> handler en (inputChoiceData a))

-- | the same, disregard the event number
onInputChoice_ :: (Typeable a, Eq a, Show a) => String -> [a] -> (a -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputChoice_ title choices handler pn = onEvent_ (inputChoiceHead pn title choices) (handler . inputChoiceData)

-- | the same, suppress the event after first trigger
onInputChoiceOnce :: (Typeable a, Eq a, Show a) => String -> [a] -> (a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputChoiceOnce title choices handler pn = onEventOnce (inputChoiceHead pn title choices) (handler . inputChoiceData)

-- | the same, disregard the event number
onInputChoiceOnce_ :: (Typeable a, Eq a, Show a) => String -> [a] -> (a -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputChoiceOnce_ title choices handler pn = onEventOnce_ (inputChoiceHead pn title choices) (handler . inputChoiceData)

-- | triggers a choice input to the user, using an enumerate as input
onInputChoiceEnum :: forall a. (Enum a, Bounded a, Typeable a, Eq a,  Show a) => String -> a -> (EventNumber -> a -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputChoiceEnum title defaultChoice handler pn = onEvent (inputChoiceEnum pn title defaultChoice) (\(en, a) -> handler en (inputChoiceData a))

-- | the same, disregard the event number
onInputChoiceEnum_ :: forall a. (Enum a, Bounded a, Typeable a, Eq a,  Show a) => String -> a -> (a -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputChoiceEnum_ title defaultChoice handler pn = onEvent_ (inputChoiceEnum pn title defaultChoice) (handler . inputChoiceData)

-- | the same, suppress the event after first trigger
onInputChoiceEnumOnce_ :: forall a. (Enum a, Bounded a, Typeable a, Eq a,  Show a) => String -> a -> (a -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputChoiceEnumOnce_ title defaultChoice handler pn = onEventOnce_ (inputChoiceEnum pn title defaultChoice) (handler . inputChoiceData)


-- | triggers a string input to the user. The result will be sent to the callback
onInputString :: String -> (EventNumber -> String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputString title handler pn = onEvent (inputString pn title) (\(en, a) -> handler en (inputStringData a))

-- | asks the player pn to answer a question, and feed the callback with this data.
onInputString_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputString_ title handler pn = onEvent_ (inputString pn title) (handler . inputStringData)

-- | asks the player pn to answer a question, and feed the callback with this data.
onInputStringOnce_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputStringOnce_ title handler pn = onEventOnce_ (inputString pn title) (handler . inputStringData)

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
