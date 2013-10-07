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
newVar_ s a = partial "newVar_: Variable existing" (newVar s a)

-- | variable reading
readVar :: (Typeable a, Show a, Eq a) => (V a) -> Nomex (Maybe a)
readVar = ReadVar

readVar_ :: forall a. (Typeable a, Show a, Eq a) => (V a) -> Nomex a
readVar_ v@(V a) = partial ("readVar_: Variable \"" ++ a ++ "\" with type \"" ++ (show $ typeOf v) ++ "\" not existing") (readVar v)

-- | variable writing
writeVar :: (Typeable a, Show a, Eq a) => (V a) -> a -> Nomex Bool
writeVar = WriteVar

writeVar_ :: (Typeable a, Show a, Eq a) => (V a) -> a -> Nomex ()
writeVar_ var val = void $ writeVar var val

-- | modify a variable using the provided function
modifyVar :: (Typeable a, Show a, Eq a) => (V a) -> (a -> a) -> Nomex ()
modifyVar v f = writeVar_ v . f =<< readVar_ v

-- | delete variable
delVar :: (V a) -> Nomex Bool
delVar = DelVar

delVar_ :: (V a) -> Nomex ()
delVar_ v = void $ delVar v

-- * Message Variable
-- | a MsgVar is a variable with a message attached, allowing to trigger registered functions anytime the var if modified
data VEvent a = VUpdated a | VDeleted deriving (Typeable, Show, Eq)
data MsgVar a = MsgVar (Msg (VEvent a)) (V a)

msgVar :: String -> MsgVar a
msgVar a = MsgVar (Message a) (V a)

newMsgVar :: (Typeable a, Show a, Eq a) => VarName -> a -> Nomex (Maybe (MsgVar a))
newMsgVar name a = do
    mv <- newVar name a
    return $ mv >>= Just . MsgVar (Message name)

newMsgVar_ :: (Typeable a, Show a, Eq a) => VarName -> a -> Nomex (MsgVar a)
newMsgVar_ name a = partial "newMsgVar_: Variable existing" (newMsgVar name a)


newMsgVar' :: (Typeable a, Show a, Eq a) => VarName -> a -> (VEvent a -> Nomex()) -> Nomex (Maybe (MsgVar a))
newMsgVar' name a f = do
    mv <- newMsgVar name a
    case mv of
       Just (MsgVar m _) -> do
          onMessage m $ f . messageData
          return mv
       Nothing -> return Nothing

writeMsgVar :: (Typeable a, Show a, Eq a) => MsgVar a -> a -> Nomex Bool
writeMsgVar (MsgVar m v) a = do
   r <- writeVar v a
   sendMessage m (VUpdated a)
   return r

writeMsgVar_ :: (Typeable a, Show a, Eq a) => MsgVar a -> a -> Nomex ()
writeMsgVar_ mv a = void $ writeMsgVar mv a

readMsgVar :: (Typeable a, Show a, Eq a) => MsgVar a -> Nomex (Maybe a)
readMsgVar (MsgVar _ v) = readVar v

readMsgVar_ :: (Typeable a, Show a, Eq a) => MsgVar a -> Nomex a
readMsgVar_ mv = partial "readMsgVar_: variable not existing" (readMsgVar mv)

modifyMsgVar :: (Typeable a, Show a, Eq a) => MsgVar a -> (a -> a) -> Nomex ()
modifyMsgVar mv f = writeMsgVar_ mv . f =<< readMsgVar_ mv

delMsgVar :: (Typeable a, Show a, Eq a) => MsgVar a -> Nomex Bool
delMsgVar (MsgVar m v) = do
   sendMessage m VDeleted
   delAllEvents m
   delVar v

onMsgVarChange :: (Typeable a, Show a, Eq a) => MsgVar a -> (VEvent a -> Nomex()) -> Nomex ()
onMsgVarChange mv f = do
   m <- getMsgVarMessage mv
   onMessage m $ \(MessageData v) -> f v

onMsgVarEvent :: (Typeable a, Show a, Eq a)
               => MsgVar a
               -> (a -> Nomex b)
               -> (a -> b -> Nomex())
               -> (b -> Nomex())
               -> Nomex ()
onMsgVarEvent mv create update delete = do
   val <- readMsgVar_ mv
   c <- create val
   onMsgVarChange mv $ f c where
      f c' (VUpdated v) = update v c'
      f c' VDeleted     = delete c'

-- | get the messsage triggered when the array is filled
getMsgVarMessage :: (Typeable a, Show a, Eq a) => (MsgVar a) -> Nomex (Msg (VEvent a))
getMsgVarMessage (MsgVar m _) = return m

-- | get the association array
getMsgVarData :: (Typeable a, Show a, Eq a) => (MsgVar a) -> Nomex (Maybe a)
getMsgVarData (MsgVar _ v) = readVar v

getMsgVarData_ :: (Typeable a, Show a, Eq a) => (MsgVar a) -> Nomex a
getMsgVarData_ (MsgVar _ v) = readVar_ v

getMsgVarName :: (Typeable a, Show a, Eq a) => (MsgVar a) -> String
getMsgVarName (MsgVar _ (V varName)) = varName

    
-- * Variable arrays
-- | ArrayVar is an indexed array with a signal attached triggered at every change.
-- | each indexed elements starts empty (value=Nothing).
type ArrayVar i a = MsgVar [(i, Maybe a)]

-- | initialize an empty ArrayVar
newArrayVar :: (Typeable a, Show a, Eq a, Typeable i, Show i, Eq i) => VarName -> [i] -> Nomex (Maybe (ArrayVar i a))
newArrayVar name l = do
    let list = map (\i -> (i, Nothing)) l
    newMsgVar name list

newArrayVar_ :: (Typeable a, Show a, Eq a, Typeable i, Show i, Eq i) => VarName -> [i] -> Nomex (ArrayVar i a)
newArrayVar_ name l = partial "newArrayVar_: Variable existing" (newArrayVar name l)

-- | initialize an empty ArrayVar, registering a callback that will be triggered at every change
newArrayVar' :: (Typeable a, Show a, Eq a, Typeable i, Show i, Eq i) => VarName -> [i] -> (VEvent [(i,Maybe a)] -> Nomex ()) -> Nomex (Maybe (ArrayVar i a))
newArrayVar' name l f = do
    let list = map (\i -> (i, Nothing)) l
    newMsgVar' name list f

-- | initialize an empty ArrayVar, registering a callback.
-- the ArrayVar will be deleted when full
newArrayVarOnce :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => VarName -> [i] -> (VEvent [(i, Maybe a)] -> Nomex ()) -> Nomex (Maybe (ArrayVar i a))
newArrayVarOnce name l f = do
   mv <- newArrayVar' name l f
   when (isJust mv) $ cleanOnFull $ fromJust mv
   return mv

cleanOnFull :: (Typeable a, Show a, Eq a, Ord i, Typeable i, Show i) => (ArrayVar i a) -> Nomex ()
cleanOnFull ar = do
   m <- getMsgVarMessage ar 
   onMessage m $ \_ -> do
      full <- (isFullArrayVar_ ar)
      when full $ void $ delMsgVar ar
      return ()
   return ()

isFullArrayVar_ :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> Nomex Bool
isFullArrayVar_ av = do
   md <- getMsgVarData av
   return $ and $ map isJust $ map snd $ fromJust md
       
   
-- | store one value and the given index. If this is the last filled element, the registered callbacks are triggered.
putArrayVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> i -> a -> Nomex Bool
putArrayVar mv i a = do
    ma <- readMsgVar mv
    case ma of
       Just ar -> do
          let ar2 = M.insert i (Just a) (fromList ar)
          writeMsgVar mv (toList ar2)
       Nothing -> return False

putArrayVar_ :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> i -> a -> Nomex ()
putArrayVar_ mv i a = void $ putArrayVar mv i a       




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
onInputTextOnce :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextOnce title handler pn = onEventOnce (inputText pn title) (handler . inputTextData)

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

onInputCheckboxOnce :: (Typeable a, Eq a,  Show a) => String -> [(a, String)] -> ([a] -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputCheckboxOnce title choices handler pn = onEventOnce (inputCheckbox pn title choices) (handler . inputCheckboxData)

onInputCheckboxOnce_ :: (Typeable a, Eq a,  Show a) => String -> [(a, String)] -> ([a] -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputCheckboxOnce_ title choices handler pn = onEventOnce_ (inputCheckbox pn title choices) (handler . inputCheckboxData)


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

onInputButtonOnce :: String -> (() -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputButtonOnce title handler pn = onEventOnce (inputButton pn title) (handler . inputButtonData)

onInputButtonOnce_ :: String -> (() -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputButtonOnce_ title handler pn = onEventOnce_ (inputButton pn title) (handler . inputButtonData)

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

onInputTextareaOnce :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
onInputTextareaOnce title handler pn = onEventOnce (inputTextarea pn title) (handler . inputTextareaData)

onInputTextareaOnce_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex ()
onInputTextareaOnce_ title handler pn = onEventOnce_ (inputTextarea pn title) (handler . inputTextareaData)

-- * Players

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

-- * Outputs

-- | outputs a message to one player
newOutput :: Nomex String -> PlayerNumber -> Nomex OutputNumber
newOutput ns pn = ns >>= NewOutput pn

newOutput_ :: Nomex String -> PlayerNumber -> Nomex ()
newOutput_ ns pn = void $ newOutput ns pn

outputAll :: String -> Nomex ()
outputAll s = getPlayers >>= mapM_ ((newOutput (return s)) . _playerNumber)

updateOutput :: OutputNumber -> Nomex String -> Nomex Bool
updateOutput on ns = ns >>= UpdateOutput on

updateOutput_ :: OutputNumber -> Nomex String -> Nomex ()
updateOutput_ on ns = void $ updateOutput on ns

delOutput :: OutputNumber -> Nomex Bool
delOutput = DelOutput

delOutput_ :: OutputNumber -> Nomex ()
delOutput_ on = void $ delOutput on

-- permanently display a variable (update display when variable is updated)
displayVar :: (Typeable a, Show a, Eq a) => PlayerNumber -> MsgVar a -> (a -> Nomex String) -> Nomex ()
displayVar pn mv dis = onMsgVarEvent
   mv
   (\a -> newOutput (dis a) pn)
   (\a n -> updateOutput_ n (dis a))
   delOutput_

displaySimpleVar :: (Typeable a, Show a, Eq a) => PlayerNumber -> Nomex String -> MsgVar a -> Nomex ()
displaySimpleVar pn ntitle mv = do
   let title a = do
        t <- ntitle
        return $ (t ++ ": " ++ (show a) ++ "\n")
   displayVar pn mv title

displayArrayVar :: (Typeable a, Show a, Eq a, Typeable i, Show i, Eq i) => PlayerNumber -> Nomex String -> ArrayVar i a -> Nomex ()
displayArrayVar pn ntitle mv = displayVar pn mv (showArrayVar ntitle)

showArrayVar :: (Show a, Show i) => Nomex String -> [(i,a)] -> Nomex String
showArrayVar title l = do
   t <- title
   return $ t ++ "\n" ++ concatMap (\(i,a) -> (show i) ++ "\t" ++ (show a) ++ "\n") l

-- * Victory, time and self-number

-- | set victory to a list of players
setVictory :: [PlayerNumber] -> Nomex ()
setVictory = SetVictory

-- | give victory to one player
giveVictory :: PlayerNumber -> Nomex ()
giveVictory pn = SetVictory [pn]

getCurrentTime :: Nomex UTCTime
getCurrentTime = CurrentTime

-- | allows a rule to retrieve its own number (for auto-deleting for example)
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

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

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
        f m (VUpdated ((_, Just a):(_, Just b):[])) = sendMessage m $ a && b
        f _ _ = return ()


partial :: String -> Nomex (Maybe a) -> Nomex a
partial s nm = do
   m <- nm
   case m of
      Just a -> return a
      Nothing -> throwError s


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
