{-# LANGUAGE DeriveDataTypeable, GADTs, ScopedTypeVariables, TupleSections#-}

--all the building blocks to build rules and basic rules.
module Language.Nomic.Rule where

import Language.Nomic.Expression
import Data.Typeable
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Time
import Data.Function
import Data.Map hiding (map, filter, insert, mapMaybe)
import qualified Data.Map as M (map, insert)
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import Control.Arrow

--variable creation
newVar :: (Typeable a, Show a, Eq a) => VarName -> a -> Exp (Maybe (V a))
newVar = NewVar

newVar_ :: (Typeable a, Show a, Eq a) => VarName -> a -> Exp (V a)
newVar_ s a = do
    mv <- NewVar s a
    case mv of
        Just var -> return var
        Nothing -> error "newVar_: Variable existing"

--variable reading
readVar :: (Typeable a, Show a, Eq a) => (V a) -> Exp (Maybe a)
readVar = ReadVar

readVar_ :: forall a. (Typeable a, Show a, Eq a) => (V a) -> Exp a
readVar_ v = do
    ma <- ReadVar v
    case ma of
        Just (val:: a) -> return val
        Nothing -> error "readVar_: Variable not existing"

--variable writing
writeVar :: (Typeable a, Show a, Eq a) => (V a) -> a -> Exp Bool
writeVar = WriteVar

writeVar_ :: (Typeable a, Show a, Eq a) => (V a) -> a -> Exp ()
writeVar_ var val = do
    ma <- WriteVar var val
    case ma of
       True -> return ()
       False -> error "writeVar_: Variable not existing"

modifyVar :: (Typeable a, Show a, Eq a) => (V a) -> (a -> a) -> Exp ()
modifyVar v f = writeVar_ v . f =<< readVar_ v

--delete variable
delVar :: (V a) -> Exp Bool
delVar = DelVar

delVar_ :: (V a) -> Exp ()
delVar_ v = DelVar v >> return ()

--ArrayVar is an indexed array with a signal attached to warn when the array is filled.
--each indexed elements starts empty (value=Nothing), and when the array is full, the signal is triggered.
--This is useful to wait for a serie of events to happen, and trigger a computation on the collected results.
data ArrayVar i a = ArrayVar (Message [(i, a)]) (V (Map i (Maybe a)))

--initialize an empty ArrayVar
newArrayVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => VarName -> [i] -> Exp (ArrayVar i a)
newArrayVar name l = do
    let list = map (\i -> (i, Nothing)) l
    v <- newVar_ name (fromList list)
    return $ ArrayVar (Message name) v

--initialize an empty ArrayVar, registering a callback that will be triggered when the array is filled
newArrayVar' :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => VarName -> [i] -> ([(i,a)] -> Exp ()) -> Exp (ArrayVar i a)
newArrayVar' name l f = do
    av@(ArrayVar m v) <- newArrayVar name l
    onMessage m $ f . messageData
    return av

--initialize an empty ArrayVar, registering a callback.
--the callback will be triggered when the array is filled, and then the ArrayVar will be deleted
newArrayVarOnce :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => VarName -> [i] -> ([(i,a)] -> Exp ()) -> Exp (ArrayVar i a)
newArrayVarOnce name l f = do
    av@(ArrayVar m v) <- newArrayVar name l
    onMessageOnce m (\a -> (f $ messageData a) >> (delVar_ v))
    return av

--store one value and the given index. If this is the last filled element, the registered callbacks are triggered.
putArrayVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> i -> a -> Exp ()
putArrayVar (ArrayVar m v) i a = do
    ar <- readVar_ v
    let ar2 = M.insert i (Just a) ar
    writeVar_ v ar2
    let finish = and $ map isJust $ elems ar2
    when finish $ sendMessage m (toList $ M.map fromJust ar2)

--get the messsage triggered when the array is filled
getArrayVarMessage :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> Exp (Message [(i, a)])
getArrayVarMessage (ArrayVar m _) = return m

--get the association array
getArrayVarData :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> Exp ([(i, Maybe a)])
getArrayVarData (ArrayVar _ v) = readVar_ v >>= return . toList

--get the association array with only the filled values
getArrayVarData' :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> Exp ([(i, a)])
getArrayVarData' v = getArrayVarData v >>= return . catMaybes . map sndMaybe

delArrayVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> Exp ()
delArrayVar (ArrayVar m v) = delAllEvents m >> delVar_ v

--register a callback on an event
onEvent :: (Event e) => e -> ((EventNumber, EventData e) -> Exp ()) -> Exp EventNumber
onEvent = OnEvent

--register a callback on an event, disregard the event number
onEvent_ :: (Event e) => e -> (EventData e -> Exp ()) -> Exp ()
onEvent_ e h = do
    OnEvent e (\(_, d) -> h d)
    return ()

-- set an handler for an event that will be triggered only once
onEventOnce :: (Event e) => e -> (EventData e -> Exp ()) -> Exp EventNumber
onEventOnce e h = do
    let handler (en, ed) = delEvent_ en >> h ed
    n <- OnEvent e handler
    return n

-- set an handler for an event that will be triggered only once
onEventOnce_ :: (Event e) => e -> (EventData e -> Exp ()) -> Exp ()
onEventOnce_ e h = do
    let handler (en, ed) = delEvent_ en >> h ed
    OnEvent e handler
    return ()

delEvent :: EventNumber -> Exp Bool
delEvent = DelEvent

delEvent_ :: EventNumber -> Exp ()
delEvent_ e = delEvent e >> return ()

delAllEvents :: (Event e) => e -> Exp ()
delAllEvents = DelAllEvents

sendMessage :: (Typeable a, Show a, Eq a) => Message a -> a -> Exp ()
sendMessage = SendMessage

sendMessage_ :: Message () -> Exp ()
sendMessage_ m = SendMessage m ()

onMessage :: (Typeable m) => Message m -> ((EventData (Message m)) -> Exp ()) -> Exp ()
onMessage m f = onEvent_ m f

onMessageOnce :: (Typeable m) => Message m -> ((EventData (Message m)) -> Exp ()) -> Exp ()
onMessageOnce m f = onEventOnce_ m f

--at each time provided, the supplied function will be called
schedule :: [UTCTime] -> (UTCTime -> Exp ()) -> Exp ()
schedule ts f = f' (TimeData (head ts)) where
    f' (TimeData t) = do
        let filtered = filter (>t) ts
        onEvent_ (Time (head filtered)) f'
        f t

schedule_ :: [UTCTime] -> Exp () -> Exp ()
schedule_ ts f = schedule ts (\_-> f)


--combine two rule responses
(&&.) :: RuleResponse -> RuleResponse -> Exp RuleResponse
(&&.) a@(BoolResp _) b@(MsgResp _) = b &&. a
(&&.) (BoolResp a) (BoolResp b) = return $ BoolResp $ a && b
(&&.) (MsgResp m1@(Message s1)) (MsgResp m2@(Message s2)) = do
    let m = Message (s1 ++ " and " ++ s2)
    v <- newArrayVarOnce (s1 ++ ", " ++ s2) [1::Integer, 2] (f m)
    return (MsgResp m) where
        f m ((_, a):(_, b):[]) = sendMessage m $ a && b
(&&.) (MsgResp m1@(Message s1)) (BoolResp b2) = do
    let m = Message (s1 ++ " and " ++ (show b2))
    onMessageOnce m1 (f m)
    return (MsgResp m) where
        f m (MessageData b1) = sendMessage m $ b1 && b2

and' :: [RuleResponse] -> Exp RuleResponse
and' l = foldM (&&.) (BoolResp True) l

activateRule :: RuleNumber -> Exp Bool
activateRule = ActivateRule

activateRule_ :: RuleNumber -> Exp ()
activateRule_ r = activateRule r >> return ()

rejectRule :: RuleNumber -> Exp Bool
rejectRule = RejectRule

rejectRule_ :: RuleNumber -> Exp ()
rejectRule_ r = rejectRule r >> return ()

--set victory to a list of players
setVictory :: [PlayerNumber] -> Exp ()
setVictory = SetVictory

--give victory to one player
giveVictory :: PlayerNumber -> Exp ()
giveVictory pn = SetVictory [pn]

getRules :: Exp [Rule]
getRules = GetRules

getActiveRules :: Exp [Rule]
getActiveRules = return . (filter ((== Active) . rStatus) ) =<< getRules

getRule :: RuleNumber -> Exp (Maybe Rule)
getRule rn = do
   rs <- GetRules
   return $ find (\(Rule {rNumber = n}) -> n == rn) rs

getRulesByNumbers :: [RuleNumber] -> Exp [Rule]
getRulesByNumbers rns = mapMaybeM getRule rns

getRuleFuncs :: Exp [RuleFunc]
getRuleFuncs = return . (map rRuleFunc) =<< getRules

addRule :: Rule -> Exp Bool
addRule r = AddRule r

suppressRule :: RuleNumber -> Exp Bool
suppressRule rn = DelRule rn

suppressAllRules :: Exp Bool
suppressAllRules = do
    rs <- getRules
    res <- mapM (suppressRule . rNumber) rs
    return $ and res

modifyRule :: RuleNumber -> Rule -> Exp Bool
modifyRule rn r = ModifyRule rn r

getPlayers :: Exp [PlayerInfo]
getPlayers = GetPlayers

--Get the total number of players
getPlayersNumber :: Exp Int
getPlayersNumber = getPlayers >>= return . length

getAllPlayerNumbers :: Exp [PlayerNumber]
getAllPlayerNumbers = do
   ps <- getPlayers
   return $ map playerNumber ps

immutableRule :: RuleNumber -> RuleFunc
immutableRule rn = RuleRule f where
   f r = do
      protectedRule <- getRule rn
      case protectedRule of
         Just pr -> case rRuleFunc r of
            RuleRule paramRule -> paramRule pr
            otherwise -> return $ BoolResp True
         Nothing -> return $ BoolResp True


-- | A rule will be always legal
legal :: RuleFunc
legal = RuleRule $ \_ -> return $ BoolResp True

-- | A rule will be always illegal
illegal :: RuleFunc
illegal = RuleRule $ \_ -> return $ BoolResp False

output :: String -> PlayerNumber -> Exp ()
output s pn = Output pn s

outputAll :: String -> Exp ()
outputAll s = do
    pls <- getPlayers
    mapM_ ((output s) . playerNumber) pls

-- asks the player pn to answer a question, and feed the callback with this data.
inputChoice :: (Enum a, Typeable a) => String -> (PlayerNumber -> a -> Exp ()) -> PlayerNumber -> Exp EventNumber
inputChoice title handler pn = onEventOnce (InputChoice pn title) ((handler pn) . inputChoiceData)

-- asks the player pn to answer a question, and feed the callback with this data.
inputChoice_ :: (Enum a, Typeable a) => String -> (a -> Exp ()) -> PlayerNumber -> Exp ()
inputChoice_ title handler pn = onEventOnce_ (InputChoice pn title) (handler . inputChoiceData)

getCurrentTime :: Exp(UTCTime)
getCurrentTime = CurrentTime

--  Rule samples:

-- This rule will activate automatically any new rule.
autoActivate :: RuleFunc
autoActivate = VoidRule $ onEvent_ (EvRule Proposed) (activateRule_ . rNumber . ruleData)

-- This rule establishes a list of criteria rules that will be used to test any incoming rule
-- the rules applyed shall give the answer immediatly
simpleApplicationRule :: RuleFunc
simpleApplicationRule = VoidRule $ do
    v <- newVar_ "rules" ([]:: [RuleNumber])
    onEvent_ (EvRule Proposed) (h v) where
        h v (RuleData rule) = do
            (rns:: [RuleNumber]) <- readVar_ v
            rs <- getRulesByNumbers rns
            oks <- mapM (applyRule rule) rs
            when (and oks) $ activateRule_ $ rNumber rule


-- active metarules are automatically used to evaluate a given rule
autoMetarules :: Rule -> Exp RuleResponse
autoMetarules r = do
    rs <- getActiveRules
    let rrs = mapMaybe f rs
    evals <- mapM (\rr -> rr r) rrs
    and' evals
    where
        f Rule {rRuleFunc = (RuleRule r)} = Just r
        f _ = Nothing

-- any incoming rule will be activate if all active meta rules agrees
applicationMetaRule :: RuleFunc
applicationMetaRule = VoidRule $ onEvent_ (EvRule Proposed) $ \(RuleData rule) -> do
            r <- autoMetarules rule
            case r of
                BoolResp b -> activateOrReject rule b
                MsgResp m -> onMessageOnce m $ (activateOrReject rule) . messageData
            return ()

applyRule :: Rule -> Rule -> Exp Bool
applyRule (Rule {rRuleFunc = rf}) r = do
    case rf of
        RuleRule f1 -> f1 r >>= return . boolResp
        otherwise -> return False

data ForAgainst = For | Against deriving (Typeable, Enum, Show, Eq)

--rule that performs a vote for a rule on all players. The provided function is used to count the votes.
vote :: ([(PlayerNumber, ForAgainst)] -> Bool) -> RuleFunc
vote f = RuleRule $ \rule -> do
    pns <- getAllPlayerNumbers
    let rn = show $ rNumber rule
    let m = Message ("Unanimity for " ++ rn)
    --create an array variable to store the votes. A message with the result of the vote is sent upon completion
    voteVar <- newArrayVarOnce ("Votes for " ++ rn) pns (sendMessage m . f)
    --create inputs to allow every player to vote and store the results in the array variable
    let askPlayer = inputChoice ("Vote for rule " ++ rn) (putArrayVar voteVar)
    mapM_ askPlayer pns
    return $ MsgResp m

--assess the vote results according to a unanimity
unanimity :: [(PlayerNumber, ForAgainst)] -> Bool
unanimity l = ((length $ filter ((== Against) . snd) l) == 0)

--assess the vote results according to a majority
majority :: [(PlayerNumber, ForAgainst)] -> Bool
majority l = ((length $ filter ((== For) . snd) l) >= length l)

activateOrReject :: Rule -> Bool -> Exp ()
activateOrReject r b = if b then activateRule_ (rNumber r) else rejectRule_ (rNumber r)

--rule that performs a vote for a rule on all players. The provided function is used to count the votes,
--it will be called when every players has voted or when the time limit is reached
voteWithTimeLimit :: ([(PlayerNumber, ForAgainst)] -> Bool) -> UTCTime -> RuleFunc
voteWithTimeLimit f t = RuleRule $ \rule -> do
    pns <- getAllPlayerNumbers
    let rn = show $ rNumber rule
    let m = Message ("Unanimity for " ++ rn)
    --create an array variable to store the votes. A message with the result of the vote is sent upon completion
    voteVar <- newArrayVarOnce ("Votes for " ++ rn) pns (sendMessage m . f)
    --create inputs to allow every player to vote and store the results in the array variable
    let askPlayer = inputChoice ("Vote for rule " ++ rn) (putArrayVar voteVar)
    ics <- mapM askPlayer pns
    --time limit
    onEventOnce_ (Time t) $ \_ -> do
        getArrayVarData' voteVar >>= sendMessage m . f
        delArrayVar voteVar
        mapM_ delEvent ics
    return $ MsgResp m


--create a value initialized to zero for each players
--manages players joining and leaving
createValueForEachPlayer :: String -> Exp ()
createValueForEachPlayer s = do
    pns <- getAllPlayerNumbers
    v <- newVar_ s $ map (,0::Int) pns
    onEvent_ (Player Arrive) $ \(PlayerData p) -> modifyVar v ((playerNumber p, 0):)
    onEvent_ (Player Leave) $ \(PlayerData p)   -> modifyVar v $ filter $ (/= playerNumber p) . fst

modifyValueOfPlayer :: PlayerNumber -> String -> (Int -> Int) -> Exp ()
modifyValueOfPlayer pn var f = modifyVar (V var::V [(Int, Int)]) $ map $ (\(a,b) -> if a == pn then (a, f b) else (a,b))

modifyAllValues :: String -> (Int -> Int) -> Exp ()
modifyAllValues var f = modifyVar (V var::V [(Int, Int)]) $ map $ second f

-- Le joueur p ne peut plus jouer:
noPlayPlayer :: PlayerNumber -> RuleFunc
noPlayPlayer p = RuleRule $ \r -> return $ BoolResp $ (rProposedBy r) /= p


-- | All rules from player p are erased:
eraseAllRules :: PlayerNumber -> Exp Bool
eraseAllRules p = do
    rs <- getRules
    let myrs = filter (\r ->  (rProposedBy r) == p) rs
    res <- mapM (suppressRule . rNumber) myrs
    return $ and res

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f

parse822Time :: String -> UTCTime
parse822Time = zonedTimeToUTC
              . fromJust
              . parseTime defaultTimeLocale rfc822DateFormat

sndMaybe :: (a, Maybe b) -> Maybe (a,b)
sndMaybe (a, Just b) = Just (a,b)
sndMaybe (a, Nothing) = Nothing
