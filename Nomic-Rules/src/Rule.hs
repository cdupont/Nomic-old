{-# LANGUAGE DeriveDataTypeable, GADTs, ScopedTypeVariables#-}

-- | This module defines a Rule, which is a structure that allow the player to define if an input Rule is legal or not.
-- That means, a Rule defines if a Rule is legal or not. 
module Rule where

import Expression
import Data.Typeable
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Time
import Data.Function
import Data.Map hiding (map, filter, insert, mapMaybe)
import qualified Data.Map as M (map, insert)

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

--delete variable
delVar :: (V a) -> Exp Bool
delVar = DelVar

delVar_ :: (V a) -> Exp ()
delVar_ v = DelVar v >> return ()

--ArrayVar is an indexed array with a signal attached to warn when the array is filled.
--each indexed elements starts empty, and when the array is full, the signal is triggered.
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
    v <- newArrayVar name l
    subscribeArrayVar v f
    return v

--store one value and the given index. If this is the last filled element, the registered callbacks are triggered.
putArrayVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> i -> a -> Exp ()
putArrayVar (ArrayVar m v) i a = do
    ar <- readVar_ v
    let ar2 = M.insert i (Just a) ar
    let finish = and $ map isJust $ elems ar2
    writeVar_ v ar2
    when finish $ sendMessage m (toList $ M.map fromJust ar2)

--get the messsage triggered when the array is filled
getArrayVarMessage :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> Exp (Message [(i, a)])
getArrayVarMessage (ArrayVar m _) = return m

--register a callback with the ArrayVar.
subscribeArrayVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArrayVar i a) -> ([(i,a)] -> Exp ()) -> Exp ()
subscribeArrayVar (ArrayVar m v) f = onMessage m (f . messageData)

onEvent :: (Event e) => e -> ((EventNumber, EventData e) -> Exp ()) -> Exp EventNumber
onEvent = OnEvent

onEvent_ :: (Event e) => e -> (EventData e -> Exp ()) -> Exp ()
onEvent_ e h = do
    OnEvent e (\(_, d) -> h d)
    return ()

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

sendMessage :: (Typeable a, Show a, Eq a) => Message a -> a -> Exp ()
sendMessage = SendMessage

sendMessage_ :: Message () -> Exp ()
sendMessage_ m = SendMessage m ()

onMessage :: (Typeable m) => Message m -> ((EventData (Message m)) -> Exp ()) -> Exp ()
onMessage m f = onEvent_ m f

--at each time provided, the supplied function will be called
schedule :: [UTCTime] -> (UTCTime -> Exp ()) -> Exp ()
schedule ts f = f' (TimeData (head ts)) where
    f' (TimeData t) = do
        let filtered = filter (>t) ts
        onEvent_ (Time (head filtered)) f'
        f t

(&&.) :: RuleResponse -> RuleResponse -> Exp RuleResponse
(&&.) (MsgResp m1@(Message s1)) (MsgResp m2@(Message s2)) = do
    let m = Message (s1 ++ " and " ++ s2)
    v <- newArrayVar' (s1 ++ ", " ++ s2) [1::Integer, 2] (f m)
    return (MsgResp m) where
        f m ((_, a):(_, b):[]) = do
            let r = a && b
            sendMessage m r

(&&.) (MsgResp m1@(Message s1)) (BoolResp b2) = do
    let m = Message (s1 ++ " and " ++ (show b2))
    onMessage m1 (f m)
    return (MsgResp m) where
        f m (MessageData b1) = do
            let r = b1 && b2
            sendMessage m r

(&&.) a@(BoolResp _) b@(MsgResp _) = b &&. a
(&&.) (BoolResp a) (BoolResp b) = return $ BoolResp $ a && b

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

--give victory to one player
giveVictory :: PlayerNumber -> Exp ()
giveVictory pn = SetVictory [pn]


--set victory to someone or no-one
setVictory :: Maybe PlayerNumber -> Exp ()
setVictory v = SetVictory $ maybeToList v
--modify (\game -> game { victory = v})

--clear all actions
--clearActions :: Exp ()
--clearActions = modify (\game -> game { actionResults = []})

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

for     = "For"
against = "Against"
blank   = "Blank"

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
inputChoice :: (Enum a, Typeable a) => String -> (PlayerNumber -> a -> Exp ()) -> PlayerNumber -> Exp ()
inputChoice title handler pn = onEventOnce_ (InputChoice pn title) ((handler pn) . inputChoiceData)

-- asks the player pn to answer a question, and feed the callback with this data.
inputChoice_ :: (Enum a, Typeable a) => String -> (a -> Exp ()) -> PlayerNumber -> Exp ()
inputChoice_ title handler pn = onEventOnce_ (InputChoice pn title) (handler . inputChoiceData)


--  Rule samples:

-- This rule will activate automatically any new rule.
autoActivate :: RuleFunc
autoActivate = VoidRule $ onEvent_ RuleProposed (activateRule_ . rNumber . ruleProposedData)

-- This rule establishes a list of criteria rules that will be used to test any incoming rule
-- the rules applyed shall give the answer immediatly
simpleApplicationRule :: RuleFunc
simpleApplicationRule = VoidRule $ do
    newVar_ "rules" ([]:: [RuleNumber])
    onEvent_ RuleProposed h where
        h (RuleProposedData rule) = do
            mrns <- readVar (V "rules")
            case mrns of
                Just (rns:: [RuleNumber]) -> do
                    rs <- getRulesByNumbers rns
                    oks <- mapM (applyRule rule) rs
                    if (and oks) then do
                        ActivateRule $ rNumber rule
                        return ()
                        else return ()
                Nothing -> return ()

-- This rule establishes a list of criteria rules that will be used to test any incoming rule
-- the rules applyed can give their answer later
applicationRule :: RuleFunc
applicationRule = VoidRule $ do
    NewVar "rules" ([]:: [RuleNumber])
    onEvent_ RuleProposed h where
        h (RuleProposedData rule) = do
            mrns <- ReadVar (V "rules")
            case mrns of
                Just (rns:: [RuleNumber]) -> do
                    rs <- getRulesByNumbers rns
                    oks <- mapM (applyRule rule) rs
                    if (and oks) then do
                        ActivateRule $ rNumber rule
                        return ()
                        else return ()
                Nothing -> return ()


applyRule :: Rule -> Rule -> Exp Bool
applyRule (Rule {rRuleFunc = rf}) r = do
    case rf of
        RuleRule f1 -> f1 r >>= return . boolResp
        otherwise -> return False

data ForAgainst = For | Against deriving (Typeable, Enum, Show, Eq)

--rule that enforce an unanimity vote to be cast for every new rules
unanimityVote :: RuleFunc
unanimityVote = RuleRule $ \rule -> do
        pns <- getAllPlayerNumbers
        let rn = rNumber rule
        let m = Message ("Unanimity for " ++ (show rn))
        --create a variable to store the votes
        voteVar <- newArrayVar' ("Votes for " ++ (show rn)) pns (voteCompleted m)
        --create inputs to allow every player to vote and store the results in the variable
        let askPlayer = inputChoice ("Vote for rule " ++ (show rn)) (putArrayVar voteVar)
        mapM_ askPlayer pns
        return $ MsgResp m

--count votes and send the result
voteCompleted :: (Message Bool) -> [(PlayerNumber, ForAgainst)] -> Exp ()
voteCompleted m l = sendMessage m $ ((length $ filter ((== Against) . snd) l) == 0)

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
applicationMetaRule = VoidRule $ do
    onEvent_ RuleProposed $ \(RuleProposedData rule) -> do
            r <- autoMetarules rule
            case r of
                BoolResp b -> activateOrReject rule b
                MsgResp m -> onMessage m $ (activateOrReject rule) . messageData
            return ()

activateOrReject :: Rule -> Bool -> Exp ()
activateOrReject r b = if b then activateRule_ (rNumber r) else rejectRule_ (rNumber r)


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


-- | Rule that disapears once executed: (exemple #15)
-- autoErase :: Exp Bool
-- autoErase = rule autoErase

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f



