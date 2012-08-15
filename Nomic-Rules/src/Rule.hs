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

data ArraySVar i a = ArraySVar (Message ()) (V (Map i (Maybe a)))

newArraySVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => VarName -> [i] -> Exp (ArraySVar i a)
newArraySVar name l = do
    let list = map (\i -> (i, Nothing)) l
    v <- newVar_ name (fromList list)
    return $ ArraySVar (Message name) v

newArraySVar' :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => VarName -> [i] -> ([(i,a)] -> Exp ()) -> Exp (ArraySVar i a)
newArraySVar' name l f = do
    v <- newArraySVar name l
    subscribeArraySVar v f
    return v

putArraySVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArraySVar i a) -> i -> a -> Exp ()
putArraySVar (ArraySVar m v) i a = do
    ar <- readVar_ v
    let ar2 = M.insert i (Just a) ar
    let finish = and $ map isJust $ elems ar2
    writeVar_ v ar2
    when finish $ sendMessage m ()

subscribeArraySVar :: (Ord i, Typeable a, Show a, Eq a, Typeable i, Show i) => (ArraySVar i a) -> ([(i,a)] -> Exp ()) -> Exp ()
subscribeArraySVar (ArraySVar m v) f = do
    onMessage m f' where
        f' _ = do
            ar <- readVar_ v
            f $ toList $ M.map fromJust ar

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

--schedule' :: [UTCTime] -> (UTCTime -> Exp ()) -> Exp ()
--schedule' ts f = unfoldrM f' ts where
--        f' (TimeData t) = do
--            let filtered = filter (>t) ts
--            onEvent_ (Time (head filtered)) f'

activateRule :: RuleNumber -> Exp Bool
activateRule = ActivateRule

activateRule_ :: RuleNumber -> Exp ()
activateRule_ r = activateRule r >> return ()


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
            otherwise -> return True
         Nothing -> return True


-- | A rule will be always legal
legal :: RuleFunc
legal = RuleRule $ \_ -> return True

-- | A rule will be always illegal
illegal :: RuleFunc
illegal = RuleRule $ \_ -> return False

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
        RuleRule f1 -> (f1 r)
        otherwise -> return False

data ForAgainst = For | Against deriving (Typeable, Enum, Show, Eq)

--rule that enforce an unanimity vote to be cast for every new rules
unanimityVote :: RuleFunc
unanimityVote = VoidRule $ do
   onEvent_ RuleProposed newRule where
      --if a new rule is proposed
      newRule (RuleProposedData rule) = do
         pns <- getAllPlayerNumbers
         --create a variable to store the votes
         voteVar <- newArraySVar' ("Votes for " ++ (show $ rNumber rule)) pns (voteCompleted $ rNumber rule)
         --create inputs to allow every player to vote
         let askPlayer = inputChoice ("Vote for rule " ++ rName rule) (putArraySVar voteVar)
         mapM_ askPlayer pns



--activate the rule if votes are positive
voteCompleted :: RuleNumber -> [(PlayerNumber, ForAgainst)] -> Exp ()
voteCompleted rn l = do
   if ((length $ filter ((== Against) . snd) l) == 0)
      then ActivateRule rn
      else RejectRule   rn
   return ()


-- active metarules are automatically used to evaluate a given rule
autoMetarules :: Rule -> Exp Bool
autoMetarules r = do
    rs <- getActiveRules
    let rrs = mapMaybe f rs
    evals <- mapM (\rr -> rr r) rrs
    return $ and evals
    where
        f Rule {rRuleFunc = (RuleRule r)} = Just r
        f _ = Nothing



-- Le joueur p ne peut plus jouer:
noPlayPlayer :: PlayerNumber -> RuleFunc
noPlayPlayer p = RuleRule $ \r -> do
    return $ (rProposedBy r) /= p


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

