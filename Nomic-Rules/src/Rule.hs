{-# LANGUAGE DeriveDataTypeable, GADTs, ScopedTypeVariables#-}

-- | This module defines a Rule, which is a structure that allow the player to define if an input Rule is legal or not.
-- That means, a Rule defines if a Rule is legal or not. 
module Rule where

import Expression
import Data.Typeable
import Control.Monad.State
import Data.List
import Data.Maybe


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

getRule :: RuleNumber -> Exp (Maybe Rule)
getRule rn = do
   rs <- GetRules
   return $ find (\(Rule {rNumber = n}) -> n == rn) rs

getRulesByNumbers :: [RuleNumber] -> Exp [Rule]
getRulesByNumbers rns = mapMaybeM getRule rns

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

--choiceVote2 :: Exp String -> Exp Int -> Exp String
--choiceVote2 s pn   = do
--   c <- InputChoice s pn $ Const [for, against]
--   return c

--choiceVote3 :: Exp String -> Exp Int -> Exp String
--choiceVote3 s pn   = InputChoice s pn $ Const [for, against, blank]

--voteReason :: Exp String -> Exp PlayerNumber -> Exp Bool
--voteReason s pn = do
--   s <- choiceVote2 s pn
--   return $ s == for

--unanimityVote :: RuleFunc
--unanimityVote = makePureMetaRule $ \r -> do
--   pns <- GetPlayers
--   allVotes <- mapM ((voteReason $ const_ $ "Please vote for rule " ++ (show $ rNumber r)) . const_ . playerNumber) pns
--   return $ (length allVotes) == (length pns)

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

inputChoice :: (Enum a, Typeable a) => String -> (a -> Exp ()) -> PlayerNumber -> Exp ()
inputChoice title handler pn = onEventOnce_ (InputChoice pn title) (\(InputChoiceData b) -> handler (b)) >> return ()

--  Rule samples:

-- This rule will activate automatically any new rule.
autoActivate :: RuleFunc
autoActivate = VoidRule $ do
    OnEvent RuleProposed h
    return () where
        h (_, RuleProposedData rule) = do
            ActivateRule $ rNumber rule
            return ()

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

data ForAgainst = For | Against deriving (Typeable, Enum)

--rule that enforce an unanimity vote to be cast for every new rules
unanimityVote :: RuleFunc
unanimityVote = VoidRule $ do
   onEvent_ RuleProposed newRule where
      --if a new rule is proposed
      newRule (RuleProposedData rule) = do
         pns <- GetPlayers
         if (length pns /= 0) then do
            let endVoteMsg = Message "vote completed"
            --create a variable to store the votes
            voteVar <- newVar_ ("Votes for " ++ (show $ rNumber rule)) ([]:: [Int])
            --create an event for when the vote will be completed
            onEventOnce_ endVoteMsg (voteCompleted voteVar)
            let updateVote' = updateVote voteVar endVoteMsg (rNumber rule)
                updateVote' :: ForAgainst -> Exp ()
            --create inputs to allow every player to vote
            mapM_ (inputChoice ("Vote for rule " ++ rName rule) updateVote') (map playerNumber pns)
            else return ()

-- store the vote of a player and warn if everyone voted
updateVote :: Enum a => (V [Int]) -> (Message RuleNumber) -> RuleNumber -> a -> Exp ()
updateVote voteVar msg rn choice = do
    votes <- readVar_ voteVar
    pnumber <- getPlayersNumber
    writeVar_ voteVar ((fromEnum choice): votes)
    if (length votes + 1 == pnumber)
       then sendMessage msg rn
       else return ()

--activate the rule if votes are positive
voteCompleted :: (V [Int]) -> (EventData (Message RuleNumber)) -> Exp ()
voteCompleted voteVar (MessageData rn) = do
   isPositive <- evalVotes voteVar
   if isPositive
      then ActivateRule rn
      else RejectRule   rn
   return ()

-- returns whereas a vote outcome is positive or negative
evalVotes :: (V [Int]) -> Exp Bool
evalVotes voteVar = do
    votes <- readVar_ voteVar
    pnumber <- getPlayersNumber
    if ((length $ filter (== 0) votes) == pnumber)
       then return True
       else return False



-- | Vote for something
--voteFor :: String -> PlayerNumber -> RuleFunc
--voteFor s n = rule (oVoteReason (Konst s) (Konst n))

-- | Vote of one personne. (example #14)
--voteRule :: PlayerNumber -> Rule
--voteRule = voteFor "Please vote"

-- | Unanimous vote (example #4)
--allVoteRule :: Rule
--allVoteRule = rule oUnanimityVote


-- | Rule egal to official rule #n:
--officialRule :: Int -> Rule
--officialRule = OfficialRule

-- Exemple 13: La démocratie est abolie. Vive le nouveau Roi, Joueur #1! 
-- Cette exemple doit être accompli en plusieurs fois.
-- 1. Dabord supprimer la règle de protection des immuables: eraseRule 2
-- 2. supprimer le vote à l'unanimité: eraseRule 1
-- 2. Instaurer la monarchie, avec le joueur 1 comme Roi: voteRule 1


-- Referendum des joueurs: (exemple #15)
-- 1er argument: Titre du référendum
-- 2° argument: loi si majorité de oui
-- 3° argument: joueurs participant au référendum

--referendum :: String -> Rule -> [PlayerNumber] -> Rule  TODO: reactivate
--referendum s r1 js = cond (oListAnd $ map (\j -> oVote s j) ojs) r1 zeroRule where --todo fix. un référendum s'applique t'il aux règle déjà présentes?
--     ojs = map oInt js  	

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

