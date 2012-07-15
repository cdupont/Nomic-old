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
--TODO verify unicity
newVar :: String -> Int -> Exp Bool
newVar name def = NewVar name def


--variable reading
--TODO error handling
readVar :: String -> Exp (Maybe Int)
readVar name = ReadVar name


--variable writing
writeVar :: String -> Int -> Exp Bool
writeVar name val = WriteVar name val

onEvent :: (Event e) => e -> ((EventNumber, EventData e) -> Exp ()) -> Exp EventNumber
onEvent = OnEvent

onEvent_ :: (Event e) => e -> (EventData e -> Exp ()) -> Exp ()
onEvent_ e h = do
    OnEvent e (\(_, d) -> h d)
    return ()


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

inputChoice :: String -> [String] -> ((EventNumber, Int) -> Exp ()) -> PlayerNumber -> Exp ()
inputChoice title choices handler pn = OnEvent (InputChoice pn title) (\(a, InputChoiceData b) -> handler (a, b)) >> return ()

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
    NewVar "rules" ([]::[RuleNumber])
    onEvent_ RuleProposed h where
        h (RuleProposedData rule) = do
            mrns <- ReadVar "rules"
            case mrns of
                Just (rns::[RuleNumber]) -> do
                    rs <- getRulesByNumbers rns
                    oks <- mapM (applyRule rule) rs
                    if (and oks) then do
                        ActivateRule $ rNumber rule
                        return ()
                        else return ()
                Nothing -> return ()

applicationRule :: RuleFunc
applicationRule = VoidRule $ do
    NewVar "rules" ([]::[RuleNumber])
    onEvent_ RuleProposed h where
        h (RuleProposedData rule) = do
            mrns <- ReadVar "rules"
            case mrns of
                Just (rns::[RuleNumber]) -> do
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

unanimityVote :: RuleFunc
unanimityVote = VoidRule $ do
   onEvent_ RuleProposed newRule where
      newRule (RuleProposedData rule) = do
         let voteVar = "Votes for " ++ (show $ rNumber rule)
         let endVoteMsg = "vote completed"
         NewVar voteVar ([]::[Int])
         onEvent_ (Message endVoteMsg) (voteCompleted voteVar)
         pns <- GetPlayers
         mapM_ (inputChoice ("Vote for rule" ++ rName rule) [for, against] (updateVote voteVar endVoteMsg (rNumber rule))) (map playerNumber pns)
      voteCompleted varName (MessageData (rule::Rule)) = do
         isPositive <- evalVotes varName
         case (cast rule) of
            Just r -> if isPositive
                then ActivateRule r
                else RejectRule   r
            Nothing -> return False
         return ()


-- store the vote of a player and suppress the corresponding event
updateVote :: Enum a => VarName -> String -> RuleNumber -> (EventNumber, a) -> Exp ()
updateVote voteVar endVoteMsg rn (en, choice) = do
    DelEvent en
    mvs <- ReadVar voteVar
    pnumber <- getPlayersNumber
    case mvs of
        Just (votes::[Int]) -> do
            WriteVar voteVar ((fromEnum choice): votes)
            if (length votes == pnumber)
               then (SendMessage endVoteMsg rn)
               else return ()
        Nothing -> return ()

-- returns whereas a vote outcome is positive or negative
evalVotes :: VarName -> Exp Bool
evalVotes voteVar = do
    mvs <- ReadVar voteVar
    pnumber <- getPlayersNumber
    case mvs of
        Just (votes::[Int]) -> do
           if ((length $ filter (== 1) votes) == pnumber)
              then return True
              else return False
        Nothing -> return False


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

