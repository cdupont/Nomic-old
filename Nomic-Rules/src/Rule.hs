
{-# LANGUAGE DeriveDataTypeable, GADTs#-}

-- | This module defines a Rule, which is a structure that allow the player to define if an input Rule is legal or not.
-- That means, a Rule defines if a Rule is legal or not. 
module Rule where

import Expression
import Data.Typeable
import Control.Monad.State
import Data.List
import Data.Maybe

-- A meta rule is a rule that takes a rule in parameter, and returns a boolean stating the legality
-- of the input rule. It can also read/write the current game state.
-- A pure meta rule is a meta rule that has no impact on the state of the game if no input rule is given
-- (it deals only with evaluating other rules).
-- A normal rule is just a rule that changes the current state.


-- Helper function to construct a normal rule.
-- argument: your state changing rule.
-- return: a Nomic rule.
makeNormalRule :: Exp () -> RuleFunc
makeNormalRule s = RuleFunc (\_ -> s >> return True)


-- Helper function to construct a meta rule.
-- argument: your function.
-- return: a Nomic rule.
makeMetaRule :: (Maybe Rule -> (Exp Bool)) -> RuleFunc
makeMetaRule f = RuleFunc f

-- Helper function to construct a pure meta rule.
-- argument: your function.
-- return: a Nomic rule.
makePureMetaRule :: (Rule -> (Exp Bool)) -> RuleFunc
makePureMetaRule f = RuleFunc g
   where g Nothing = return True
         g (Just r) = f r


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


getRule :: RuleNumber -> Exp (Maybe Rule)
getRule rn = do
   rs <- GetRules
   return $ find (\(Rule {rNumber = n}) -> n == rn) rs

addRule :: Rule -> Exp Bool
addRule r = AddRule r

suppressRule :: RuleNumber -> Exp Bool
suppressRule rn = DelRule rn

getRules :: Exp [Rule]
getRules = GetRules

suppressAllRules :: Exp Bool
suppressAllRules = do
    rs <- getRules
    res <- mapM (suppressRule . rNumber) rs
    return $ and res

modifyRule :: RuleNumber -> Rule -> Exp Bool
modifyRule rn r = ModifyRule rn r

getPlayers :: Exp [PlayerInfo]
getPlayers = GetPlayers

getAllPlayerNumbers :: Exp [PlayerNumber]
getAllPlayerNumbers = do
   ps <- getPlayers
   return $ map playerNumber ps

for     = "For"
against = "Against"
blank   = "Blank"

choiceVote2 :: Exp String -> Exp Int -> Exp String
choiceVote2 s pn   = do
   c <- InputChoice s pn $ Const [for, against]
   return c

choiceVote3 :: Exp String -> Exp Int -> Exp String
choiceVote3 s pn   = InputChoice s pn $ Const [for, against, blank]

voteReason :: Exp String -> Exp PlayerNumber -> Exp Bool
voteReason s pn = do
   s <- choiceVote2 s pn
   return $ s == for

unanimityVote :: RuleFunc
unanimityVote = makePureMetaRule $ \r -> do
   pns <- GetPlayers
   allVotes <- mapM ((voteReason $ const_ $ "Please vote for rule " ++ (show $ rNumber r)) . const_ . playerNumber) pns
   return $ (length allVotes) == (length pns)

immutableRule :: RuleNumber -> RuleFunc
immutableRule rn = makePureMetaRule $ \r -> do
   protectedRule <- getRule rn
   let (RuleFunc ruleFunction) = rRuleFunc r
   case protectedRule of
      Just pr -> ruleFunction $ Just pr
      Nothing -> return True


-- | A rule will be legal if the observable is True
--rule :: Obs Bool -> Rule
--rule = Rule

-- | A rule will be always legal
legal :: RuleFunc
legal = makePureMetaRule $ \_ -> return True

-- | A rule will be always illegal
illegal :: RuleFunc
illegal = makePureMetaRule $ \_ -> return False

output :: String -> PlayerNumber -> Exp ()
output s pn = Output pn s

--  Rule samples:

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

-- | Do not modify rule #n: (example #18)
immutable :: RuleNumber -> RuleFunc
immutable rn = makePureMetaRule $ \r -> do
    protectedRule <- getRule rn
    let testedRule = ruleFunc $ rRuleFunc r
    case protectedRule of
       Just pr -> testedRule $ Just pr
       Nothing -> return True


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
noPlayPlayer p = makePureMetaRule $ \r -> do
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


