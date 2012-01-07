
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
--readVar :: String -> Exp Int
--readVar name = do
--   vars <- gets variables
--   case find (\(myName, val) -> myName == name) vars of
--      Nothing -> error "no variable by that name"
--      Just (n, v) -> return v

--variable writing
--writeVar :: String -> Int -> StateT Game Exp ()
--writeVar name val = do
--   vars <- gets variables
--   let newVars = replaceWith (\(n, v) -> n == name) (name, val) vars
--   case find (\(myName, val) -> myName == name) vars of
--      Nothing -> error "no variable by that name"
--      Just (n, v) -> modify (\game -> game { variables = newVars})



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

--to suppress?
--isOfficial :: Rule -> StateT Game Exp Bool
--isOfficial r = do
--   rs <- gets rules
--   case find (\(Rule {rNumber = n}) -> n == (rNumber r)) rs of
--      Nothing -> return False
--      Just _ -> return True

getRule :: RuleNumber -> Exp (Maybe Rule)
getRule rn = do
   rs <- GetRules
   return $ find (\(Rule {rNumber = n}) -> n == rn) rs

addRule :: Rule -> StateT Game Exp ()
addRule r = modify (\g -> g { rules = r : (rules g)})

suppressRule :: RuleNumber -> StateT Game Exp ()
suppressRule rn = modify (\g -> g { rules = filter (\Rule {rNumber = myRn} -> myRn /= rn) (rules g)})

suppressAllRules :: StateT Game Exp ()
suppressAllRules = modify (\g -> g { rules = []})

modifyRule :: RuleNumber -> Rule -> StateT Game Exp ()
modifyRule rn r = suppressRule rn >> addRule r

getAllPlayers :: StateT Game Exp [PlayerInfo]
getAllPlayers = gets players

getAllPlayerNumbers :: StateT Game Exp [PlayerNumber]
getAllPlayerNumbers = do
   ps <- gets players
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



-- | the Rule type allows to describe laws over Rules themselves.
-- Rule :: Rule -> (Legal | Illegal)  
--data Rule = Rule (Obs Bool)            -- contruct a Rule
--          | MustBeEgalTo Rule          -- a (tested) rule must be equal to the rule in parameter
--          | TestRuleOver Rule          -- a rule must declare the rule in parameter as legal
--          | OfficialRule Int           -- a rule must be legal to the official rule #N
--          deriving (Typeable) --, Show, Eq) 





-- | A rule will be legal if the observable is True
--rule :: Obs Bool -> Rule
--rule = Rule

-- | A rule will be always legal
--legal :: Rule
--legal = rule true

-- | A rule will be always illegal
--illegal :: Rule
--illegal = rule false

--  Rule samples:

-- | Vote for something
--voteFor :: String -> PlayerNumber -> Rule
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
--immutable :: Int -> Rule
--immutable = TestRuleOver . OfficialRule


-- | Suppress rule n: (example #2)
--eraseRule :: Int -> Rule
--eraseRule = rule . erase . konst

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

--noPlayPlayer :: PlayerNumber -> Rule   TODO: reactivate
--noPlayPlayer p = mustNotBe (oPlayerTurn `oEqu` (oInt p))


-- | All rules from player p are erased:
--eraseAllRules :: PlayerNumber -> Rule
--eraseAllRules p = rule . autoErase . konst


-- Personne ne peut jouer au tour n:

--noPlayTurn :: Turn -> Rule
--noPlayTurn t = mustNotBe (oTurn `oEqu` (oConst t))


-- Le joueur 1 ne peut pas jouer au prochain tour (exemple #17)
-- Les règles du joueur donné en argument seront toujours rejectées (illégales):

--ex17 = noPlayPlayer 1 `rOr` noPlayTurn 2


-- Le joueur 2 doit faire un tour sur lui-même (exemple #19)

--  actionForPlayer :: Player -String -Rule -Rule -Rule
--  actionForPlayer p whatToDo ifOK ifNOK = cond (oListAnd $ oMap (\j -oVote s j) ojs) ifOK ifNOK  --todo fix


-- | Rule that disapears once executed: (exemple #15)
--autoEraseRule :: Rule
--autoEraseRule = rule autoErase


