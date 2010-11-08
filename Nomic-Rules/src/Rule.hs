
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable#-}

-- | This module defines a Rule, which is a structure that allow the player to define if an input Rule is legal or not.
-- That means, a Rule defines if a Rule is legal or not. 
module Rule where

import Observable
import Data.Typeable



-- | the Rule type allows to describe laws over Rules themselves.
-- Rule :: Rule -> (Legal | Illegal)  
data Rule = MustBeEgalTo Rule          -- a (tested) rule must be equal to the rule in parameter
          | Legal                      -- a rule will allways be declared legal
          | Illegal                    -- a rule will allways be declared illegal
          | Rand Rule Rule             -- a rule must be legal according to both rules in parameters
          | Ror Rule Rule              -- a rule must be legal according to one of the rules in parameters
          | Rnot Rule                  -- a rule must be illegal according to the rule in parameter
          | Cond (Obs Bool) Rule Rule  -- a rule must be legal to the first rule in parameter or the other, depending on the Observable
          | TestRuleOver Rule          -- a rule must declare the rule in parameter as legal
          | OfficialRule Int           -- a rule must be legal to the official rule #N
          deriving (Typeable, Show, Eq) 

defaultRule = "Legal"

-- | A rule will be always legal
legal :: Rule
legal = Legal

-- | A rule will be always illegal
illegal :: Rule
illegal = Illegal

-- | choose a rule or another depending on an observable
cond :: Obs Bool -> Rule -> Rule -> Rule
cond = Cond

-- | A rule will be legal if the observable is True
mustBe :: Obs Bool -> Rule
mustBe o = cond o legal illegal 

mustBe' :: Obs Bool -> Rule
mustBe' o = cond o legal illegal 

-- | A rule will be illegal if the observable is True
mustNotBe :: Obs Bool -> Rule
mustNotBe o = cond o illegal legal 

-- | A rule will be legal if it complies with both rules in argument
rAnd :: Rule -> Rule -> Rule
rAnd = Rand

-- | A rule will be legal if it complies with one rule in argument
rOr :: Rule -> Rule -> Rule
rOr = Ror

-- | A rule will be legal if it complies with only one rule in argument
rXOr :: Rule -> Rule -> Rule
rXOr a b = (a `rAnd` (rNot b)) `rOr` ((rNot a) `rAnd` b)

-- | A rule will be legal if it is egal to the argument
mustBeEgalTo :: Rule -> Rule
mustBeEgalTo = MustBeEgalTo 

-- | A rule will be legal if it is illegal to the rule in argument
rNot :: Rule -> Rule
rNot = Rnot 



--  Rule samples:

-- | Vote for something
voteFor :: String -> PlayerNumber -> Rule
voteFor s n = mustBe (oVote (oConst s) (oConst n))

-- | Vote of one personne. (example #14)
voteRule :: PlayerNumber -> Rule
voteRule p = voteFor "Please vote" p

-- | Unanimous vote (example #4)
allVoteRule :: Rule
allVoteRule = voteRule 1 `rAnd` voteRule 2
--allVoteRule = mustBe (oListAnd $ Map (Vote $ Konst "Please vote") AllPlayers)


-- | Rule that do not modify official rules during it's execution (itself including):
noModify :: Rule
noModify = mustBe oRuleOfficial

noModify' :: Rule -> Rule
noModify' r = cond oRuleOfficial legal r

-- | Rule egal to official rule #n:
officialRule :: Int -> Rule
officialRule n = OfficialRule n

-- | Do not modify rule #n: (example #18)
immutable :: Int -> Rule
immutable n = TestRuleOver $ OfficialRule n


-- | Suppress rule n: (example #2)
eraseRule :: Int -> Rule
eraseRule n = cond (oRuleNumber `oEqu` (oConst n)) illegal legal

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
eraseAllRules :: PlayerNumber -> Rule
eraseAllRules p = mustNotBe (oRuleProposedBy `oEqu` (oConst p))


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
autoErase :: Rule
autoErase = mustNotBe (oRuleNumber `oEqu` oSelfNumber)

autoErase' :: Rule -> Rule
autoErase' = rAnd autoErase

