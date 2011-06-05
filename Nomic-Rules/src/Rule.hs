
{-# LANGUAGE DeriveDataTypeable#-}

-- | This module defines a Rule, which is a structure that allow the player to define if an input Rule is legal or not.
-- That means, a Rule defines if a Rule is legal or not. 
module Rule where

import Observable
import Data.Typeable



-- | the Rule type allows to describe laws over Rules themselves.
-- Rule :: Rule -> (Legal | Illegal)  
data Rule = Rule (Obs Bool)            -- contruct a Rule
          | MustBeEgalTo Rule          -- a (tested) rule must be equal to the rule in parameter
          | TestRuleOver Rule          -- a rule must declare the rule in parameter as legal
          | OfficialRule Int           -- a rule must be legal to the official rule #N
          deriving (Typeable, Show, Eq) 

defaultRule = "legal"

-- | A rule will be legal if the observable is True
rule :: Obs Bool -> Rule
rule = Rule

-- | A rule will be always legal
legal :: Rule
legal = rule true

-- | A rule will be always illegal
illegal :: Rule
illegal = rule false

--  Rule samples:

-- | Vote for something
voteFor :: String -> PlayerNumber -> Rule
voteFor s n = rule (oVoteReason (Konst s) (Konst n))

-- | Vote of one personne. (example #14)
voteRule :: PlayerNumber -> Rule
voteRule = voteFor "Please vote"

-- | Unanimous vote (example #4)
allVoteRule :: Rule
allVoteRule = rule oUnanimityVote


-- | Rule egal to official rule #n:
officialRule :: Int -> Rule
officialRule = OfficialRule

-- | Do not modify rule #n: (example #18)
immutable :: Int -> Rule
immutable = TestRuleOver . OfficialRule


-- | Suppress rule n: (example #2)
eraseRule :: Int -> Rule
eraseRule = rule . erase . konst

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
autoEraseRule :: Rule
autoEraseRule = rule autoErase


