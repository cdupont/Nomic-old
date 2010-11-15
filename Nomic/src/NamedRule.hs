{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators #-}

-- | This module defines a NamedRule, an informational structure over a Rule.
module NamedRule where

import Rule
import Text.Printf
import Data.List
import Observable
import Happstack.State
import Data.Typeable


-- | An informationnal structure about a rule:
data NamedRule = NamedRule { rNumber :: RuleNumber,          -- number of the rule (must be unique) TO CHECK
                             rName :: String,                -- short name of the rule 
                             rText :: String,                -- descrition of the rule
                             rProposedBy :: PlayerNumber,    -- player proposing the rule
                             rule :: String,                 -- code of the rule
                             rStatus :: RuleStatus,          -- status of the rule
                             rejectedBy :: Maybe RuleNumber} -- who rejected this rule
                             deriving (Eq, Typeable)

-- | the status of a rule.
data RuleStatus = Active      -- The current Constitution
                | Pending     -- Proposed rules
                | Rejected    -- Proposed and rejected rules
                | Suppressed  -- Once Active but suppressed rules
                deriving (Eq, Show, Typeable)



defaultNR = NamedRule { rNumber = 1,
                        rName = "",
                        rText = "",
                        rProposedBy = 0, 
                        rule = defaultRule,
                        rStatus = Active,
                        rejectedBy = Nothing}

defaultNRStatus s = NamedRule { rNumber = 1,
                                rName = "",
                                rText = "",
                                rProposedBy = 0, 
                                rule = defaultRule,
                                rStatus = s,
                                rejectedBy = Nothing}

defaultRS = [defaultNR]
defaultRSWithPropRule = (defaultNRStatus Pending):defaultRS

instance Version NamedRule
$(deriveSerialize ''NamedRule)


instance Version RuleStatus
$(deriveSerialize ''RuleStatus)

-- | show a rule set.
showRS :: [NamedRule] -> String
showRS a = (concat $ intersperse "\n" (map show (sort a)))

-- | find a rule in the rule set.
findNamedRule :: RuleNumber -> [NamedRule] -> Maybe NamedRule
findNamedRule rn rs = find (\NamedRule { rNumber = myrn} -> myrn == rn ) rs


-- Initial rules


-- | the initial rule set for a game.
initialRuleSet :: [NamedRule]
initialRuleSet = [nrVote, nrImmutable]
  
-- | initial rule #1 that states that rules must be voted unanimously
nrVote = NamedRule {rNumber=1, rName ="Vote", rText="Unanimous vote", rProposedBy=0, rule = "allVoteRule", rStatus = Active, rejectedBy = Nothing}

-- | initial rule #2 that states that rules must not erase rules #1.
nrImmutable = NamedRule {rNumber=2, rName ="Immutable Rules", rText="The rule #1 must not be suppressed", rProposedBy=0, rule = "immutable 1", rStatus = Active, rejectedBy = Nothing}



	
-- instances

instance Ord NamedRule where
   (NamedRule {rNumber=n}) <= (NamedRule {rNumber=m}) = n <= m

instance Show NamedRule where
   show (NamedRule rNumber rName rText rProposedBy rule rStatus rejectedBy) =
        printf "%d. %s    Proposed by player %d   Status: %s %s \n %s\n %s\n"
               rNumber rName rProposedBy (show rStatus) (maybe "" (printf "by rule %d") rejectedBy) rText (show rule)
        



