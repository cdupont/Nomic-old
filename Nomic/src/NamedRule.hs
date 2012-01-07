{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators #-}

-- | This module defines a NamedRule, an informational structure over a Rule.
module NamedRule where

import Rule
import Text.Printf
import Data.List
import Happstack.State
import Expression
import Data.Binary.Get

type RuleName = String
type RuleText = String
type RuleCode = String


--defaultNR = NamedRule { rNumber = 1,
--                        rName = "",
--                        rText = "",
--                        rProposedBy = 0, 
--                        rRule = defaultRule,
--                        rStatus = Active,
--                        rejectedBy = Nothing}
--
--defaultNRStatus s = NamedRule { rNumber = 1,
--                                rName = "",
--                                rText = "",
--                                rProposedBy = 0, 
--                                rRule = defaultRule,
--                                rStatus = s,
--                                rejectedBy = Nothing}
--
--defaultRS = [defaultNR]
--defaultRSWithPropRule = (defaultNRStatus Pending):defaultRS

instance Version Rule
instance Serialize Rule where
           getCopy = contain $ getc
           putCopy (Rule rn n d p r _ s rb) = contain $ (safePut r >> safePut n >> safePut d >> safePut p >> safePut r >> safePut s >> safePut rb)

getc :: Data.Binary.Get.Get Rule
getc = do
   rn <- safeGet
   n <- safeGet
   d <- safeGet
   p <- safeGet
   rc <- safeGet
   let rf = RuleFunc (\_ -> return True)
   s <- safeGet
   rb <- safeGet
   return $ Rule { rNumber       = rn,
                   rName         = n, 
                   rDescription  = d,
                   rProposedBy   = p,
                   rRuleCode     = rc,
                   rRuleFunc     = rf,
                   rStatus       = s,
                   rejectedBy    = rb}




--instance Version RuleFunc
--instance Serialize RuleFunc where
--          getCopy = contain $ getc
--          putCopy (ActionType r pn cs) = contain $ (safePut r >>> safePut pn >> safe


-- | show a rule set.
showRS :: [Rule] -> String
showRS = concat . intersperse "\n" . map show . sort

-- | find a rule in the rule set.
findNamedRule :: RuleNumber -> [Rule] -> Maybe Rule
findNamedRule rn = find (\Rule { rNumber = myrn} -> myrn == rn )


-- Initial rules


-- | the initial rule set for a game.
initialRuleSet :: [Rule]
initialRuleSet = [nrVote, nrImmutable]
  
-- | initial rule #1 that states that rules must be voted unanimously
nrVote = Rule {rNumber=1, rName ="Vote", rDescription="Unanimous vote", rProposedBy=0, rRuleCode = "allVoteRule", rRuleFunc = unanimityVote, rStatus = Active, rejectedBy = Nothing}

-- | initial rule #2 that states that rules must not erase rules #1.
nrImmutable = Rule {rNumber=2, rName ="Immutable Rules", rDescription="The rule #1 must not be suppressed", rProposedBy=0, rRuleCode = "immutable 1", rRuleFunc = immutableRule 1, rStatus = Active, rejectedBy = Nothing}



	
-- instances

instance Ord Rule where
   (Rule {rNumber=n}) <= (Rule {rNumber=m}) = n <= m

-- rules are equal if everything is equal, code as a string included. The function of the rule is not comparable (field rRuleFunc)
instance Eq Rule where
   (Rule rn n d p r _ s rb) == (Rule rn' n' d' p' r' _ s' rb') = (rn, n, d, p, r, s, rb) == (rn', n', d', p', r', s', rb')   

instance Show Rule where
   show (Rule rNumber rName rText rProposedBy rule _ rStatus rejectedBy) =
        printf "%d. %s    Proposed by player %d   Status: %s %s \n %s\n %s\n"
               rNumber rName rProposedBy (show rStatus) (maybe "" (printf "by rule %d") rejectedBy) rText (show rule)
        



