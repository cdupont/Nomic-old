
-- | Basic rules examples.
module Language.Nomyx.Rules (
   RuleFunc,
   RuleResp(..),
   Rule(..),
   RuleNumber,
   RuleCode,
   RuleEvent(..),
   RuleStatus(..),
   ruleFunc,
   activateRule, activateRule_,
   rejectRule, rejectRule_,
   getRules, getActiveRules, getRule,
   getRulesByNumbers,
   getRuleFuncs,
   addRule, addRule_, addRuleParams,
   getFreeRuleNumber,
   suppressRule, suppressRule_, suppressAllRules,
   modifyRule,
   autoActivate,
   activateOrReject,
   noPlayPlayer,
   autoDelete,
   eraseAllRules,
   getSelfRuleNumber, getSelfRule
   ) where

import Prelude hiding (foldr)
import Language.Nomyx.Expression
import Language.Nomyx.Events
import Data.Lens
import Control.Monad
import Data.List
import Data.Maybe

-- * Rule management

ruleFunc :: Nomex a -> RuleFunc
ruleFunc e = e >> return Void

-- | activate a rule: change its state to Active and execute it
activateRule :: RuleNumber -> Nomex Bool
activateRule = ActivateRule

activateRule_ :: RuleNumber -> Nomex ()
activateRule_ r = activateRule r >> return ()

-- | reject a rule: change its state to Suppressed and suppresses all its environment (events, variables, inputs)
-- the rule can be activated again later
rejectRule :: RuleNumber -> Nomex Bool
rejectRule = RejectRule

rejectRule_ :: RuleNumber -> Nomex ()
rejectRule_ r = void $ rejectRule r

getRules :: Nomex [Rule]
getRules = GetRules

getActiveRules :: Nomex [Rule]
getActiveRules = return . (filter ((== Active) . _rStatus) ) =<< getRules

getRule :: RuleNumber -> Nomex (Maybe Rule)
getRule rn = do
   rs <- GetRules
   return $ find ((== rn) . getL rNumber) rs

getRulesByNumbers :: [RuleNumber] -> Nomex [Rule]
getRulesByNumbers rns = mapMaybeM getRule rns

getRuleFuncs :: Nomex [RuleFunc]
getRuleFuncs = return . (map _rRuleFunc) =<< getRules

-- | add a rule to the game, it will have to be activated
addRule :: Rule -> Nomex Bool
addRule r = AddRule r

addRule_ :: Rule -> Nomex ()
addRule_ r = void $ AddRule r

--TODO: too permissive. Should use SubmitRule instead.
addRuleParams :: RuleName -> RuleFunc -> RuleCode -> String -> Nomex RuleNumber
addRuleParams name func code desc = do
   number <- getFreeRuleNumber
   res <- addRule $ defaultRule {_rName = name, _rRuleFunc = func, _rRuleCode = code, _rNumber = number, _rDescription = desc}
   return $ if res then number else error "addRuleParams: cannot add rule"


getFreeRuleNumber :: Nomex RuleNumber
getFreeRuleNumber = do
   rs <- getRules
   return $ getFreeNumber $ map _rNumber rs

getFreeNumber :: (Eq a, Num a, Enum a) => [a] -> a
getFreeNumber l = head [a| a <- [1..], not $ a `elem` l]

--suppresses completly a rule and its environment from the system
suppressRule :: RuleNumber -> Nomex Bool
suppressRule rn = RejectRule rn

suppressRule_ :: RuleNumber -> Nomex ()
suppressRule_ rn = void $ RejectRule rn

suppressAllRules :: Nomex Bool
suppressAllRules = do
    rs <- getRules
    res <- mapM (suppressRule . _rNumber) rs
    return $ and res

modifyRule :: RuleNumber -> Rule -> Nomex Bool
modifyRule rn r = ModifyRule rn r


-- | This rule will activate automatically any new rule.
autoActivate :: RuleFunc
autoActivate = ruleFunc $ onEvent_ (RuleEv Proposed) (activateRule_ . _rNumber . ruleData)

-- | This rule will forbid any new rule to delete the rule in parameter
--immutableRule :: RuleNumber -> RuleFunc
--immutableRule rn = return $ Meta f where
--   f r = do
--      protectedRule <- getRule rn
--      case protectedRule of
--         Just pr -> case _rRuleFunc r of
--            RuleRule paramRule -> paramRule pr
--            _ -> return $ BoolResp True
--         Nothing -> return $ BoolResp True

-- | A rule will be always legal
legal :: RuleFunc
legal =  return $ Meta (\_ -> return $ BoolResp True)

-- | A rule will be always illegal
illegal :: RuleFunc
illegal = return $ Meta (\_ -> return $ BoolResp False)

-- | active metarules are automatically used to evaluate a given rule
--checkWithMetarules :: Rule -> Nomex (Event (Message ForAgainst)
--checkWithMetarules rule = do
--    rs <- getActiveRules
--    (metas :: [Rule -> Nomex BoolResp]) <- mapMaybeM maybeMetaRule rs
--    let (evals :: [Nomex BoolResp]) = map (\meta -> meta rule) metas
--    foldr (&&*) true evals


maybeMetaRule :: Rule -> Nomex (Maybe (Rule -> Nomex BoolResp))
maybeMetaRule Rule {_rRuleFunc = rule} = do
   meta <- rule
   case meta of
      (Meta m) -> return $ Just m
      _ -> return Nothing


-- | activate or reject a rule
activateOrReject :: Rule -> Bool -> Nomex ()
activateOrReject r b = if b then activateRule_ (_rNumber r) else rejectRule_ (_rNumber r)


-- | Player p cannot propose anymore rules
noPlayPlayer :: PlayerNumber -> RuleFunc
noPlayPlayer p = return $ Meta $ \r -> return $ BoolResp $ (_rProposedBy r) /= p

-- | a rule can autodelete itself (generaly after having performed some actions)
autoDelete :: Nomex ()
autoDelete = getSelfRuleNumber >>= suppressRule_

-- | All rules from player p are erased:
eraseAllRules :: PlayerNumber -> Nomex Bool
eraseAllRules p = do
    rs <- getRules
    let myrs = filter ((== p) . getL rProposedBy) rs
    res <- mapM (suppressRule . _rNumber) myrs
    return $ and res


-- | allows a rule to retrieve its own number (for auto-deleting for example)
getSelfRuleNumber :: Nomex RuleNumber
getSelfRuleNumber = SelfRuleNumber

getSelfRule :: Nomex Rule
getSelfRule  = do
   srn <- getSelfRuleNumber
   rs:[] <- getRulesByNumbers [srn]
   return rs

-- | a default rule
defaultRule = Rule  {
    _rNumber       = 1,
    _rName         = "",
    _rDescription  = "",
    _rProposedBy   = 0,
    _rRuleCode     = "",
    _rRuleFunc     = return Void,
    _rStatus       = Pending,
    _rAssessedBy   = Nothing}

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f
    
