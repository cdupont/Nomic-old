{-# LANGUAGE StandaloneDeriving, GADTs, DeriveDataTypeable,
    FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies,
    TypeOperators, FlexibleInstances, NoMonomorphismRestriction,
    TypeSynonymInstances #-}

-- | This module implements Game management.
-- a game is a set of rules, and results of actions made by players (usually vote results)
-- the module manages the effects of rules over each others.
module Game (GameState, GameStateWith, initialGame, activeRules, runWithGame, pendingRules, rejectedRules) where

import Language.Nomic.Rule
import Control.Monad.State
import Text.Printf
import Data.List
import Utils
import Interpret
import Data.Either
import Control.Applicative
import Control.Arrow
import Happstack.State
import Data.Typeable
import Safe
import qualified Data.Traversable as T
import Data.Maybe (fromMaybe)
import Language.Nomic.Expression
import Language.Nomic.Evaluation
import Debug.Trace.Helpers


-- | the initial rule set for a game.
rApplicationMetaRule = Rule  {
    rNumber       = 1,
    rName         = "application meta-rules", 
    rDescription  = "all active metarules will be automatically used to evaluate a proposed rule",
    rProposedBy   = 0,
    rRuleCode     = "applicationMetaRule",
    rRuleFunc     = applicationMetaRule,
    rStatus       = Active,
    rAssessedBy   = Nothing}

rVoteUnanimity = Rule  {
    rNumber       = 2,
    rName         = "applicationMetaRule", 
    rDescription  = "all active metarules are automatically used to evaluate a proposed rule",
    rProposedBy   = 0,
    rRuleCode     = "vote unanimity",
    rRuleFunc     = vote unanimity,
    rStatus       = Active,
    rAssessedBy   = Nothing}
                   
initialRuleSet :: [Rule]
initialRuleSet = [rApplicationMetaRule, rVoteUnanimity]

emptyGame name date = Game { gameName      = name,
                          rules         = [],
                          players       = [],
                          variables     = [],
                          events        = [],
                          outputs       = [],
                          victory       = [],
                          currentTime   = date}

--initialGame :: Game
initialGame name date = flip execState (emptyGame name date) $ do
    evAddRule rVoteUnanimity
    evActivateRule (rNumber rVoteUnanimity) 0
    evAddRule rApplicationMetaRule
    evActivateRule (rNumber rApplicationMetaRule) 0



-- | Allow to pass around the state of the game while making IO on a specified Handle:
type GameState = StateT Game IO ()

type GameStateWith a = StateT Game IO a

-- | An helper function that makes it very clear how to use the state transformer GameState.
runWithGame :: Game -> GameState -> IO Game
runWithGame = flip execStateT


-- | This function will proposes a rule for addition to the constitution.
--proposeRule :: Rule -> GameState 
--proposeRule nr = do
--   say $ "\nYour rule: \n" ++ (show nr)
--   gs <- get
--   say $ "is going to be evaluated against the current legislation: \n\n" ++ (show $ activeRules gs) ++ "\n"
--   -- try to amend the current ruleset with the new rule.
--   amend nr
--   newgs <- get    
--   if newgs == gs 
--       then say "no changes to the legislation. \n"
--       else say $ "new legislation: \n" ++ (show $ activeRules newgs) ++ "\n"



-- | this function tests whereas a rule is legal according to current legislation.
-- If legal, we return Right Nothing. If illegal, we return Just the number of the rule that illegalised our rule.
-- if there are remaining actions to accomplish, we return them in Left.
--isLegal :: Rule -> GameStateWith (Either [Action] (Maybe RuleNumber))
--isLegal nr = do
--   lega <- legalityList nr
--   case concat $ lefts $ map fst lega of
--      a:as -> return $ Left (a:as)
--      []   -> return $ Right $ lookup (Right False) lega


-- | this function tests a rule against a legislation.
-- it returns the result for every rules of the constitution, along with the rules number.
--legalityList :: Rule -> GameStateWith [(Either [Action] Bool, RuleNumber)]
--legalityList nr = do
--   ar <- gets activeRules
--   legals <- mapM (flip executeRule $ Just nr) ar
--   return $ zip legals (map rNumber ar)


-- | this function apply the rule to the current legislation.
-- this mean applying the rule to each rules in the constitution.
-- some rules from the constitution may be declared illegal, and suppressed.
--applyTo :: Rule -> GameState
--applyTo nr = do
--        ars <- gets activeRules
--        legals <- applyTo' nr ars
--        let rejectedList = map fst $ filter (not.snd) legals
--        case rejectedList of
--            _:_ -> say $ "The new rule declares the following rules as illegal: " ++ (show $ map rNumber rejectedList)
--                     ++ ". They are withdrawn from the constitution.\n"
--            [] -> say "The new rule doesn't change the constitution.\n"
--        let rejectedList' = map (\mynr -> mynr{rStatus = Suppressed, rejectedBy = Just $ rNumber nr}) rejectedList
--        mapM_ modifyRule rejectedList'
--
--
--applyTo' :: Rule -> [Rule] -> GameStateWith [(Rule, Bool)]
--applyTo' testing rs = do
--   legals <- applyTo'' testing rs
--   return $ map (second getValue) legals

--getValue :: Either [Action] Bool -> Bool
--getValue e = case e of
--   Right b -> b
--   Left _ -> True    --TODO: repair. For now pending votes are Trued in execution.                                                   

-- | apply a rule to a set of rules. Return whereas the given rules are legal or not to the former.
--applyTo'' :: Rule -> [Rule] -> GameStateWith [(Rule, Either [Action] Bool)]
--applyTo'' testing rs = do
--   -- test each rule.
--   legals <- mapM (executeRule testing . Just) rs
--   return $ zip rs legals


-- | Play a rule. It may be added to the current ruleset if legal!
-- it may then modify the ruleset.
--amend :: Rule -> GameState
--amend nr = do
--   say "Checking your rule against the current legislation...\n"
--   g <- get
--   traceM $ "output1" ++ (show $ outputs g)
--   legal <- lift $ evalStateT (isLegal nr) g
--   g <- get
--   traceM $ "output2 " ++ (show $ outputs g)
--   g <- get
--   case legal of
--      Right Nothing -> do
--         say "Rule legal. It is added to the constitution:\n"
--         -- the rule become official: it is added to the rule set
--         modifyRuleInGS nr{rStatus = Active}
--         get >>= say . showRS . activeRules
--         say "Your rule is beeing executed.\n"
--         let (RuleFunc ruleFunction) = rRuleFunc nr
--         a <- executeRule nr Nothing
--         case a of
--            Right _ -> return ()
--            Left _ -> say "their are remaining actions left to execute this rule"
--
--      Right (Just r) -> do
--         say $ "Your rule is illegal against rule n#" ++ show r ++ "! It is rejected.\n"
--         modifyRuleInGS nr{rStatus = Rejected, rejectedBy = Just r}
--
--      Left as -> do
--         sas <- showActions as
--         say $ "Their are remaining actions left to complete before beeing able to amend the constitution:\n"
--            ++ sas

   

-- | Finds the corresponding game in the multistate and replaces it.
--modifyRuleInGS :: Rule -> GameState
--modifyRuleInGS nr = do
--   rs <- gets rules
--   case find (\mynr -> rNumber nr == rNumber mynr) rs of
--      Nothing -> error "modifyRule: No rule by that number"
--      Just oldnr -> modify (\game -> game { rules = replace oldnr nr rs})



--accessors

activeRules :: Game -> [Rule]
activeRules = sort . filter (\(Rule {rStatus=rs}) -> rs==Active) . rules

pendingRules :: Game -> [Rule]
pendingRules = sort . filter (\(Rule {rStatus=rs}) -> rs==Pending) . rules

rejectedRules :: Game -> [Rule]
rejectedRules = sort . filter (\(Rule {rStatus=rs}) -> rs==Reject) . rules

allRules :: Game -> [Rule]
allRules = sort . liftM2 (++)  activeRules pendingRules

currentlyProposedRule :: Game -> Maybe Rule
currentlyProposedRule = headMay . pendingRules



-- Actions

-- | This function lists all pending actions to do by evaluating the rule list         
--pendingActions :: GameStateWith [Action]
--pendingActions = do
--   prs <- gets pendingRules
--   ars <- gets activeRules
--   legals <- mapM (\testing -> mapM (\tested -> executeRule testing $ Just tested) (sort prs)) (sort ars)  --TODO refactor
--   return $ concat $ lefts $ concat legals


-- | This function lists pending actions of a player
--playersPendingActions :: PlayerNumber -> GameStateWith [Action]
--playersPendingActions pn = pendingActions >>= filterM (isPlayersaction pn)



-- | Execute a rule, maybe against another rule.
-- return: the new state of the game, with either the legality of the tested rule (the Bool),
-- or the list of remaining actions if there are left to complete.
--executeRule :: Rule -> Maybe Rule -> StateT Game Comm (Either [Action] Bool)
--executeRule testing tested = do
--    let rf = ruleFunc $ rRuleFunc testing
--    evalExp (rf tested) (rNumber testing)


-- defaultNRWith :: RuleFunc -> Rule
-- defaultNRWith r = undefined -- NamedRule {rNumber=0, rName ="", rText="", rProposedBy=0, rRule = show r, rStatus = Pending, rejectedBy = Nothing}

-- | Get an official rule by its number.
--getOfficialRuleNumber :: RuleNumber -> GameStateWith (Maybe RuleFunc)
--getOfficialRuleNumber n = do
--   ar <- gets activeRules
--   let mnr = find (\Rule {rNumber=m} -> m == n) ar
--   case mnr of
--      Just nr -> do
--         r <- lift $ readNamedRule nr
--         return $ Just r
--      Nothing -> return Nothing


if3 a b c = if a then b else c


-- | search the rule number amount official rules
isOfficial :: RuleNumber -> Game -> Bool
isOfficial rn g = not . null $ filter (\(Rule {rNumber=rNumberOff}) -> rNumberOff == rn) $ activeRules g
           

instance Eq Game where
   (Game name1 _ _ _ _ _ _ _) == (Game name2 _ _ _ _ _ _ _) = name1 == name2

instance Ord Game where
   compare (Game name1 _ _ _ _ _ _ _) (Game name2 _ _ _ _ _ _ _) = compare name1 name2
      

instance Ord PlayerInfo where
   h <= g = (playerNumber h) <= (playerNumber g)
