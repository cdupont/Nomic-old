{-# LANGUAGE StandaloneDeriving, GADTs, DeriveDataTypeable,
    FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies,
    TypeOperators, FlexibleInstances, NoMonomorphismRestriction #-}

-- | This module implements Game management.
-- a game is a set of rules, and results of actions made by players (usually vote results)
-- the module manages the effects of rules over each others.
module Game where

import Rule
import Control.Monad.State
import Text.Printf
import Data.List
import Utils
import Interpret
import Data.Either
import Action
import Control.Applicative
import Comm
import Control.Arrow
import NamedRule
import Happstack.State
import Data.Typeable
import Safe
import qualified Data.Traversable as T
import Data.Maybe (fromMaybe)
import Expression
import Debug.Trace.Helpers

instance Version PlayerInfo
$(deriveSerialize ''PlayerInfo)

instance Version Game
$(deriveSerialize ''Game)
   
initialGame name = Game { gameName      = name,
                          rules         = initialRuleSet,
                          actionResults = [],
                          players       = [],
                          variables     = [],
                          victory       = Nothing}


-- | Allow to pass around the state of the game while making IO on a specified Handle:
type GameState = StateT Game Comm ()

type GameStateWith a = StateT Game Comm a

-- | An helper function that makes it very clear how to use the state transformer GameState.
runWithGame :: Game -> GameState -> Comm Game
runWithGame = flip execStateT


-- | This function will propose all pending rules for addition to the constitution.               
proposeAllPendings :: GameState
proposeAllPendings = do
    prs <- gets pendingRules
    case prs of
       [] -> say "No pending rules"
       _:_ -> mapM_ proposeRule (sort prs)


-- | This function will proposes a rule for addition to the constitution.
proposeRule :: Rule -> GameState 
proposeRule nr = do
   say $ "\nYour rule: \n" ++ (show nr)
   gs <- get
   say $ "is going to be evaluated against the current legislation: \n\n" ++ (showRS $ activeRules gs) ++ "\n"
   -- try to amend the current ruleset with the new rule.
   amend nr
   newgs <- get    
   if newgs == gs 
       then say "no changes to the legislation. \n"
       else say $ "new legislation: \n" ++ (showRS $ activeRules newgs) ++ "\n"



-- | this function tests whereas a rule is legal according to current legislation.
-- If legal, we return Right Nothing. If illegal, we return Just the number of the rule that illegalised our rule.
-- if there are remaining actions to accomplish, we return them in Left.
isLegal :: Rule -> GameStateWith (Either [Action] (Maybe RuleNumber))
isLegal nr = do
   lega <- legalityList nr
   case concat $ lefts $ map fst lega of
      a:as -> return $ Left (a:as)
      []   -> return $ Right $ lookup (Right False) lega


-- | this function tests a rule against a legislation.
-- it returns the result for every rules of the constitution, along with the rules number.
legalityList :: Rule -> GameStateWith [(Either [Action] Bool, RuleNumber)]
legalityList nr = do
   ar <- gets activeRules
   legals <- mapM (flip executeRule $ Just nr) ar
   return $ zip legals (map rNumber ar)


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

getValue :: Either [Action] Bool -> Bool
getValue e = case e of
   Right b -> b
   Left _ -> True    --TODO: repair. For now pending votes are Trued in execution.                                                   

-- | apply a rule to a set of rules. Return whereas the given rules are legal or not to the former.
applyTo'' :: Rule -> [Rule] -> GameStateWith [(Rule, Either [Action] Bool)]
applyTo'' testing rs = do
   -- test each rule.
   legals <- mapM (executeRule testing . Just) rs
   return $ zip rs legals


-- | Play a rule. It may be added to the current ruleset if legal!
-- it may then modify the ruleset.
amend :: Rule -> GameState
amend nr = do
   say "Checking your rule against the current legislation...\n"
   legal <- isLegal nr
   g <- get
   case legal of
      Right Nothing -> do
         say "Rule legal. It is added to the constitution:\n"
         -- the rule become official: it is added to the rule set
         modifyRuleInGS nr{rStatus = Active}
         get >>= say . showRS . activeRules
         say "Your rule is beeing executed.\n"
         let (RuleFunc ruleFunction) = rRuleFunc nr
         traceM $ "amend 1" ++ (show $ victory g)
         a <- executeRule nr Nothing
         traceM $ "amend 2" ++ (show $ victory g)
         case a of
            Right a -> return ()
            Left b -> say "their are remaining actions left to execute this rule" --TODO

      Right (Just r) -> do
         say $ "Your rule is illegal against rule n#" ++ show r ++ "! It is rejected.\n"
         modifyRuleInGS nr{rStatus = Rejected, rejectedBy = Just r}

      Left as -> do
         sas <- showActions as
         say $ "Their are remaining actions left to complete before beeing able to amend the constitution:\n"
            ++ sas

   

-- | Finds the corresponding game in the multistate and replaces it.
modifyRuleInGS :: Rule -> GameState
modifyRuleInGS nr = do
   rs <- gets rules
   case find (\mynr -> rNumber nr == rNumber mynr) rs of
      Nothing -> error "modifyRule: No rule by that number"
      Just oldnr -> modify (\game -> game { rules = replace oldnr nr rs})



--accessors

activeRules :: Game -> [Rule]
activeRules = sort . filter (\(Rule {rStatus=rs}) -> rs==Active)  .  rules

pendingRules :: Game -> [Rule]
pendingRules = sort . filter (\(Rule {rStatus=rs}) -> rs==Pending)  .  rules

rejectedRules :: Game -> [Rule]
rejectedRules = sort . filter (\(Rule {rStatus=rs}) -> rs==Rejected)  .  rules

suppressedRules :: Game -> [Rule]
suppressedRules = sort . filter (\(Rule {rStatus=rs}) -> rs==Suppressed)  .  rules

allRules :: Game -> [Rule]
allRules = sort . liftM2 (++)  activeRules pendingRules

currentlyProposedRule :: Game -> Maybe Rule
currentlyProposedRule = headMay . pendingRules



-- Actions

-- | This function lists all pending actions to do         
pendingActions :: GameStateWith [Action]
pendingActions = do
   prs <- gets pendingRules
   ars <- gets activeRules
   --fs <- mapM isRuleLegal (sort prs)
   legals <- mapM (\testing -> mapM (\tested -> executeRule testing $ Just tested) (sort prs)) (sort ars)  --TODO refactor
   return $ concat $ lefts $ concat legals


-- | This function lists pending actions of a player
playersPendingActions :: PlayerNumber -> GameStateWith [Action]
playersPendingActions pn = pendingActions >>= filterM (isPlayersaction pn)


-- | This function tells whereas an action is for a player
isPlayersaction :: PlayerNumber -> Action -> GameStateWith Bool
isPlayersaction pn (Action _ (ActionType _ apn _) _) = return $ pn == apn


-- | Show an action
showAction :: Action -> GameStateWith String
showAction (Action testing (ActionType reason pn _) result) = do
   return $ " Action from rule #" ++ (show testing) ++ " for player " ++ (show pn) ++ ": " ++ reason ++ "\n"
            ++ maybe "" (\b -> "Action result: " ++ (show b) ++ "\n") result


-- | Show actions
showActions :: [Action] -> GameStateWith String
showActions as = do
   sas <- mapM showAction as
   case sas of
      _:_ -> return $ concat $ zipWith (\n a -> show n ++  ". " ++ a) [1..] sas
      [] -> return "No actions"



-- Evaluator on rules

-- | This type is usefull for rules evaluation:
-- it allows modifications of a game
-- it allows IO on players handles
-- its return type is either the value calculated, or a list of remaining actions.
type Evaluator a = GameStateWith (Either [Action] a)

-- | Combined lifters for Evaluator
liftE  = liftM  . liftA
liftE2 = liftM2 . liftA2
liftE3 = liftM3 . liftA3


-- | Execute a rule, maybe against another rule.
-- return: the new state of the game, with either the legality of the tested rule (the Bool),
-- or the list of remaining actions if there are left to complete.
executeRule :: Rule -> Maybe Rule -> StateT Game Comm (Either [Action] Bool)
executeRule testing tested = do
   g <- get
   let rn = rNumber testing
   traceM $ "executeRule 1" ++ (show $ victory g)
   let (RuleFunc ruleFunction) = rRuleFunc testing
   let expr = runStateT (ruleFunction tested) g
       expr :: Exp (Bool, Game)
   e <- evalExp expr rn
   case e of
      Left as -> return $ Left as
      Right (b, g') -> do
         put g'
         return $ Right b
   --traceM $ "executeRule 2" ++ (show $ victory g)


--newtype RuleFunc = RuleFunc {ruleFunc :: Maybe Rule -> (StateT Game Exp Bool)} deriving (Typeable)


-- | tests the legality of a rule against another.
-- arguments: testing rule, tested rule, number of the testing rule, game.
-- return: either the legality of the tested rule, or remaining actions.
--isRuleLegal' :: RuleFunc -> Rule -> RuleNumber -> StateT Game Comm (Either [Action] Bool)
--isRuleLegal' (RuleFunc testing) tested _ = do
--   myr <- lift $ readNamedRule nr
--   return undefined -- $ pure $ r == myr

--   RuleFunc = RuleFunc {ruleFunc :: Rule -> (StateT Game Exp Bool)

--isRuleLegal' (RuleFunc o) nr sn  = evalObs o nr sn
--     
--isRuleLegal' (OfficialRule n) nr sn = do
--   morn <- getOfficialRuleNumber n
--   case morn of
--      Just orn -> isRuleLegal' orn nr sn
--      Nothing -> return $ pure True  --TODO check if the number is false, the rule says always yes...
--   
--isRuleLegal' (TestRuleOver r) nr sn = do
--   nr' <- lift $ readNamedRule nr
--   isRuleLegal' nr' (defaultNRWith r) sn

-- defaultNRWith :: RuleFunc -> Rule
-- defaultNRWith r = undefined -- NamedRule {rNumber=0, rName ="", rText="", rProposedBy=0, rRule = show r, rStatus = Pending, rejectedBy = Nothing}

-- | Get an official rule by its number.
getOfficialRuleNumber :: RuleNumber -> GameStateWith (Maybe RuleFunc)
getOfficialRuleNumber n = do
   ar <- gets activeRules
   let mnr = find (\Rule {rNumber=m} -> m == n) ar
   case mnr of
      Just nr -> do
         r <- lift $ readNamedRule nr
         return $ Just r
      Nothing -> return Nothing

-- | query the player to enter his rule.
--enterRule' :: PlayerNumber -> RuleNumber -> Comm Rule
--enterRule' nP num = do
--   putCom $ "Enter the name of your rule:"
--   name <- getCom
--   putCom $ "Enter a description of your rule:"
--   text <- getCom
--   putCom $ "Enter the code of your rule:"
--   rule <- getCom
--   return $ Rule {rNumber = num,
--                       rName = name,
--                       rDescription = text,
--                       rProposedBy = nP,
--                       rRuleCode = rule,
--                       rStatus = Pending,
--                       rejectedBy = Nothing}


-- evaluator on observable

-- | Evaluate an Observable. We pass it the tested rule, its own rule number and the state of the game.
-- it returns either the value of the observable if it can, or the actions necessary to complete to reach this value.
--evalObs' :: Exp a -> RuleNumber -> RuleNumber -> Evaluator a
--evalObs' o tested testing = do
--   g <- get
--   case findNamedRule tested (rules g) of
--      Just nr -> evalExp o nr testing
--      Nothing -> error "evalObs': can't find tested named rule. Shouldn't have happened."


-- | Evaluate an Observable. We pass it the tested rule, its own rule number and the state of the game.
-- it returns either the value of the observable if it can, or the actions necessary to complete to reach this value.
--evalObs :: Exp a -> Rule -> RuleNumber -> Evaluator a
--evalObs ProposedBy (Rule {rProposedBy=proposedBy}) _ = return $ pure proposedBy
--evalObs RuleNumber (Rule {rNumber=rNumber})        _ = return $ pure rNumber
--evalObs Official   (Rule {rNumber=rn})             _ = return . pure . (isOfficial rn) =<< get  
--evalObs SelfNumber _  sn                                  = return $ pure sn
--evalObs AllPlayers _ _                                    = return . pure . map playerNumber =<< gets players
--evalObs (Konst a)  _ _                                    = return $ pure a
--evalObs (Nil)  _ _                                        = return $ pure []
--evalObs (Not a)     nr sn = liftE  not   (evalObs a nr sn)
--evalObs (Plus a b)  nr sn = liftE2 (+)   (evalObs a nr sn) (evalObs b nr sn)
--evalObs (Minus a b) nr sn = liftE2 (-)   (evalObs a nr sn) (evalObs b nr sn) 
--evalObs (Time a b)  nr sn = liftE2 (*)   (evalObs a nr sn) (evalObs b nr sn)
--evalObs (And a b)   nr sn = liftE2 (&&)  (evalObs a nr sn) (evalObs b nr sn)  
--evalObs (Equ a b)   nr sn = liftE2 (==)  (evalObs a nr sn) (evalObs b nr sn)
--evalObs (Cons a b)  nr sn = liftE2 (:)   (evalObs a nr sn) (evalObs b nr sn)
--evalObs (Lt a b)    nr sn = liftE2 (<)   (evalObs a nr sn) (evalObs b nr sn)
--evalObs (If a b c)  nr sn = liftE3 (if3) (evalObs a nr sn) (evalObs b nr sn) (evalObs c nr sn)
--evalObs (Div a b)   nr sn = liftE2 (/)   (evalObs a nr sn) (evalObs b nr sn)
--
---- TODO simplify...
--evalObs (Map f obs) nr sn = liftE (map (eval . f . Konst)) (evalObs obs nr sn)
--   >>= either (return . Left)
--              (sequence >=> return . T.sequenceA)
--   where eval a = evalObs a nr sn
--
--
--evalObs (Foldr f obs lobs) nr sn = liftE2 (\a b -> eval $ foldr f (Konst a) (map Konst b)) (evalObs obs nr sn) (evalObs lobs nr sn)
--   >>= either (return . Left)
--              id
--   where eval a = evalObs a nr sn

evalExp :: Exp a -> RuleNumber -> Evaluator a
evalExp Get _   = do
   g <- get
   return $ pure g

evalExp (Put a) _ = do
   put a
   return $ pure ()

evalExp (Expression.Const a)  _ = return $ pure a

evalExp (Bind exp f) rn = do
   eitherA <- evalExp exp rn
   case eitherA of
      Right a -> evalExp (f a) rn
      Left as -> return $ Left as


--Bind       :: Exp b -> (b -> Exp c) -> Exp c

--TODO: verify if there is no ambiguity in the search for action result
evalExp (InputChoice or opn ocs) rn = do
   g <- get
   eAction <- liftE3 ActionType (evalExp or rn) (evalExp opn rn) (evalExp ocs rn)
   return $ case eAction of
      Right action ->
         case findActionResult action rn (actionResults g) of  --TODO: vérifications d'usage: nb players etc.
            Just r -> Right $ fromMaybe (error "evalObs: Action result should be fulfilled at this stage.") (result r)
            Nothing -> (Left $ [Action rn action Nothing])
      Left a -> Left a


if3 a b c = if a then b else c


-- | search the rule number amount official rules
isOfficial :: RuleNumber -> Game -> Bool
isOfficial rn g = not . null $ filter (\(Rule {rNumber=rNumberOff}) -> rNumberOff == rn) $ activeRules g

           
instance Show Game where
   show (Game name rules ars pls vars vic) = 
      printf "Game name: %s \n%s\n Completed actions: %s\n Players: %s" name (showRS rules) (show ars) (show $ sort pls) (show vars) (show vic)

instance Eq Game where
   (Game name1 _ _ _ _ _) == (Game name2 _ _ _ _ _) = name1 == name2

instance Ord Game where
   compare (Game name1 _ _ _ _ _) (Game name2 _ _ _ _ _) = compare name1 name2
      
instance Show PlayerInfo where
   show (PlayerInfo { playerNumber = pn,
                      playerName = name})  
      = show pn ++ ": " ++ name

instance Ord PlayerInfo where
   h <= g = (playerNumber h) <= (playerNumber g)
