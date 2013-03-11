{-# LANGUAGE DeriveDataTypeable, GADTs, ScopedTypeVariables, TupleSections, TemplateHaskell#-}

-- | Basic rules examples.
module Language.Nomyx.Rule where

import Language.Nomyx.Expression
import Language.Nomyx.Definition
import Data.Typeable
import Control.Monad.State hiding (forM_)
import Data.Maybe
import Data.Time hiding (getCurrentTime)
import Control.Arrow
import Control.Applicative
import Data.Foldable hiding (and, mapM_)

-- | This rule will activate automatically any new rule.
autoActivate :: RuleFunc
autoActivate = VoidRule $ onEvent_ (RuleEv Proposed) (activateRule_ . rNumber . ruleData)

-- | This rule will forbid any new rule to delete the rule in parameter
immutableRule :: RuleNumber -> RuleFunc
immutableRule rn = RuleRule f where
   f r = do
      protectedRule <- getRule rn
      case protectedRule of
         Just pr -> case rRuleFunc r of
            RuleRule paramRule -> paramRule pr
            _ -> return $ BoolResp True
         Nothing -> return $ BoolResp True

-- | A rule will be always legal
legal :: RuleFunc
legal = RuleRule $ \_ -> return $ BoolResp True

-- | A rule will be always illegal
illegal :: RuleFunc
illegal = RuleRule $ \_ -> return $ BoolResp False

-- | This rule establishes a list of criteria rules that will be used to test any incoming rule
-- the rules applyed shall give the answer immediatly
simpleApplicationRule :: RuleFunc
simpleApplicationRule = VoidRule $ do
    v <- newVar_ "rules" ([]:: [RuleNumber])
    onEvent_ (RuleEv Proposed) (h v) where
        h v (RuleData rule) = do
            (rns:: [RuleNumber]) <- readVar_ v
            rs <- getRulesByNumbers rns
            oks <- mapM (applyRule rule) rs
            when (and oks) $ activateRule_ $ rNumber rule

applyRule :: Rule -> Rule -> Exp Bool
applyRule (Rule {rRuleFunc = rf}) r = do
    case rf of
        RuleRule f1 -> f1 r >>= return . boolResp
        _ -> return False

        
-- | active metarules are automatically used to evaluate a given rule
checkWithMetarules :: Rule -> Exp RuleResponse
checkWithMetarules r = do
    rs <- getActiveRules
    let rrs = mapMaybe maybeMetaRule rs
    evals <- mapM (\rr -> rr r) rrs
    andrrs evals


maybeMetaRule :: Rule -> Maybe (OneParamRule Rule)
maybeMetaRule Rule {rRuleFunc = (RuleRule r)} = Just r
maybeMetaRule _ = Nothing


-- | any new rule will be activate if the rule in parameter returns True
onRuleProposed :: (Rule -> Exp RuleResponse) -> RuleFunc
onRuleProposed r = VoidRule $ onEvent_ (RuleEv Proposed) $ \(RuleData rule) -> do
    resp <- r rule
    case resp of
        BoolResp b -> activateOrReject rule b
        MsgResp m -> onMessageOnce m $ (activateOrReject rule) . messageData



data ForAgainst = For | Against deriving (Typeable, Enum, Show, Eq, Bounded, Read)
type Votes = [(PlayerNumber, Maybe ForAgainst)]
type AssessFunction = Votes -> Maybe Bool
data VoteData = VoteData { msgEnd :: Event (Message Bool),
                              inputEvents :: [EventNumber],
                              voteVar :: ArrayVar PlayerNumber ForAgainst,
                              assessFunction :: AssessFunction}
type Assessor a = StateT VoteData Exp a

-- | Performs a vote for a rule on all players. The provided function is used to count the votes.
-- the assessors allows to configure how and when the vote will be assessed. The assessors can be chained.
voteWith :: (Votes -> Maybe Bool) -> Assessor () -> Rule -> Exp RuleResponse 
voteWith assessFunction assessors rule = do
    pns <- getAllPlayerNumbers
    let rn = show $ rNumber rule
    let resultMsg = Message ("Result of votes for " ++ rn) :: Event(Message Bool)
--    --create an array variable to store the votes. The passed function will be called upon each vote.
    (voteVar :: ArrayVar PlayerNumber ForAgainst) <- newArrayVar ("Votes for rule " ++ rn) pns
    let askPlayer pn = onInputChoiceOnce ("Vote for rule " ++ rn) [For, Against] (putArrayVar voteVar pn) pn
    ics <- mapM askPlayer pns
    let voteData = VoteData resultMsg ics voteVar assessFunction
    evalStateT assessors voteData
    cleanVote voteData
    return $ MsgResp resultMsg

-- assess the vote on every new vote with the assess function, and as soon as the vote has an issue (positive of negative), sends a signal
assessOnEveryVotes :: Assessor ()
assessOnEveryVotes = do
   (VoteData msgEnd _ voteVar assess) <- get
   lift $ do
      msgVotes <- getArrayVarMessage voteVar
      onMessage msgVotes $ \(MessageData votes) -> maybeWhen (assess votes) $ sendMessage msgEnd

-- assess the vote with the assess function when time is elapsed, and sends a signal with the issue (positive of negative)
assessOnTimeLimit ::  NominalDiffTime -> Assessor ()
assessOnTimeLimit delay = do
   (VoteData msgEnd _ voteVar assess) <- get
   lift $ do
      t <- addUTCTime delay <$> getCurrentTime
      onEvent_ (Time t) $ \_ -> do
         votes <- getArrayVarData voteVar
         sendMessage msgEnd $ fromMaybe False $ assessOnlyVoters assess $ votes

-- assess the vote only when every body voted
assessOnVoteComplete :: Assessor ()
assessOnVoteComplete = do
   (VoteData msgEnd _ voteVar assess) <- get
   lift $ do
      msgVotes <- getArrayVarMessage voteVar
      onMessage msgVotes $ \(MessageData votes) -> when (length (voters votes) == length votes) $
         sendMessage msgEnd $ fromMaybe False $ assessOnlyVoters assess $ votes

-- clean events and variables necessary for the vote
cleanVote :: VoteData -> Exp ()
cleanVote (VoteData msgEnd inputEvents voteVar _) = onMessage msgEnd$ \_ -> do
   delAllEvents msgEnd
   mapM_ delEvent_ inputEvents
   delArrayVar voteVar

assessOnlyVoters :: AssessFunction -> AssessFunction
assessOnlyVoters assess vs = assess $ map (second Just) $ voters vs
--assess $ map (second Just) $ voters votes

-- a quorum is the neccessary number of voters for the validity of the vote
quorum :: Int -> Votes -> Bool
quorum q vs = (length $ voters vs) >= q

-- adds a quorum to an assessing function
withQuorum :: AssessFunction -> Int -> AssessFunction
withQuorum assess q vs = if (quorum q vs) then assess vs else Nothing

-- | assess the vote results according to a unanimity (everybody votes for)
unanimity :: Votes -> Maybe Bool
unanimity votes = voteQuota (length votes) votes
  
-- | assess the vote results according to an absolute majority (half voters plus one, no quorum is needed)
majority :: Votes -> Maybe Bool
majority votes = voteQuota ((length votes) `div` 2 + 1) votes

-- | assess the vote results according to a majority of x (in %)
majorityWith :: Int -> Votes -> Maybe Bool
majorityWith x votes = voteQuota ((length votes) * x `div` 100 + 1) votes

-- | assess the vote results according to a necessary number of positive votes
numberPositiveVotes :: Int -> Votes -> Maybe Bool
numberPositiveVotes = voteQuota

-- | helper function for assessement functions
voteQuota :: Int -> Votes -> Maybe Bool
voteQuota quotaFor votes
   | nbFor votes >= quotaFor = Just True
   | nbAgainst votes > (length votes) - quotaFor = Just False
   | otherwise = Nothing
   
nbFor, nbAgainst :: Votes -> Int
nbFor = length . filter ((== Just For) . snd)
nbAgainst = length . filter ((== Just Against) . snd)
      
voters :: [(PlayerNumber, Maybe ForAgainst)] -> [(PlayerNumber, ForAgainst)]
voters vs = catMaybes $ map voter vs where
    voter (pn, Just fa) = Just (pn, fa)
    voter (_, Nothing) = Nothing

activateOrReject :: Rule -> Bool -> Exp ()
activateOrReject r b = if b then activateRule_ (rNumber r) else rejectRule_ (rNumber r)

-- | rule that performs a vote for a rule on all players. The provided function is used to count the votes,
--it will be called when every players has voted or when the time limit is reached
--voteWithTimeLimit :: ([(PlayerNumber, Maybe ForAgainst)] -> Bool) -> UTCTime -> Rule -> Exp RuleResponse
--voteWithTimeLimit assessVote t r = do
--    (finalDecisionEvent, inputs, voteVar) <- voteWith assessVote r
--    --time limit
--    onEventOnce_ (Time t) $ \_ -> do
--        getArrayVarData voteVar >>= sendMessage finalDecisionEvent . assessVote
--        delArrayVar voteVar
--        mapM_ delEvent inputs
--    return $ MsgResp finalDecisionEvent

-- | perform an action for each current players, new players and leaving players
forEachPlayer :: (PlayerNumber -> Exp ()) -> (PlayerNumber -> Exp ()) -> (PlayerNumber -> Exp ()) -> Exp ()
forEachPlayer action actionWhenArrive actionWhenLeave = do
    pns <- getAllPlayerNumbers
    mapM_ action pns
    onEvent_ (Player Arrive) $ \(PlayerData p) -> actionWhenArrive $ playerNumber p
    onEvent_ (Player Leave) $ \(PlayerData p) -> actionWhenLeave $ playerNumber p

-- | perform the same action for each players, including new players
forEachPlayer_ :: (PlayerNumber -> Exp ()) -> Exp ()
forEachPlayer_ action = forEachPlayer action action (\_ -> return ())

forEachPlayer' :: (PlayerNumber -> Exp a) -> ((PlayerNumber, a) -> Exp ()) -> Exp ()
forEachPlayer' = undefined

-- | create a value initialized for each players
--manages players joining and leaving
createValueForEachPlayer :: Int -> V [(Int, Int)] -> Exp ()
createValueForEachPlayer initialValue var = do
    pns <- getAllPlayerNumbers
    v <- newVar_ (varName var) $ map (,initialValue::Int) pns
    forEachPlayer (\_-> return ())
                  (\p -> modifyVar v ((p, initialValue):))
                  (\p -> modifyVar v $ filter $ (/= p) . fst)

-- | create a value initialized for each players initialized to zero
--manages players joining and leaving
createValueForEachPlayer_ :: V [(Int, Int)] -> Exp ()
createValueForEachPlayer_ = createValueForEachPlayer 0

modifyValueOfPlayer :: PlayerNumber -> V [(Int, Int)] -> (Int -> Int) -> Exp ()
modifyValueOfPlayer pn var f = modifyVar var $ map $ (\(a,b) -> if a == pn then (a, f b) else (a,b))

modifyAllValues :: V [(Int, Int)] -> (Int -> Int) -> Exp ()
modifyAllValues var f = modifyVar var $ map $ second f

-- | Player p cannot propose anymore rules
noPlayPlayer :: PlayerNumber -> RuleFunc
noPlayPlayer p = RuleRule $ \r -> return $ BoolResp $ (rProposedBy r) /= p

-- | a rule can autodelete itself (generaly after having performed some actions)
autoDelete :: Exp ()
autoDelete = getSelfRuleNumber >>= suppressRule_


-- | All rules from player p are erased:
eraseAllRules :: PlayerNumber -> Exp Bool
eraseAllRules p = do
    rs <- getRules
    let myrs = filter (\r ->  (rProposedBy r) == p) rs
    res <- mapM (suppressRule . rNumber) myrs
    return $ and res

oneDay :: NominalDiffTime
oneDay = 60 * 60 * 24

maybeWhen :: Maybe a -> (a -> Exp ()) -> Exp ()
maybeWhen = forM_
