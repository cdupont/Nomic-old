{-# LANGUAGE DeriveDataTypeable, GADTs, ScopedTypeVariables, TupleSections, FlexibleInstances#-}

-- | Basic rules examples.
module Language.Nomyx.Rule where

import Prelude hiding (foldr)
import Language.Nomyx.Expression
import Language.Nomyx.Definition
import Data.Typeable
import Control.Monad.State hiding (forM_)
import Data.Maybe
import Data.Time hiding (getCurrentTime)
import Control.Arrow
import Control.Applicative
import Data.Lens
import Data.Foldable hiding (and, mapM_)
import Data.Boolean

-- | This rule will activate automatically any new rule.
autoActivate :: RuleFunc
autoActivate = voidRule $ onEvent_ (RuleEv Proposed) (activateRule_ . _rNumber . ruleData)

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
checkWithMetarules :: Rule -> Nomex BoolResp
checkWithMetarules rule = do
    rs <- getActiveRules
    (metas :: [Rule -> Nomex BoolResp]) <- mapMaybeM maybeMetaRule rs
    let (evals :: [Nomex BoolResp]) = map (\meta -> meta rule) metas
    foldr (&&*) true evals


maybeMetaRule :: Rule -> Nomex (Maybe (Rule -> Nomex BoolResp))
maybeMetaRule Rule {_rRuleFunc = rule} = do
   meta <- rule
   case meta of
      (Meta m) -> return $ Just m
      _ -> return Nothing


-- | any new rule will be activate if the rule in parameter returns True
onRuleProposed :: (Rule -> Nomex BoolResp) -> RuleFunc
onRuleProposed r = voidRule $ onEvent_ (RuleEv Proposed) $ \(RuleData rule) -> do
    resp <- r rule
    case resp of
        BoolResp b -> activateOrReject rule b
        MsgResp m -> onMessageOnce m $ (activateOrReject rule) . messageData



data ForAgainst = For | Against deriving (Typeable, Enum, Show, Eq, Bounded, Read)
type Vote = (PlayerNumber, Maybe ForAgainst)
type AssessFunction = [Vote] -> Maybe Bool
data VoteData = VoteData { msgEnd :: Event (Message Bool),
                           voteVar :: ArrayVar PlayerNumber ForAgainst,
                           inputNumbers :: [EventNumber],
                           assessFunction :: AssessFunction}
type Assessor a = StateT VoteData Nomex a

-- | Performs a vote for a rule on all players. The provided function is used to count the votes.
-- the assessors allows to configure how and when the vote will be assessed. The assessors can be chained.
voteWith :: ([Vote] -> Maybe Bool) -> Assessor () -> Rule -> Nomex BoolResp 
voteWith assessFunction assessors rule = do
    pns <- getAllPlayerNumbers
    let rn = show $ _rNumber rule
    let msgEnd = Message ("Result of votes for " ++ rn) :: Event(Message Bool)
    --create an array variable to store the votes.
    voteVar <- newArrayVar ("Votes for rule " ++ rn) pns
    let askPlayer pn = onInputChoiceOnce ("Vote for rule " ++ rn) [For, Against] (putArrayVar voteVar pn) pn
    inputs <- mapM askPlayer pns
    let voteData = VoteData msgEnd voteVar inputs assessFunction
    evalStateT assessors voteData
    cleanVote voteData
    return $ MsgResp msgEnd

-- | assess the vote on every new vote with the assess function, and as soon as the vote has an issue (positive of negative), sends a signal
assessOnEveryVotes :: Assessor ()
assessOnEveryVotes = do
   (VoteData msgEnd voteVar _ assess) <- get
   lift $ do
      msgVotes <- getArrayVarMessage voteVar
      onMessage msgVotes $ \(MessageData votes) -> maybeWhen (assess votes) $ sendMessage msgEnd


-- | assess the vote with the assess function when time is reached, and sends a signal with the issue (positive of negative)
--
assessOnTimeLimit ::  UTCTime -> Assessor ()
assessOnTimeLimit time = do
   (VoteData msgEnd voteVar _ assess) <- get
   lift $ do
      onEvent_ (Time time) $ \_ -> do
         votes <- getArrayVarData voteVar
         let result = assess $ getOnlyVoters $ votes
         when (result == Nothing) $ outputAll "Vote: Quorum not reached, rule is rejected"
         sendMessage msgEnd $ fromMaybe False $ result

-- | assess the vote with the assess function when time is elapsed, and sends a signal with the issue (positive of negative)
assessOnTimeDelay ::  NominalDiffTime -> Assessor ()
assessOnTimeDelay delay = do
   t <- addUTCTime delay <$> lift getCurrentTime
   assessOnTimeLimit t

-- | assess the vote only when every body voted. An error is generated if the assessing function returns Nothing.
assessWhenEverybodyVoted :: Assessor ()
assessWhenEverybodyVoted = do
   (VoteData msgEnd voteVar _ assess) <- get
   lift $ do
      msgVotes <- getArrayVarMessage voteVar
      onMessage msgVotes $ \(MessageData votes) -> when (length (voters votes) == length votes) $
         sendMessage msgEnd $ fromJust $ assess $ votes

-- | players that did not voted are counted as negative.
noStatusQuo :: [Vote] -> [Vote]
noStatusQuo = map noVoteCountAsAgainst where
   noVoteCountAsAgainst :: Vote -> Vote
   noVoteCountAsAgainst (a, Nothing) = (a, Just Against)
   noVoteCountAsAgainst a = a

-- | clean events and variables necessary for the vote
cleanVote :: VoteData -> Nomex ()
cleanVote (VoteData msgEnd voteVar inputsNumber _) = onMessage msgEnd$ \_ -> do
   delAllEvents msgEnd
   delArrayVar voteVar
   mapM_ delEvent inputsNumber

assessOnlyVoters :: AssessFunction -> AssessFunction
assessOnlyVoters assess vs = assess $ map (second Just) $ voters vs

-- | a quorum is the neccessary number of voters for the validity of the vote
quorum :: Int -> [Vote] -> Bool
quorum q vs = (length $ voters vs) >= q

-- | adds a quorum to an assessing function
withQuorum :: AssessFunction -> Int -> AssessFunction
withQuorum assess q vs = if (quorum q vs) then assess vs else Nothing

-- | assess the vote results according to a unanimity (everybody votes for)
unanimity :: [Vote] -> Maybe Bool
unanimity votes = voteQuota (length votes) votes
  
-- | assess the vote results according to an absolute majority (half voters plus one, no quorum is needed)
majority :: [Vote] -> Maybe Bool
majority votes = voteQuota ((length votes) `div` 2 + 1) votes

-- | assess the vote results according to a majority of x (in %)
majorityWith :: Int -> [Vote] -> Maybe Bool
majorityWith x votes = voteQuota ((length votes) * x `div` 100 + 1) votes

-- | assess the vote results according to a necessary number of positive votes
numberPositiveVotes :: Int -> [Vote] -> Maybe Bool
numberPositiveVotes = voteQuota

-- | helper function for assessement functions
voteQuota :: Int -> [Vote] -> Maybe Bool
voteQuota quotaFor votes
   | nbFor votes >= quotaFor = Just True
   | nbAgainst votes > (length votes) - quotaFor = Just False
   | otherwise = Nothing
   
-- | get the number of positive votes and negative votes
nbFor, nbAgainst :: [Vote] -> Int
nbFor = length . filter ((== Just For) . snd)
nbAgainst = length . filter ((== Just Against) . snd)
      
-- | get only those who voted
voters :: [(PlayerNumber, Maybe ForAgainst)] -> [(PlayerNumber, ForAgainst)]
voters vs = catMaybes $ map voter vs where
    voter (pn, Just fa) = Just (pn, fa)
    voter (_, Nothing) = Nothing

-- | get only those who voted
getOnlyVoters :: [(PlayerNumber, Maybe ForAgainst)] -> [(PlayerNumber, Maybe ForAgainst)]
getOnlyVoters vs = map (second Just) $ voters vs

-- | activate or reject a rule
activateOrReject :: Rule -> Bool -> Nomex ()
activateOrReject r b = if b then activateRule_ (_rNumber r) else rejectRule_ (_rNumber r)

-- | perform an action for each current players, new players and leaving players
forEachPlayer :: (PlayerNumber -> Nomex ()) -> (PlayerNumber -> Nomex ()) -> (PlayerNumber -> Nomex ()) -> Nomex ()
forEachPlayer action actionWhenArrive actionWhenLeave = do
    pns <- getAllPlayerNumbers
    mapM_ action pns
    onEvent_ (Player Arrive) $ \(PlayerData p) -> actionWhenArrive $ _playerNumber p
    onEvent_ (Player Leave) $ \(PlayerData p) -> actionWhenLeave $ _playerNumber p

-- | perform the same action for each players, including new players
forEachPlayer_ :: (PlayerNumber -> Nomex ()) -> Nomex ()
forEachPlayer_ action = forEachPlayer action action (\_ -> return ())


-- | create a value initialized for each players
--manages players joining and leaving
createValueForEachPlayer :: Int -> V [(Int, Int)] -> Nomex ()
createValueForEachPlayer initialValue var = do
    pns <- getAllPlayerNumbers
    v <- newVar_ (varName var) $ map (,initialValue::Int) pns
    forEachPlayer (\_-> return ())
                  (\p -> modifyVar v ((p, initialValue) : ))
                  (\p -> modifyVar v $ filter $ (/= p) . fst)

-- | create a value initialized for each players initialized to zero
--manages players joining and leaving
createValueForEachPlayer_ :: V [(Int, Int)] -> Nomex ()
createValueForEachPlayer_ = createValueForEachPlayer 0

modifyValueOfPlayer :: PlayerNumber -> V [(Int, Int)] -> (Int -> Int) -> Nomex ()
modifyValueOfPlayer pn var f = modifyVar var $ map $ (\(a,b) -> if a == pn then (a, f b) else (a,b))

modifyAllValues :: V [(Int, Int)] -> (Int -> Int) -> Nomex ()
modifyAllValues var f = modifyVar var $ map $ second f

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

oneDay :: NominalDiffTime
oneDay = 60 * 60 * 24

maybeWhen :: Maybe a -> (a -> Nomex ()) -> Nomex ()
maybeWhen = forM_
