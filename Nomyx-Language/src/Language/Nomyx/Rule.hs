{-# LANGUAGE DeriveDataTypeable, GADTs, ScopedTypeVariables, TupleSections,
    FlexibleInstances, TypeFamilies, FlexibleContexts, Rank2Types #-}

-- | Basic rules examples.
module Language.Nomyx.Rule where

import Prelude hiding (foldr)
import Language.Nomyx.Expression
import Language.Nomyx.Definition
import Data.Typeable
import Control.Monad.State hiding (forM_)
import Data.Maybe (mapMaybe, catMaybes, Maybe)
import Data.Time hiding (getCurrentTime)
import Control.Arrow
import Control.Applicative
import Data.Lens
import Data.Foldable hiding (and, mapM_)
import Data.Boolean
import Data.List (sort, group)
import Debug.Trace (trace)

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


-- | any new rule will be activate if the rule in parameter returns True
onRuleProposed :: (Rule -> Nomex (Msg (Maybe ForAgainst)) ) -> RuleFunc
onRuleProposed f = voidRule $ onEvent_ (RuleEv Proposed) $ \(RuleData rule) -> do
    resp <- f rule
    onMessageOnce resp $ (activateOrReject rule) . (== Just For) . messageData



class (Eq (Alts a), Show (Alts a), Ord (Alts a), Typeable a) => Votable a where
   data Alts a
   alts :: [Alts a]
   quota :: Alts a -> Int -> Int -> Int
   name :: a -> String

instance Votable Rule where
   data Alts Rule = For | Against deriving (Typeable, Enum, Show, Eq, Bounded, Read, Ord)
   alts = [For, Against]
   quota For q vs = q
   quota Against q vs = vs - q + 1
   name r = "rule " ++ (show $ _rNumber r)

type ForAgainst = Alts Rule
type Vote a = (PlayerNumber, Maybe (Alts a))
type Quotas a = [(Alts a, Int)]
type CountVotes a = [Vote a] -> Maybe (Alts a)
data VoteData a = VoteData { msgEnd :: Msg (Maybe (Alts a)),           -- message sent when the vote is finished (reached time limit or everybody voted, for example)
                             voteVar :: ArrayVar PlayerNumber (Alts a),-- variable containing the votes
                             inputNumbers :: [EventNumber],            -- numbers of the toggle inputs of the players (to delete them at the end of vote)
                             assessFunction :: CountVotes a}           -- function used to count the votes
type Assessor a = StateT (VoteData a) Nomex ()

-- | Performs a vote.
voteWith :: (Votable a) => CountVotes a         -- ^ the function used to count the votes.
                        -> Assessor a           -- ^ assessors: when and how to perform the vote assessment (several assessors can be chained).
                        -> a                    -- ^ toVote: the matter to be voted.
                        -> [Alts a]             -- ^ the vote alternatives.
                        -> Nomex (Msg (Maybe (Alts a))) -- ^ return value: a message containing the result of the vote. 
voteWith countVotes assessors toVote als = do
    pns <- getAllPlayerNumbers
    let toVoteName = name toVote
    let msgEnd = Message ("Result of votes for " ++ toVoteName) :: Msg (Maybe (Alts a))
    --create an array variable to store the votes.
    (voteVar :: ArrayVar PlayerNumber (Alts a)) <- newArrayVar ("Votes for " ++ toVoteName) pns
    let askPlayer pn = onInputChoiceOnce ("Vote for " ++ toVoteName ++ ":") als (putArrayVar voteVar pn) pn
    inputs <- mapM askPlayer pns
    let voteData = VoteData msgEnd voteVar inputs countVotes
    evalStateT assessors voteData
    cleanVote voteData
    return $ msgEnd

-- | Performs a vote, all the possible alternatives are selected.
voteWith_ :: (Votable a) => CountVotes a -> Assessor a -> a -> Nomex (Msg (Maybe (Alts a))) 
voteWith_ assessFunction assessors toVote = voteWith assessFunction assessors toVote alts


-- | assess the vote on every new vote with the assess function, and as soon as the vote has an issue (positive of negative), sends a signal
assessOnEveryVotes :: (Votable a) => Assessor a
assessOnEveryVotes = do
   (VoteData msgEnd voteVar _ assess) <- get
   lift $ do
      msgVotes <- getArrayVarMessage voteVar
      onMessage msgVotes $ \(MessageData votes) -> maybeWhen (assess votes) $ (\res -> sendMessage msgEnd (Just res))


-- | assess the vote with the assess function when time is reached, and sends a signal with the issue (positive of negative)
assessOnTimeLimit :: (Votable a) => UTCTime -> Assessor a
assessOnTimeLimit time = do
   (VoteData msgEnd voteVar _ assess) <- get
   lift $ do
      onEvent_ (Time time) $ \_ -> do
         votes <- getArrayVarData voteVar
         let result = assess $ getOnlyVoters $ votes -- TODO: check if getOnlyVoters is OK
         case result of
            Just _  -> outputAll "Vote assessed on time limit: Quorum reached"
            Nothing -> outputAll "Vote assessed on time limit: Quorum not reached"
         sendMessage msgEnd result

-- | assess the vote with the assess function when time is elapsed, and sends a signal with the issue (positive of negative)
assessOnTimeDelay :: (Votable a) => NominalDiffTime -> Assessor a
assessOnTimeDelay delay = do
   t <- addUTCTime delay <$> lift getCurrentTime
   assessOnTimeLimit t

-- | assess the vote only when every body voted. An error is generated if the assessing function returns Nothing.
assessWhenEverybodyVoted :: (Votable a) => Assessor a
assessWhenEverybodyVoted = do
   (VoteData msgEnd voteVar _ assess) <- get
   lift $ do
      msgVotes <- getArrayVarMessage voteVar
      onMessage msgVotes $ \(MessageData votes) -> when (length (voters votes) == length votes) $
         sendMessage msgEnd $ assess $ votes

-- | players that did not voted are counted as negative.
noStatusQuo :: [Vote Rule] -> [Vote Rule]
noStatusQuo = map noVoteCountAsAgainst where
   noVoteCountAsAgainst :: Vote Rule -> Vote Rule
   noVoteCountAsAgainst (a, Nothing) = (a, Just Against)
   noVoteCountAsAgainst a = a

-- | clean events and variables necessary for the vote
cleanVote :: (Votable a) => VoteData a -> Nomex ()
cleanVote (VoteData msgEnd voteVar inputsNumber _) = trace "CleanVote called" $ onMessage msgEnd$ \_ -> trace "msgEnd called in Cleanvote" $ do
   delAllEvents msgEnd
   delArrayVar voteVar
   mapM_ delEvent inputsNumber

assessOnlyVoters :: CountVotes a -> CountVotes a
assessOnlyVoters assess vs = assess $ map (second Just) $ voters vs

-- | a quorum is the neccessary number of voters for the validity of the vote
quorum :: Int -> [Vote a] -> Bool
quorum q vs = (length $ voters vs) >= q

-- | adds a quorum to an assessing function
withQuorum :: CountVotes a -> Int -> CountVotes a
withQuorum assess q vs = if (quorum q vs) then assess vs else Nothing

-- | assess the vote results according to a unanimity (everybody votes for)
unanimity :: (Votable a) => [Vote a] -> Maybe (Alts a)
unanimity vs = voteQuota (length vs) vs
  
-- | assess the vote results according to an absolute majority (half voters plus one, no quorum is needed)
majority :: (Votable a) => [Vote a] -> Maybe (Alts a)
majority votes = voteQuota ((length votes) `div` 2 + 1) votes

-- | assess the vote results according to a majority of x (in %)
majorityWith :: (Votable a) => Int -> [Vote a] -> Maybe (Alts a)
majorityWith x votes = voteQuota ((length votes) * x `div` 100 + 1) votes

-- | assess the vote results according to a necessary number of positive votes
numberPositiveVotes :: (Votable a) => Int -> [Vote a] -> Maybe (Alts a)
numberPositiveVotes i vs = voteQuota i vs

-- | return the first vote alternative that is above its threshold
voteQuota :: (Votable a) => Int -> [Vote a] -> Maybe (Alts a)
voteQuota q votes = snd <$> ((find (\ (nb, _) -> nb >= q) (getOnlyVotes $ voteCount votes)))

nbVotes :: (Votable a) => [Vote a] -> Alts a -> Int
nbVotes vs a = length $ filter ((== (Just a)) . snd) vs

voteCount :: (Votable a) => [(PlayerNumber, Maybe (Alts a))] -> [(Int, Maybe (Alts a))]
voteCount vs = counts (snd <$> vs)

getOnlyVotes :: [(Int, Maybe (Alts a))] -> [(Int, (Alts a))]
getOnlyVotes vs = mapMaybe noMaybe vs where
   noMaybe (a, Just b) = Just (a,b)
   noMaybe (a, Nothing) = Nothing

counts :: (Eq a, Ord a) => [a] -> [(Int, a)]
counts as = map (length &&& head) (group $ sort as)
   
-- | get only those who voted
voters :: [Vote a] -> [(PlayerNumber, Alts a)]
voters vs = catMaybes $ map voter vs where
   voter (pn, Just fa) = Just (pn, fa)
   voter (_, Nothing) = Nothing

-- | get only those who voted
getOnlyVoters :: [Vote a] -> [Vote a]
getOnlyVoters vs = map (second Just) $ voters vs

-- * Referendum & elections

data Referendum = Referendum String deriving (Typeable)
instance Votable Referendum where
   data Alts Referendum = Yes | No deriving (Enum, Show, Eq, Bounded, Read, Ord)
   alts = [Yes, No]
   quota Yes q vs = q
   quota No q vs = vs - q + 1
   name (Referendum n) = "referendum on " ++ n

referendum :: String -> Nomex () -> RuleFunc
referendum name action = voidRule $ do
   msg <- voteWith_ (majority `withQuorum` 2) (assessOnEveryVotes >> assessOnTimeDelay oneDay) (Referendum name)
   onMessageOnce msg resolution where
      resolution (MessageData (Just Yes)) = do
            outputAll "Positive result of referendum"
            action
      resolution (MessageData (Just No)) = outputAll "Negative result of referendum"
      resolution (MessageData Nothing)   = outputAll "No result for referendum"


data Election = Election String deriving (Typeable)
instance Votable Election where
   data Alts Election = Candidate {candidate :: PlayerInfo}
   alts = map (\n -> Candidate (PlayerInfo n "")) [1..]
   quota _ q _ = q
   name (Election n) = "elections on " ++ n

instance Show (Alts Election) where
   show (Candidate (PlayerInfo _ name)) = name

instance Eq (Alts Election) where
   (Candidate (PlayerInfo pn1 _)) == (Candidate (PlayerInfo pn2 _)) = pn1 == pn2

instance Ord (Alts Election) where
   compare (Candidate (PlayerInfo pn1 _)) (Candidate (PlayerInfo pn2 _)) = compare pn1 pn2

elections :: String -> [PlayerInfo] -> (PlayerNumber -> Nomex()) -> Nomex ()
elections name pns action = do
   msg <- voteWith majority (assessWhenEverybodyVoted {-assessOnEveryVotes >> assessOnTimeDelay oneDay-}) (Election name) (Candidate <$> pns)
   onMessageOnce msg resolution where
      resolution (MessageData (Just (Candidate pi))) = do
         outputAll $ "Result of elections: player " ++ (show $ _playerName pi) ++ " won!"
         action $ _playerNumber pi
      resolution (MessageData Nothing) = do
         outputAll $ "Result of elections: nobody won!"


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
