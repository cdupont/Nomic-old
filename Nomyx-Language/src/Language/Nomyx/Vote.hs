{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeFamilies, FlexibleContexts,
    DeriveDataTypeable, GADTs #-}
    
-- | Voting system
module Language.Nomyx.Vote where

import Prelude hiding (foldr)
import Language.Nomyx.Expression
import Language.Nomyx.Definition
import Language.Nomyx.Rule
import Data.Typeable
import Control.Monad.State hiding (forM_)
import Data.Maybe
import Data.Time hiding (getCurrentTime)
import Control.Arrow
import Control.Applicative
import Language.Nomyx.Utils
import Data.List
import qualified Data.Map as M
import Control.Monad.Error (MonadError(..))


data VoteType a = ExclusiveVote (Maybe (Alts a))
                | NonExclusiveVote [Alts a]

class (Eq (Alts a), Show (Alts a), Ord (Alts a), Typeable a) => Votable a where
   data Alts a
   alts :: [Alts a]
   quota :: Alts a -> Int -> Int -> Int
   name :: a -> String
   exclusiveWinner :: a ->  Maybe (Alts a, Alts a)
   exclusiveWinner _ = Nothing

type ForAgainst = Alts Rule
instance Votable Rule where
   data Alts Rule = For | Against deriving (Typeable, Enum, Show, Eq, Bounded, Read, Ord)
   alts = [For, Against]
   quota For q _ = q
   quota Against q vs = vs - q + 1
   name r = "rule " ++ (show $ _rNumber r)
   exclusiveWinner _ = Just (For, Against)


type Vote a = (PlayerNumber, Maybe (Alts a))
type VoteResult a = VoteStats a -> [Alts a]
data VoteStats a = VoteStats { voteCounts :: M.Map (Maybe (Alts a)) Int,
                               voteFinished :: Bool}

data VoteData a = VoteData { msgEnd :: Msg [Alts a],                   -- message sent when the vote is finished with the winners
                             voteVar :: ArrayVar PlayerNumber (Alts a),-- variable containing the votes
                             inputNumbers :: [EventNumber],            -- numbers of the toggle inputs of the players (to delete them at the end of vote)
                             assessFunction :: VoteResult a}           -- function used to count the votes
type Assessor a = StateT (VoteData a) Nomex ()

-- | Perform a vote.
voteWith :: (Votable a) => VoteResult a         -- ^ the function used to count the votes.
                        -> Assessor a           -- ^ assessors: when and how to perform the vote assessment (several assessors can be chained).
                        -> a                    -- ^ toVote: the matter to be voted.
                        -> [Alts a]             -- ^ the vote alternatives.
                        -> Nomex (Msg [Alts a]) -- ^ return value: a message containing the result of the vote. 
voteWith countVotes assessors toVote als = do
    pns <- getAllPlayerNumbers
    let toVoteName = name toVote
    let msgEnd = Message ("Result of votes for " ++ toVoteName) :: Msg [Alts a]
    --create an array variable to store the votes.
    (voteVar :: ArrayVar PlayerNumber (Alts a)) <- newArrayVar_ ("Votes for " ++ toVoteName) pns
    let askPlayer pn = onInputRadioOnce ("Vote for " ++ toVoteName ++ ":") als (putArrayVar_ voteVar pn) pn
    inputs <- mapM askPlayer pns
    let voteData = VoteData msgEnd voteVar inputs countVotes
    evalStateT assessors voteData
    mapM (\n -> displayVoteVar n ("Votes for " ++ toVoteName ++ ":") voteVar) pns
    displayVoteResult toVoteName voteData
    cleanVote voteData
    return $ msgEnd

-- | Performs a vote, all the possible alternatives are selected.
voteWith_ :: (Votable a) => VoteResult a -> Assessor a -> a -> Nomex (Msg [Alts a]) 
voteWith_ assessFunction assessors toVote = voteWith assessFunction assessors toVote alts


-- | assess the vote on every new vote with the assess function, and as soon as the vote has an issue (positive of negative), sends a signal
assessOnEveryVote :: (Votable a) => Assessor a
assessOnEveryVote = do
   (VoteData msgEnd voteVar _ assess) <- get
   lift $ do
      onMsgVarChange voteVar $ f assess msgEnd where
         f assess msgEnd (VUpdated votes) = do
            let res = assess $ getVoteStats votes False
            when (not $ null res) $ sendMessage msgEnd res
         f _ _ _ = return ()   


-- | assess the vote with the assess function when time is reached, and sends a signal with the issue (positive of negative)
assessOnTimeLimit :: (Votable a) => UTCTime -> Assessor a
assessOnTimeLimit time = do
   (VoteData msgEnd voteVar _ assess) <- get
   lift $ do
      onEvent_ (Time time) $ \_ -> do
         votes <- getMsgVarData_ voteVar
         sendMessage msgEnd (assess $ getVoteStats votes True)

   
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
      onMsgVarChange voteVar $ f assess msgEnd where
         f assess msgEnd (VUpdated votes) = when (all isJust (map snd votes)) $ sendMessage msgEnd $ assess $ getVoteStats votes True
         f _ _ _ = return ()


-- | clean events and variables necessary for the vote
cleanVote :: (Votable a) => VoteData a -> Nomex ()
cleanVote (VoteData msgEnd voteVar inputsNumber _) = onMessage msgEnd $ \_ -> do
   delAllEvents msgEnd
   delMsgVar voteVar
   mapM_ delEvent inputsNumber


-- | a quorum is the neccessary number of voters for the validity of the vote
quorum :: (Votable a) => Int -> VoteStats a -> Bool
quorum q vs = (voted vs) >= q

-- | adds a quorum to an assessing function
withQuorum :: (Votable a) => VoteResult a -> Int -> VoteResult a
withQuorum assess q vs = if (quorum q vs) then assess vs else []

-- | assess the vote results according to a unanimity (everybody votes for)
unanimity :: (Votable a) => VoteStats a -> [Alts a]
unanimity vs = voteQuota (nbVoters vs) vs
  
-- | assess the vote results according to an absolute majority (half voters plus one, no quorum is needed)
majority :: (Votable a) => VoteStats a -> [Alts a]
majority vs = voteQuota ((nbVoters vs) `div` 2 + 1) vs

-- | assess the vote results according to a majority of x (in %)
majorityWith :: (Votable a) => Int -> VoteStats a -> [Alts a]
majorityWith x vs = voteQuota ((nbVoters vs) * x `div` 100 + 1) vs

-- | assess the vote results according to a necessary number of positive votes
numberVotes :: (Votable a) => Int -> VoteStats a -> [Alts a]
numberVotes i vs = voteQuota i vs

-- | return the vote alternatives that are above threshold
voteQuota :: forall a. (Votable a) => Int -> VoteStats a -> [Alts a]
voteQuota q votes = case (exclusiveWinner (undefined :: a)) of
   Nothing -> catMaybes $ M.keys $ M.filter (>= q) (voteCounts votes)
   Just a -> maybeToList $ exclusiveVoteQuota q votes a

exclusiveVoteQuota :: (Votable a) => Int -> VoteStats a -> (Alts a, Alts a) -> Maybe (Alts a)
exclusiveVoteQuota q votes (for, against)
   | M.findWithDefault 0 (Just for) vs     >= q                   = Just for
   | M.findWithDefault 0 (Just against) vs > (nbVoters votes) - q = Just against
   | otherwise = Nothing
   where vs = voteCounts votes


-- | number of people that voted if the voting is finished,
-- total number of people that should vote otherwise
nbVoters :: (Votable a) => VoteStats a -> Int
nbVoters vs
   | voteFinished vs = (totalVoters vs) - (notVoted vs)
   | otherwise = totalVoters vs

totalVoters, voted, notVoted :: (Votable a) => VoteStats a -> Int
totalVoters vs = M.foldr (+) 0 (voteCounts vs)
notVoted    vs = fromMaybe 0 $ M.lookup Nothing (voteCounts vs)
voted       vs = (totalVoters vs) - (notVoted vs)


getVoteStats :: (Votable a) => [Vote a] -> Bool -> VoteStats a
getVoteStats vs voteFinished = VoteStats
   {voteCounts = M.fromList $ counts (snd <$> vs),
    voteFinished = voteFinished}

counts :: (Eq a, Ord a) => [a] -> [(a, Int)]
counts as = map (head &&& length) (group $ sort as)


displayVoteVar :: (Votable a) => PlayerNumber -> String -> ArrayVar PlayerNumber (Alts a) -> Nomex ()
displayVoteVar pn title mv = do
   sp <- showPlayer
   displayVar pn mv (showVotes title sp)


showChoice :: (Votable a) => Maybe (Alts a) -> String
showChoice (Just a) = show a
showChoice Nothing  = "Not Voted "

showChoices :: (Votable a) => [(Alts a)] -> String
showChoices cs = concat $ intersperse ", " $ map show cs

showVotes :: (Votable a) => String -> (PlayerNumber -> String) -> [(PlayerNumber, Maybe (Alts a))] -> String
showVotes title showPlayer l = title ++ "\n" ++
                     concatMap (\(i,a) -> (showPlayer i) ++ "\t" ++ (showChoice a) ++ "\n") l

showVotes' :: (Votable a) => (PlayerNumber -> String) -> [(PlayerNumber, Maybe (Alts a))] -> String
showVotes' showPlayer l = concat $ intersperse ", " $ map (\(i,a) -> (showPlayer i) ++ ": " ++ (showChoice a)) voted where
   voted = filter (\(_, r) -> isJust r) l
                                              
displayVoteResult :: (Votable a) => String -> VoteData a -> Nomex ()
displayVoteResult toVoteName (VoteData msgEnd voteVar _ _) = onMessage msgEnd $ \(MessageData result) -> do
   vs <- getMsgVarData_ voteVar
   sp <- showPlayer
   outputAll $ "Vote result for " ++ toVoteName ++ ": " ++ (showChoices result) ++
               " (" ++ showVotes' sp vs ++ ")"

-- | any new rule will be activate if the rule in parameter returns True
onRuleProposed :: (Rule -> Nomex (Msg [ForAgainst]) ) -> RuleFunc
onRuleProposed f = voidRule $ onEvent_ (RuleEv Proposed) $ \(RuleData rule) -> do
    resp <- f rule
    onMessageOnce resp $ (activateOrReject rule) . (== [For]) . messageData

-- * Referendum & elections

data Referendum = Referendum String deriving (Typeable)
instance Votable Referendum where
   data Alts Referendum = Yes | No deriving (Enum, Show, Eq, Bounded, Read, Ord)
   alts = [Yes, No]
   quota Yes q _ = q
   quota No q vs = vs - q + 1
   name (Referendum n) = "referendum on " ++ n

referendum :: String -> Nomex () -> RuleFunc
referendum name action = voidRule $ do
   msg <- voteWith_ (majority `withQuorum` 2) (assessOnEveryVote >> assessOnTimeDelay oneDay) (Referendum name)
   onMessageOnce msg resolution where
      resolution (MessageData [Yes]) = do
            outputAll "Positive result of referendum"
            action
      resolution (MessageData [No]) = outputAll "Negative result of referendum"
      resolution (MessageData [])   = outputAll "No result for referendum"
      resolution (MessageData _)    = throwError "Impossible result for referendum"


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
      resolution (MessageData [Candidate pi]) = do
         outputAll $ "Result of elections: player(s) " ++ (show $ _playerName pi) ++ " won!"
         action $ _playerNumber pi
      resolution (MessageData []) = outputAll $ "Result of elections: nobody won!"
      resolution (MessageData _)  = throwError "Impossible result for elections"
