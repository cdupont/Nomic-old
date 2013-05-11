{-# LANGUAGE StandaloneDeriving, GADTs, DeriveDataTypeable,
    FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies,
    TypeOperators, FlexibleInstances, NoMonomorphismRestriction,
    TypeSynonymInstances, DoAndIfThenElse, RecordWildCards #-}

-- | This module implements Game management.
-- a game is a set of rules, and results of actions made by players (usually vote results)
-- the module manages the effects of rules over each others.
module Language.Nomyx.Game (GameEvent(..), update, update', LoggedGame(..), game, gameLog, emptyGame,
  execWithGame, execWithGame', outputAll, getLoggedGame, tracePN, getTimes, activeRules, pendingRules, rejectedRules)  where

import Prelude hiding (catch)
import Control.Monad.State
import Data.List
import Language.Nomyx hiding (outputAll)
import Data.Lens
import Control.Category ((>>>))
import Data.Lens.Template
import Control.Exception

data TimedEvent = TimedEvent UTCTime GameEvent deriving (Show, Read, Eq, Ord)

data GameEvent = GameSettings      GameName GameDesc UTCTime
               | JoinGame          PlayerNumber PlayerName
               | LeaveGame         PlayerNumber
               | ProposeRuleEv     PlayerNumber SubmitRule
               | InputChoiceResult PlayerNumber EventNumber Int
               | InputStringResult PlayerNumber String String
               | OutputPlayer      PlayerNumber String
               | TimeEvent         UTCTime
               | SystemAddRule     SubmitRule
                 deriving (Show, Read, Eq, Ord)

--A game being non serializable, we have to store events in parralel in order to rebuild the state latter.
data LoggedGame = LoggedGame { _game :: Game,
                               _gameLog :: [TimedEvent]}
                               deriving (Read, Show)

instance Eq LoggedGame where
   (LoggedGame {_game=g1}) == (LoggedGame {_game=g2}) = g1 == g2

instance Ord LoggedGame where
   compare (LoggedGame {_game=g1}) (LoggedGame {_game=g2}) = compare g1 g2


emptyGame name desc date = Game {
    _gameName      = name,
    _gameDesc      = desc,
    _rules         = [],
    _players       = [],
    _variables     = [],
    _events        = [],
    _outputs       = [],
    _victory       = [],
    _currentTime   = date}

$( makeLens ''LoggedGame)

--TODO: get rid of inter param?
enactEvent :: GameEvent -> Maybe (RuleCode -> IO RuleFunc) -> StateT Game IO ()
enactEvent (GameSettings name desc date) _    = liftT $ gameSettings name desc date
enactEvent (JoinGame pn name) _               = liftT $ joinGame name pn
enactEvent (LeaveGame pn) _                   = liftT $ leaveGame pn
enactEvent (ProposeRuleEv pn sr) (Just inter) = void $ proposeRule sr pn inter
enactEvent (InputChoiceResult pn en ci) _     = liftT $ inputChoiceResult en ci pn
enactEvent (InputStringResult pn ti res) _    = liftT $ inputStringResult (InputString pn ti) res pn
enactEvent (OutputPlayer pn s) _              = liftT $ outputPlayer s pn
enactEvent (TimeEvent t) _                    = liftT $ evTriggerTime t
enactEvent (SystemAddRule r) (Just inter)     = systemAddRule r inter
enactEvent (ProposeRuleEv _ _) Nothing        = error "ProposeRuleEv: interpreter function needed"
enactEvent (SystemAddRule _) Nothing          = error "SystemAddRule: interpreter function needed"

enactTimedEvent :: Maybe (RuleCode -> IO RuleFunc) -> TimedEvent -> StateT Game IO ()
enactTimedEvent inter (TimedEvent t ge) = do
   currentTime ~= t
   enactEvent ge inter


update :: GameEvent -> StateT LoggedGame IO ()
update ge = update' Nothing ge

update' :: Maybe (RuleCode -> IO RuleFunc) -> GameEvent -> StateT LoggedGame IO ()
update' inter ge = do
   --t <- lift $ T.getCurrentTime
   t <- access $ game >>> currentTime
   let te = TimedEvent t ge
   gameLog %= \gl -> gl ++ [te]
   evalTimedEvent te inter `liftCatchIO` commandExceptionHandler'

evalTimedEvent :: TimedEvent -> Maybe (RuleCode -> IO RuleFunc) -> StateT LoggedGame IO ()
evalTimedEvent (TimedEvent _ e) inter = focus game $ do
   enactEvent e inter
   lg <- get
   lift $ evaluate lg
   return ()

commandExceptionHandler' :: ErrorCall -> StateT LoggedGame IO ()
commandExceptionHandler' e = do
   lift $ putStrLn $ "Exception in rule: " ++ (show e)
   outputAll $ "Error in command: " ++ (show e)


getLoggedGame :: Game -> (RuleCode -> IO RuleFunc) -> [TimedEvent] -> IO LoggedGame
getLoggedGame g mInter tes = do
   let a = mapM_ (enactTimedEvent (Just mInter)) tes
   g' <- execStateT a g
   return $ LoggedGame g' tes


-- | initialize the game.
gameSettings :: GameName -> GameDesc -> UTCTime -> State Game ()
gameSettings name desc date = do
   gameName ~= name
   gameDesc ~= desc
   currentTime ~= date
   return ()


-- | join the game.
joinGame :: PlayerName -> PlayerNumber -> State Game ()
joinGame name pn = do
   g <- get
   case find ((== pn) . getL playerNumber) (_players g) of
      Just _ -> return ()
      Nothing -> do
         tracePN pn $ "Joining game: " ++ (_gameName g)
         let player = PlayerInfo { _playerNumber = pn, _playerName = name}
         players %= (player : )
         triggerEvent (Player Arrive) (PlayerData player)


-- | leave the game.
leaveGame :: PlayerNumber -> State Game ()
leaveGame pn = void $ evDelPlayer pn


-- | insert a rule in pending rules.
proposeRule :: SubmitRule -> PlayerNumber -> (RuleCode -> IO RuleFunc) -> StateT Game IO ()
proposeRule sr pn inter = do
   rule <- createRule sr pn inter
   r <- liftT $ evProposeRule rule
   if r == True then tracePN pn $ "Your rule has been added to pending rules."
   else tracePN pn $ "Error: Rule could not be proposed"


outputPlayer :: String -> PlayerNumber -> State Game ()
outputPlayer s pn = void $ outputs %= ((pn, s) : )

outputAll :: String -> StateT LoggedGame IO ()
outputAll s = do
   pls <- access (game >>> players)
   mapM_ (update . ((flip OutputPlayer) s)) (map _playerNumber pls)

inputChoiceResult :: EventNumber -> Int -> PlayerNumber -> State Game ()
inputChoiceResult eventNumber choiceIndex pn = do
   tracePN pn $ "input choice result: Event " ++ (show eventNumber) ++ ", choice " ++  (show choiceIndex)
   triggerChoice eventNumber choiceIndex

-- TODO maybe homogeneise both inputs event
inputStringResult :: Event InputString -> String -> PlayerNumber -> State Game ()
inputStringResult event input pn = do
   tracePN pn $ "input String result: input " ++ input
   triggerEvent event (InputStringData input)


getTimes :: EventHandler -> Maybe UTCTime
getTimes (EH _ _ (Time t) _) = Just t
getTimes _ = Nothing


-- | An helper function to use the state transformer GameState.
-- It additionally sets the current time.
execWithGame :: UTCTime -> State LoggedGame () -> LoggedGame -> LoggedGame
execWithGame t gs g = execState gs $ ((game >>> currentTime) `setL` t $ g)

execWithGame' :: UTCTime -> StateT LoggedGame IO () -> LoggedGame -> IO LoggedGame
execWithGame' t gs g = execStateT gs ((game >>> currentTime) `setL` t $ g)


--accessors

activeRules :: Game -> [Rule]
activeRules = sort . filter ((==Active) . getL rStatus) . _rules

pendingRules :: Game -> [Rule]
pendingRules = sort . filter ((==Pending) . getL rStatus) . _rules

rejectedRules :: Game -> [Rule]
rejectedRules = sort . filter ((==Reject) . getL rStatus) . _rules

instance Ord PlayerInfo where
   h <= g = (_playerNumber h) <= (_playerNumber g)


liftT :: Show s => State s a -> StateT s IO a
liftT st = do
   s1 <- get
   let (a, s) = runState st s1
   put s
   return a


liftCatchIO :: StateT s IO a -> (ErrorCall -> StateT s IO a) -> StateT s IO a
liftCatchIO m h = StateT $ \s -> runStateT m s `catch` \e -> runStateT (h e) s

createRule :: SubmitRule -> PlayerNumber -> (RuleCode -> IO RuleFunc) -> StateT Game IO Rule
createRule (SubmitRule name desc code) pn inter = do
   rs <- access rules
   let rn = getFreeNumber $ map _rNumber rs
   rf <- lift $ inter code
   return $ Rule {_rNumber = rn,
                  _rName = name,
                  _rDescription = desc,
                  _rProposedBy = pn,
                  _rRuleCode = code,
                  _rRuleFunc = rf,
                  _rStatus = Pending,
                  _rAssessedBy = Nothing}

systemAddRule :: SubmitRule -> (RuleCode -> IO RuleFunc) -> StateT Game IO ()
systemAddRule sr inter = do
   rule <- createRule sr 0 inter
   let sysRule = (rStatus ^= Active).(rAssessedBy ^= Just 0)
   rules %= (sysRule rule : )
   void $ liftT $ evalExp (_rRuleFunc rule) (_rNumber rule)

