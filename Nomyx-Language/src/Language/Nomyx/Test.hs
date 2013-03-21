{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, NamedFieldPuns, GADTs#-}

module Language.Nomyx.Test where

import Language.Nomyx.Rule
import Language.Nomyx.Expression
import Language.Nomyx.Evaluation
import Language.Nomyx.Definition
import Control.Monad.State
import Data.Typeable
import Data.Lens


date1 = parse822Time "Tue, 02 Sep 1997 09:00:00 -0400"
date2 = parse822Time "Tue, 02 Sep 1997 10:00:00 -0400"
date3 = parse822Time "Tue, 02 Sep 1997 11:00:00 -0400"

testGame = Game { _gameName      = "test",
                  _gameDesc      = GameDesc "test" "test",
                  _rules         = [],
                  _players       = [PlayerInfo 1 "coco"],
                  _variables     = [],
                  _events        = [],
                  _outputs       = [],
                  _victory       = [],
                  _currentTime   = date1}

testRule = Rule  { _rNumber       = 0,
                   _rName         = "test",
                   _rDescription  = "test",
                   _rProposedBy   = 0,
                   _rRuleCode     = "",
                   _rRuleFunc     = return Void,
                   _rStatus       = Pending,
                   _rAssessedBy   = Nothing}

evalRuleFunc f = evalState (evalExp f 0) testGame
execRuleFuncEvent f e d = execState (evalExp f 0 >> (triggerEvent e d)) testGame
execRuleFuncGame f g = execState (evalExp f 0) g
execRuleFuncEventGame f e d g = execState (evalExp f 0 >> (triggerEvent e d)) g
execRuleFunc f = execRuleFuncGame f testGame

tests = [("test var 1", testVarEx1),
         ("test var 2", testVarEx2),
         ("test var 3", testVarEx3),
         ("test var 4", testVarEx4),
         ("test var 5", testVarEx5),
         ("test single input", testSingleInputEx),
         ("test input string", testInputStringEx),
         ("test send messsage", testSendMessageEx),
         ("test send message 2", testSendMessageEx2),
         ("test user input write", testUserInputWriteEx),
         ("test activate rule", testActivateRuleEx),
         ("test auto activate", testAutoActivateEx),
         ("test meta rules vote", testApplicationMetaRuleEx),
         ("test time event", testTimeEventEx),
         ("test time event 2", testTimeEventEx2),
         ("test assess on vote complete 1", testVoteAssessOnVoteComplete1),
         ("test assess on vote complete 2", testVoteAssessOnVoteComplete2),
         ("test assess on every vote 1", testVoteAssessOnEveryVotes1),
         ("test assess on every vote 2", testVoteAssessOnEveryVotes2),
         ("test assess on every vote 3", testVoteAssessOnEveryVotes3),
         ("test assess on every vote 4", testVoteAssessOnEveryVotes4),
         ("test majority with", testVoteMajorityWith),
         ("test number positive votes", testVoteNumberPositiveVotes),
         ("test vote with quorum 1", testVoteWithQuorum1),
         ("test vote with quorum 2", testVoteWithQuorum2),
         ("test assess on time limit 1", testVoteAssessOnTimeLimit1),
         ("test assess on time limit 2", testVoteAssessOnTimeLimit2),
         ("test assess on time limit 3", testVoteAssessOnTimeLimit3),
         ("test assess on time limit 4", testVoteAssessOnTimeLimit4),
         ("test assess on time limit 5", testVoteAssessOnTimeLimit5)]

allTests = and $ map snd tests

--Test variable creation
testVar1 :: RuleFunc
testVar1 = voidRule $ do
   NewVar "toto" (1::Integer)
   return ()

testVarEx1 = (variables ^$ execRuleFunc testVar1) == [(Var 0 "toto" (1::Integer))]

--Test variable deleting
testVar2 :: RuleFunc
testVar2 = voidRule $ do
   var <- newVar_ "toto" (1::Int)
   delVar var
   return ()

testVarEx2 = _variables (execRuleFunc testVar2) == []

--Test variable reading
testVar3 :: RuleFunc
testVar3 = voidRule $ do
   var <- newVar_ "toto" (1::Int)
   a <- readVar var
   case a of
      Just (1::Int) -> output "ok" 1
      _ -> output "nok" 1

testVarEx3 = _outputs (execRuleFunc testVar3) == [(1,"ok")]

--Test variable writing
testVar4 :: RuleFunc
testVar4 = voidRule $ do
   var <- newVar_ "toto" (1::Int)
   writeVar var (2::Int)
   a <- readVar var
   case a of
      Just (2::Int) -> output "ok" 1
      _ -> output "nok" 1

testVarEx4 = _outputs (execRuleFunc testVar4) == [(1,"ok")]

--Test variable writing
testVar5 :: RuleFunc
testVar5 = voidRule $ do
   var <- newVar_ "toto" ([]::[Int])
   writeVar var ([1]::[Int])
   a <- readVar var
   case a of
      Just (a::[Int]) -> do
         writeVar var (2:a)
         return ()
      Nothing -> output "nok" 1

testVarEx5 = _variables (execRuleFunc testVar5) == [(Var 0 "toto" ([2,1]::[Int]))]

data Choice = Holland | Sarkozy deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test input
testSingleInput :: RuleFunc
testSingleInput = voidRule $ do
    onInputChoiceEnum_ "Vote for Holland or Sarkozy" Holland h 1 where
        h a = output ("voted for " ++ (show a)) 1

testSingleInputEx = (_outputs $ execRuleFuncEvent testSingleInput (inputChoiceEnum 1 "Vote for Holland or Sarkozy" Holland) (InputChoiceData Holland)) == [(1, "voted for Holland")]

testInputString :: RuleFunc
testInputString = voidRule $ do
    onInputString_ "Enter a number:" h 1 where
        h a = output ("You entered: " ++ a) 1

testInputStringEx = (_outputs $ execRuleFuncEvent testInputString (inputString 1 "Enter a number:") (InputStringData "1")) == [(1, "You entered: 1")]

-- Test message
testSendMessage :: RuleFunc
testSendMessage = voidRule $ do
    let msg = Message "msg" :: Event(Message String)
    onEvent_ msg f
    sendMessage msg "toto" where
        f (MessageData a :: EventData(Message String)) = output a 1

testSendMessageEx = _outputs (execRuleFunc testSendMessage) == [(1,"toto")]

testSendMessage2 :: RuleFunc
testSendMessage2 = voidRule $ do
    onEvent_ (Message "msg":: Event(Message ())) $ const $ output "Received" 1
    sendMessage_ (Message "msg")


testSendMessageEx2 = _outputs (execRuleFunc testSendMessage2) == [(1,"Received")]

data Choice2 = Me | You deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test user input + variable read/write
testUserInputWrite :: RuleFunc
testUserInputWrite = voidRule $ do
    newVar_ "vote" (Nothing::Maybe Choice2)
    onEvent_ (Message "voted" :: Event (Message ())) h2
    onEvent_ (InputChoice 1 "Vote for" [Me, You] Me) h1 where
        h1 (InputChoiceData a :: EventData (InputChoice Choice2)) = do
            writeVar (V "vote") (Just a)
            SendMessage (Message "voted") ()
        h2 (MessageData _) = do
            a <- readVar (V "vote")
            case a of
                Just (Just Me) -> output "voted Me" 1
                _ -> output "problem" 1

testUserInputWriteEx = (_outputs $ execRuleFuncEvent testUserInputWrite (InputChoice 1 "Vote for" [Me, You] Me) (InputChoiceData Me)) == [(1,"voted Me")]

-- Test ruel activation
testActivateRule :: RuleFunc
testActivateRule = voidRule $ do
    a <- GetRules
    if (_rStatus (head a) == Pending) then do
        ActivateRule $ _rNumber (head a)
        return ()
        else return ()


testActivateRuleEx = _rStatus (head $ _rules (execRuleFuncGame testActivateRule testGame {_rules=[testRule]}))  == Active

testAutoActivateEx = _rStatus (head $ _rules (execRuleFuncEventGame autoActivate (RuleEv Proposed) (RuleData testRule) (testGame {_rules=[testRule]})))  == Active

--Time tests

testTimeEvent :: RuleFunc
testTimeEvent = voidRule $ do
    onEvent_ (Time date1) f where
        f _ = outputAll $ show date1

testTimeEventEx = (_outputs $ execRuleFuncEvent testTimeEvent (Time date1) (TimeData date1)) == [(1,show date1)]

testTimeEvent2 :: Nomex ()
testTimeEvent2 = schedule' [date1, date2] (outputAll . show)

testTimeEventEx2 = (_outputs $ flip execState testGame (evalExp testTimeEvent2 0 >> gameEvs)) == [(1,show date2), (1,show date1)] where
    gameEvs = do
        evTriggerTime date1
        evTriggerTime date2


-- Test votes

voteGameActions :: Int -> Int -> Int  -> Bool -> State Game () -> Game
voteGameActions positives negatives total timeEvent actions = flip execState testGame {_players = []} $ do
    mapM_ (\x -> addPlayer (PlayerInfo x $ "coco " ++ (show x))) [1..total]
    actions
    evProposeRule testRule
    mapM_ (\x -> evInputChoice (InputChoice x "Vote for rule 0" [For, Against] For) For) [1..positives]
    mapM_ (\x -> evInputChoice (InputChoice (x+positives) "Vote for rule 0" [For, Against] For) Against) [1..negatives]
    when timeEvent $ evTriggerTime date2

voteGame' :: Int -> Int -> Int -> Bool -> RuleFunc -> Game
voteGame' positives negatives notVoted timeEvent rf  = voteGameActions positives negatives notVoted timeEvent $ do
   let rule = testRule {_rName = "unanimityRule", _rRuleFunc = rf, _rNumber = 1, _rStatus = Active}
   evAddRule rule
   evActivateRule (_rNumber rule) 0
   return ()

voteGame :: Int -> Int -> Int -> RuleFunc -> Game
voteGame positives negatives notVoted rf = voteGame' positives negatives notVoted True rf

voteGameTimed :: Int -> Int -> Int -> RuleFunc -> Game
voteGameTimed positives negatives notVoted rf = voteGame' positives negatives notVoted True rf

-- Test application meta rule
unanimityRule = testRule {_rName = "unanimityRule", _rRuleFunc = return $ Meta $ voteWith unanimity $ assessWhenEverybodyVoted, _rNumber = 1, _rStatus = Active}
applicationMetaRuleRule = testRule {_rName = "onRuleProposedUseMetaRules", _rRuleFunc = onRuleProposed checkWithMetarules, _rNumber = 2, _rStatus = Active}
testApplicationMetaRuleVote :: Game
testApplicationMetaRuleVote = voteGameActions 2 0 2 False $ do
    evAddRule unanimityRule
    evActivateRule (_rNumber unanimityRule) 0
    evAddRule applicationMetaRuleRule
    evActivateRule (_rNumber applicationMetaRuleRule) 0
    return ()

testApplicationMetaRuleEx = (_rStatus $ head $ _rules testApplicationMetaRuleVote) == Active

-- vote rules
testVoteAssessOnVoteComplete1 = testVoteRule Active  $ voteGame      10 0 10 $ onRuleProposed $ voteWith majority $ assessWhenEverybodyVoted
testVoteAssessOnVoteComplete2 = testVoteRule Pending $ voteGame      9  0 10 $ onRuleProposed $ voteWith majority $ assessWhenEverybodyVoted
testVoteAssessOnEveryVotes1   = testVoteRule Active  $ voteGame      10 0 10 $ onRuleProposed $ voteWith unanimity $ assessOnEveryVotes
testVoteAssessOnEveryVotes2   = testVoteRule Active  $ voteGame      6  0 10 $ onRuleProposed $ voteWith majority $ assessOnEveryVotes
testVoteAssessOnEveryVotes3   = testVoteRule Pending $ voteGame      5  0 10 $ onRuleProposed $ voteWith majority $ assessOnEveryVotes
testVoteAssessOnEveryVotes4   = testVoteRule Reject  $ voteGame      0  5 10 $ onRuleProposed $ voteWith majority $ assessOnEveryVotes
testVoteMajorityWith          = testVoteRule Active  $ voteGame      6  0 10 $ onRuleProposed $ voteWith (majorityWith 50) $ assessOnEveryVotes
testVoteNumberPositiveVotes   = testVoteRule Active  $ voteGame      3  7 10 $ onRuleProposed $ voteWith (numberPositiveVotes 3) $ assessOnEveryVotes
testVoteWithQuorum1           = testVoteRule Active  $ voteGame      7  3 10 $ onRuleProposed $ voteWith (majority `withQuorum` 7) $ assessOnEveryVotes
testVoteWithQuorum2           = testVoteRule Pending $ voteGame      6  0 10 $ onRuleProposed $ voteWith (majority `withQuorum` 7) $ assessOnEveryVotes
testVoteAssessOnTimeLimit1    = testVoteRule Active  $ voteGameTimed 10 0 10 $ onRuleProposed $ voteWith unanimity $ assessOnTimeLimit date2
testVoteAssessOnTimeLimit2    = testVoteRule Active  $ voteGameTimed 1  0 10 $ onRuleProposed $ voteWith unanimity $ assessOnTimeLimit date2
testVoteAssessOnTimeLimit3    = testVoteRule Reject  $ voteGameTimed 1  0 10 $ onRuleProposed $ voteWith (unanimity `withQuorum` 5) $ assessOnTimeLimit date2
testVoteAssessOnTimeLimit4    = testVoteRule Reject  $ voteGameTimed 0  0 10 $ onRuleProposed $ voteWith (unanimity `withQuorum` 1) $ assessOnTimeLimit date2
testVoteAssessOnTimeLimit5    = testVoteRule Pending $ voteGameTimed 10 0 10 $ onRuleProposed $ voteWith unanimity $ assessOnTimeLimit date3

testVoteRule s g = (_rStatus $ head $ _rules g) == s
