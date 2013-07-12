{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, NamedFieldPuns, GADTs#-}

module Language.Nomyx.Test where

import Language.Nomyx.Rule
import Language.Nomyx.Expression
import Language.Nomyx.Evaluation
import Language.Nomyx.Utils
import Language.Nomyx.Vote
import Control.Monad.State
import Data.Typeable
import Data.Lens
import Language.Nomyx.Definition

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
                  _logs          = [],
                  _currentTime   = date1}

testRule = Rule  { _rNumber       = 0,
                   _rName         = "test",
                   _rDescription  = "test",
                   _rProposedBy   = 0,
                   _rRuleCode     = "",
                   _rRuleFunc     = return Void,
                   _rStatus       = Pending,
                   _rAssessedBy   = Nothing}

evalRuleFunc f = evalState (runEvalError 0 $ evalExp f 0) testGame
execRuleFuncEvent f e d = execState (runEvalError 0 $ evalExp f 0 >> (triggerEvent_ e d)) testGame
execRuleFuncGame f g = execState (runEvalError 0 $ void $ evalExp f 0) g
execRuleFuncEventGame f e d g = execState (runEvalError 0 $ evalExp f 0 >> (triggerEvent_ e d)) g
execRuleFunc f = execRuleFuncGame f testGame

tests = [("test var 1", testVarEx1),
         ("test var 2", testVarEx2),
         ("test var 3", testVarEx3),
         ("test var 4", testVarEx4),
         ("test var 5", testVarEx5),
         ("test single input", testSingleInputEx),
         ("test multiple input", testMultipleInputsEx),
         ("test input string", testInputStringEx),
         ("test send messsage", testSendMessageEx),
         ("test send message 2", testSendMessageEx2),
         ("test user input write", testUserInputWriteEx),
         ("test activate rule", testActivateRuleEx),
         ("test auto activate", testAutoActivateEx),
         --("test meta rules vote", testApplicationMetaRuleEx),
         ("test time event", testTimeEventEx),
         ("test time event 2", testTimeEventEx2),
         ("test assess on vote complete 1", testVoteAssessOnVoteComplete1),
         ("test assess on vote complete 2", testVoteAssessOnVoteComplete2),
         ("test assess on every vote 1", testVoteAssessOnEveryVote1),
         ("test assess on every vote 2", testVoteAssessOnEveryVote2),
         ("test assess on every vote 3", testVoteAssessOnEveryVote3),
         ("test assess on every vote 4", testVoteAssessOnEveryVote4),
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
      Just (1::Int) -> newOutput_ "ok" 1
      _ -> newOutput_ "nok" 1

testVarEx3 = isOutput "ok" (execRuleFunc testVar3)

--Test variable writing
testVar4 :: RuleFunc
testVar4 = voidRule $ do
   var <- newVar_ "toto" (1::Int)
   writeVar var (2::Int)
   a <- readVar var
   case a of
      Just (2::Int) -> newOutput_ "ok" 1
      _ -> newOutput_ "nok" 1

testVarEx4 = isOutput "ok" (execRuleFunc testVar4)

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
      Nothing -> newOutput_ "nok" 1

testVarEx5 = _variables (execRuleFunc testVar5) == [(Var 0 "toto" ([2,1]::[Int]))]

data Choice = Holland | Sarkozy deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test input
testSingleInput :: RuleFunc
testSingleInput = voidRule $ do
    onInputRadioEnum_ "Vote for Holland or Sarkozy" Holland h 1 where
        h a = newOutput_ ("voted for " ++ (show a)) 1

testSingleInputEx = isOutput "voted for Holland" g where
   g = execRuleFuncEvent testSingleInput (inputRadioEnum 1 "Vote for Holland or Sarkozy" Holland) (InputData (RadioData Holland))

testMultipleInputs :: RuleFunc
testMultipleInputs = voidRule $ do
    onInputCheckbox_ "Vote for Holland and Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")] h 1 where
        h a = newOutput_ ("voted for " ++ (show a)) 1

testMultipleInputsEx = isOutput "voted for [Holland,Sarkozy]" g where
   g = execRuleFuncEvent testMultipleInputs (inputCheckbox 1 "Vote for Holland and Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")]) (InputData (CheckboxData [Holland, Sarkozy]))


testInputString :: RuleFunc
testInputString = voidRule $ do
    onInputText_ "Enter a number:" h 1 where
        h a = newOutput_ ("You entered: " ++ a) 1

testInputStringEx = isOutput "You entered: 1" g where
   g = execRuleFuncEvent testInputString (inputText 1 "Enter a number:") (InputData (TextData "1"))

-- Test message
testSendMessage :: RuleFunc
testSendMessage = voidRule $ do
    let msg = Message "msg" :: Event(Message String)
    onEvent_ msg f
    sendMessage msg "toto" where
        f (MessageData a :: EventData(Message String)) = newOutput_ a 1

testSendMessageEx = isOutput "toto" (execRuleFunc testSendMessage)

testSendMessage2 :: RuleFunc
testSendMessage2 = voidRule $ do
    onEvent_ (Message "msg":: Event(Message ())) $ const $ newOutput_ "Received" 1
    sendMessage_ (Message "msg")


testSendMessageEx2 = isOutput "Received" (execRuleFunc testSendMessage2)

data Choice2 = Me | You deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test user input + variable read/write
testUserInputWrite :: RuleFunc
testUserInputWrite = voidRule $ do
    newVar_ "vote" (Nothing::Maybe Choice2)
    onEvent_ (Message "voted" :: Event (Message ())) h2
    onEvent_ (InputEv (Input 1 "Vote for" (Radio [(Me, "Me"), (You, "You")]))) h1 where
        h1 (InputData (RadioData a) :: EventData (Input Choice2)) = do
            writeVar (V "vote") (Just a)
            SendMessage (Message "voted") ()
        h1 _ = undefined
        h2 (MessageData _) = do
            a <- readVar (V "vote")
            case a of
                Just (Just Me) -> newOutput_ "voted Me" 1
                _ -> newOutput_ "problem" 1
        h2 _ = undefined

testUserInputWriteEx = isOutput "voted Me" g where
   g = execRuleFuncEvent testUserInputWrite (InputEv (Input 1 "Vote for" (Radio [(Me, "Me"), (You, "You")]))) (InputData (RadioData Me))

-- Test rule activation
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

testTimeEventEx = isOutput (show date1) g where
   g = execRuleFuncEvent testTimeEvent (Time date1) (TimeData date1)

testTimeEvent2 :: Nomex ()
testTimeEvent2 = schedule' [date1, date2] (outputAll . show)

testTimeEventEx2 = isOutput (show date1) g && isOutput (show date2) g where
    g = flip execState testGame (runEvalError 0 $ evalExp testTimeEvent2 0 >> void gameEvs)
    gameEvs = do
        evTriggerTime date1
        evTriggerTime date2


-- Test votes

voteGameActions :: Int -> Int -> Int  -> Bool -> Evaluate () -> Game
voteGameActions positives negatives total timeEvent actions = flip execState testGame {_players = []} $ runEvalError 0 $ do
    mapM_ (\x -> addPlayer (PlayerInfo x $ "coco " ++ (show x))) [1..total]
    actions
    evProposeRule testRule
    evs <- getChoiceEvents
    let pos = take positives evs
    let neg = take negatives $ drop positives evs
    mapM_ (\x -> triggerInput x (URadioData $ fromEnum For)) pos
    mapM_ (\x -> triggerInput x (URadioData $ fromEnum Against)) neg
    when timeEvent $ void $ evTriggerTime date2

voteGame' :: Int -> Int -> Int -> Bool -> RuleFunc -> Game
voteGame' positives negatives notVoted timeEvent rf  = voteGameActions positives negatives notVoted timeEvent $ do
   let rule = testRule {_rName = "testRule", _rRuleFunc = rf, _rNumber = 1, _rStatus = Pending}
   evAddRule rule
   evActivateRule (_rNumber rule) 0
   return ()

voteGame :: Int -> Int -> Int -> RuleFunc -> Game
voteGame positives negatives notVoted rf = voteGame' positives negatives notVoted True rf

voteGameTimed :: Int -> Int -> Int -> RuleFunc -> Game
voteGameTimed positives negatives notVoted rf = voteGame' positives negatives notVoted True rf

-- Test application meta rule
--unanimityRule = testRule {_rName = "unanimityRule", _rRuleFunc = return $ Meta $ voteWith unanimity $ assessWhenEverybodyVoted, _rNumber = 1, _rStatus = Active}
--applicationMetaRuleRule = testRule {_rName = "onRuleProposedUseMetaRules", _rRuleFunc = onRuleProposed checkWithMetarules, _rNumber = 2, _rStatus = Active}
--testApplicationMetaRuleVote :: Game
--testApplicationMetaRuleVote = voteGameActions 2 0 2 False $ do
--    evAddRule unanimityRule
--    evActivateRule (_rNumber unanimityRule) 0
--    evAddRule applicationMetaRuleRule
--    evActivateRule (_rNumber applicationMetaRuleRule) 0
--    return ()

--testApplicationMetaRuleEx = (_rStatus $ head $ _rules testApplicationMetaRuleVote) == Active

-- vote rules                                |Expected result        |pos |neg |total                    |description of voting system
testVoteAssessOnVoteComplete1 = testVoteRule Active  $ voteGame      10 0 10 $ onRuleProposed $ voteWith_ majority $ assessWhenEverybodyVoted
testVoteAssessOnVoteComplete2 = testVoteRule Pending $ voteGame      9  0 10 $ onRuleProposed $ voteWith_ majority $ assessWhenEverybodyVoted
testVoteAssessOnEveryVote1   = testVoteRule Active  $ voteGame      10 0 10 $ onRuleProposed $ voteWith_ unanimity $ assessOnEveryVote
testVoteAssessOnEveryVote2   = testVoteRule Active  $ voteGame      6  0 10 $ onRuleProposed $ voteWith_ majority $ assessOnEveryVote
testVoteAssessOnEveryVote3   = testVoteRule Pending $ voteGame      5  0 10 $ onRuleProposed $ voteWith_ majority $ assessOnEveryVote
testVoteAssessOnEveryVote4   = testVoteRule Reject  $ voteGame      0  5 10 $ onRuleProposed $ voteWith_ majority $ assessOnEveryVote
testVoteMajorityWith          = testVoteRule Active  $ voteGame      6  0 10 $ onRuleProposed $ voteWith_ (majorityWith 50) $ assessOnEveryVote
testVoteNumberPositiveVotes   = testVoteRule Active  $ voteGame      3  7 10 $ onRuleProposed $ voteWith_ (numberVotes 3) $ assessOnEveryVote
testVoteWithQuorum1           = testVoteRule Active  $ voteGame      7  3 10 $ onRuleProposed $ voteWith_ (majority `withQuorum` 7) $ assessOnEveryVote
testVoteWithQuorum2           = testVoteRule Pending $ voteGame      6  0 10 $ onRuleProposed $ voteWith_ (majority `withQuorum` 7) $ assessOnEveryVote
testVoteAssessOnTimeLimit1    = testVoteRule Active  $ voteGameTimed 10 0 10 $ onRuleProposed $ voteWith_ unanimity $ assessOnTimeLimit date2
testVoteAssessOnTimeLimit2    = testVoteRule Active  $ voteGameTimed 1  0 10 $ onRuleProposed $ voteWith_ unanimity $ assessOnTimeLimit date2
testVoteAssessOnTimeLimit3    = testVoteRule Reject  $ voteGameTimed 1  0 10 $ onRuleProposed $ voteWith_ (unanimity `withQuorum` 5) $ assessOnTimeLimit date2
testVoteAssessOnTimeLimit4    = testVoteRule Reject $ voteGameTimed 0  0 10 $ onRuleProposed $ voteWith_ (unanimity `withQuorum` 1) $ assessOnTimeLimit date2
testVoteAssessOnTimeLimit5    = testVoteRule Pending $ voteGameTimed 10 0 10 $ onRuleProposed $ voteWith_ unanimity $ assessOnTimeLimit date3

testVoteRule s g = (_rStatus $ head $ _rules g) == s

isOutput :: String -> Game -> Bool
isOutput s g = any (\(Output _ _ mys SActive) -> mys == s) (_outputs g)


