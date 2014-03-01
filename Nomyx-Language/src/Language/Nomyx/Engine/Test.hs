{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module Language.Nomyx.Engine.Test where

import Language.Nomyx.Expression
import Language.Nomyx.Variables
import Language.Nomyx.Rules
import Language.Nomyx.Events
import Language.Nomyx.Outputs
import Language.Nomyx.Inputs
import Language.Nomyx.Vote
import Language.Nomyx.Examples
import Language.Nomyx.Engine.Evaluation
import Language.Nomyx.Engine.Game
import Language.Nomyx.Engine.Utils
import Control.Monad.State
import Data.Typeable
import Data.Lens


date1 = parse822Time "Tue, 02 Sep 1997 09:00:00 -0400"
date2 = parse822Time "Tue, 02 Sep 1997 10:00:00 -0400"
date3 = parse822Time "Tue, 02 Sep 1997 11:00:00 -0400"

testGame = Game { _gameName      = "test",
                  _gameDesc      = GameDesc "test" "test",
                  _rules         = [],
                  _players       = [PlayerInfo 1 "coco" Nothing],
                  _variables     = [],
                  _events        = [],
                  _outputs       = [],
                  _victory       = Nothing,
                  _logs          = [],
                  _simu          = Nothing,
                  _currentTime   = date1}

testRule = Rule  { _rNumber       = 0,
                   _rName         = "test",
                   _rDescription  = "test",
                   _rProposedBy   = 0,
                   _rRuleCode     = "",
                   _rRuleFunc     = return Void,
                   _rStatus       = Pending,
                   _rAssessedBy   = Nothing}

evalRuleFunc f = evalState (runEvalError Nothing $ evalNomex f 0) testGame
execRuleFuncEvent f e d = execState (runEvalError Nothing $ evalNomex f 0 >> (triggerEvent_ e d)) testGame
execRuleFuncGame f g = execState (runEvalError Nothing $ void $ evalNomex f 0) g
execRuleFuncEventGame f e d g = execState (runEvalError Nothing $ evalNomex f 0 >> (triggerEvent_ e d)) g
execRuleFunc f = execRuleFuncGame f testGame

addActivateRule :: RuleFunc -> RuleNumber -> Evaluate ()
addActivateRule rf rn = do
   let rule = testRule {_rName = "testRule", _rRuleFunc = rf, _rNumber = rn, _rStatus = Pending}
   evAddRule rule
   evActivateRule (_rNumber rule) 0
   return ()

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
         ("test delete rule", testDeleteRuleEx1),
         ("test victory rule", testVictoryEx1),
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
testVar1 = ruleFunc $ do
   NewVar "toto" (1::Integer)
   return ()

testVarEx1 = (variables ^$ execRuleFunc testVar1) == [(Var 0 "toto" (1::Integer))]

--Test variable deleting
testVar2 :: RuleFunc
testVar2 = ruleFunc $ do
   var <- newVar_ "toto" (1::Int)
   delVar var
   return ()

testVarEx2 = _variables (execRuleFunc testVar2) == []

--Test variable reading
testVar3 :: RuleFunc
testVar3 = ruleFunc $ do
   var <- newVar_ "toto" (1::Int)
   a <- liftEffect $ readVar var
   case a of
      Just (1::Int) -> newOutput (Just 1) (return "ok")
      _ -> newOutput (Just 1) (return "nok")

testVarEx3 = isOutput "ok" (execRuleFunc testVar3)

--Test variable writing
testVar4 :: RuleFunc
testVar4 = ruleFunc $ do
   var <- newVar_ "toto" (1::Int)
   writeVar var (2::Int)
   a <- liftEffect $ readVar var
   case a of
      Just (2::Int) -> newOutput (Just 1) (return "ok")
      _ -> newOutput (Just 1) (return "nok")

testVarEx4 = isOutput "ok" (execRuleFunc testVar4)

--Test variable writing
testVar5 :: RuleFunc
testVar5 = ruleFunc $ do
   var <- newVar_ "toto" ([]::[Int])
   writeVar var ([1]::[Int])
   a <- liftEffect $ readVar var
   case a of
      Just (a::[Int]) -> void $ writeVar var (2:a)
      Nothing         -> void $ newOutput (Just 1) (return "nok")

testVarEx5 = _variables (execRuleFunc testVar5) == [(Var 0 "toto" ([2,1]::[Int]))]

data Choice = Holland | Sarkozy deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test input
testSingleInput :: RuleFunc
testSingleInput = ruleFunc $ do
    onInputRadio_ "Vote for Holland or Sarkozy" [Holland, Sarkozy] h 1 where
        h a = void $ newOutput (Just 1) (return $ "voted for " ++ (show a))

testSingleInputEx = isOutput "voted for Holland" g where
   g = execRuleFuncEvent testSingleInput (inputRadio 1 "Vote for Holland or Sarkozy" [Holland, Sarkozy] Holland) (InputData (RadioData Holland))

testMultipleInputs :: RuleFunc
testMultipleInputs = ruleFunc $ do
    onInputCheckbox_ "Vote for Holland and Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")] h 1 where
        h a = void $ newOutput (Just 1) (return $ "voted for " ++ (show a))

testMultipleInputsEx = isOutput "voted for [Holland,Sarkozy]" g where
   g = execRuleFuncEvent testMultipleInputs (inputCheckbox 1 "Vote for Holland and Sarkozy" [(Holland, "Holland"), (Sarkozy, "Sarkozy")]) (InputData (CheckboxData [Holland, Sarkozy]))


testInputString :: RuleFunc
testInputString = ruleFunc $ do
    onInputText_ "Enter a number:" h 1 where
        h a = void $ newOutput (Just 1) (return $ "You entered: " ++ a)

testInputStringEx = isOutput "You entered: 1" g where
   g = execRuleFuncEvent testInputString (inputText 1 "Enter a number:") (InputData (TextData "1"))

-- Test message
testSendMessage :: RuleFunc
testSendMessage = ruleFunc $ do
    let msg = Message "msg" :: Event(Message String)
    onEvent_ msg f
    sendMessage msg "toto" where
        f (MessageData a :: EventData(Message String)) = void $ newOutput (Just 1) (return a)

testSendMessageEx = isOutput "toto" (execRuleFunc testSendMessage)

testSendMessage2 :: RuleFunc
testSendMessage2 = ruleFunc $ do
    onEvent_ (Message "msg":: Event(Message ())) $ const $ void $ newOutput (Just 1) (return "Received")
    sendMessage_ (Message "msg")


testSendMessageEx2 = isOutput "Received" (execRuleFunc testSendMessage2)

data Choice2 = Me | You deriving (Enum, Typeable, Show, Eq, Bounded)

-- Test user input + variable read/write
testUserInputWrite :: RuleFunc
testUserInputWrite = ruleFunc $ do
    newVar_ "vote" (Nothing::Maybe Choice2)
    onEvent_ (Message "voted" :: Event (Message ())) h2
    onEvent_ (InputEv (Input 1 "Vote for" (Radio [(Me, "Me"), (You, "You")]))) h1 where
        h1 (InputData (RadioData a) :: EventData (Input Choice2)) = do
            writeVar (V "vote") (Just a)
            SendMessage (Message "voted") ()
        h1 _ = undefined
        h2 (MessageData _) = do
            a <- liftEffect $ readVar (V "vote")
            void $ case a of
                Just (Just Me) -> newOutput (Just 1) (return "voted Me")
                _ -> newOutput (Just 1) (return "problem")
        h2 _ = undefined

testUserInputWriteEx = isOutput "voted Me" g where
   g = execRuleFuncEvent testUserInputWrite (InputEv (Input 1 "Vote for" (Radio [(Me, "Me"), (You, "You")]))) (InputData (RadioData Me))

-- Test rule activation
testActivateRule :: RuleFunc
testActivateRule = ruleFunc $ do
    a <- liftEffect GetRules
    if (_rStatus (head a) == Pending) then do
        ActivateRule $ _rNumber (head a)
        return ()
        else return ()


testActivateRuleEx = _rStatus (head $ _rules (execRuleFuncGame testActivateRule testGame {_rules=[testRule]}))  == Active

testAutoActivateEx = _rStatus (head $ _rules (execRuleFuncEventGame autoActivate (RuleEv Proposed) (RuleData testRule) (testGame {_rules=[testRule]})))  == Active

--Time tests

testTimeEvent :: RuleFunc
testTimeEvent = ruleFunc $ do
    onEvent_ (Time date1) f where
        f _ = outputAll_ $ show date1

testTimeEventEx = isOutput (show date1) g where
   g = execRuleFuncEvent testTimeEvent (Time date1) (TimeData date1)

testTimeEvent2 :: Nomex ()
testTimeEvent2 = schedule' [date1, date2] (outputAll_ . show)

testTimeEventEx2 = isOutput (show date1) g && isOutput (show date2) g where
    g = flip execState testGame (runEvalError Nothing $ evalNomex testTimeEvent2 0 >> void gameEvs)
    gameEvs = do
        evTriggerTime date1
        evTriggerTime date2

-- Test deletes
testDeleteRule :: RuleFunc
testDeleteRule = ruleFunc $ do
    newVar_ "toto" (1::Int)
    onMessage (Message "msg":: Event(Message ())) (const $ return ())
    newOutput (Just 1) (return "toto")

testDeleteGame :: Game
testDeleteGame = flip execState testGame {_players = []} $ runEvalError Nothing $ do
  addActivateRule testDeleteRule 1
  addActivateRule (ruleFunc $ suppressRule 1) 2

testDeleteRuleEx1 = (_rStatus $ head $ drop 1 $ _rules testDeleteGame) == Reject &&
                    (_variables testDeleteGame == []) &&
                    (_oStatus $ head $ _outputs testDeleteGame) == SDeleted &&
                    (_evStatus $ head $ _events testDeleteGame) == SDeleted

-- Test victory
testVictoryGame :: Game
testVictoryGame = flip execState testGame $ runEvalError Nothing $ do
  addActivateRule (victoryXRules 1) 1
  addActivateRule (ruleFunc $ nothing) 2

testVictoryEx1 = (length $ getVictorious testVictoryGame) == 1

-- Test votes

voteGameActions :: Int -> Int -> Int  -> Bool -> Evaluate () -> Game
voteGameActions positives negatives total timeEvent actions = flip execState testGame {_players = []} $ runEvalError Nothing $ do
    mapM_ (\x -> addPlayer (PlayerInfo x ("coco " ++ (show x)) Nothing)) [1..total]
    actions
    evProposeRule testRule
    evs <- lift getChoiceEvents
    let pos = take positives evs
    let neg = take negatives $ drop positives evs
    mapM_ (\x -> triggerInput x (URadioData $ fromEnum For)) pos
    mapM_ (\x -> triggerInput x (URadioData $ fromEnum Against)) neg
    when timeEvent $ void $ evTriggerTime date2

voteGame' :: Int -> Int -> Int -> Bool -> RuleFunc -> Game
voteGame' positives negatives notVoted timeEvent rf  = voteGameActions positives negatives notVoted timeEvent $ addActivateRule rf 1

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
testVoteAssessOnEveryVote1    = testVoteRule Active  $ voteGame      10 0 10 $ onRuleProposed $ voteWith_ unanimity $ assessOnEveryVote
testVoteAssessOnEveryVote2    = testVoteRule Active  $ voteGame      6  0 10 $ onRuleProposed $ voteWith_ majority $ assessOnEveryVote
testVoteAssessOnEveryVote3    = testVoteRule Pending $ voteGame      5  0 10 $ onRuleProposed $ voteWith_ majority $ assessOnEveryVote
testVoteAssessOnEveryVote4    = testVoteRule Reject  $ voteGame      0  5 10 $ onRuleProposed $ voteWith_ majority $ assessOnEveryVote
testVoteMajorityWith          = testVoteRule Active  $ voteGame      6  0 10 $ onRuleProposed $ voteWith_ (majorityWith 50) $ assessOnEveryVote
testVoteNumberPositiveVotes   = testVoteRule Active  $ voteGame      3  7 10 $ onRuleProposed $ voteWith_ (numberVotes 3) $ assessOnEveryVote
testVoteWithQuorum1           = testVoteRule Active  $ voteGame      7  3 10 $ onRuleProposed $ voteWith_ (majority `withQuorum` 7) $ assessOnEveryVote
testVoteWithQuorum2           = testVoteRule Pending $ voteGame      6  0 10 $ onRuleProposed $ voteWith_ (majority `withQuorum` 7) $ assessOnEveryVote
testVoteAssessOnTimeLimit1    = testVoteRule Active  $ voteGameTimed 10 0 10 $ onRuleProposed $ voteWith_ unanimity $ assessOnTimeLimit date2
testVoteAssessOnTimeLimit2    = testVoteRule Active  $ voteGameTimed 1  0 10 $ onRuleProposed $ voteWith_ unanimity $ assessOnTimeLimit date2
testVoteAssessOnTimeLimit3    = testVoteRule Reject  $ voteGameTimed 1  0 10 $ onRuleProposed $ voteWith_ (unanimity `withQuorum` 5) $ assessOnTimeLimit date2
testVoteAssessOnTimeLimit4    = testVoteRule Reject $ voteGameTimed  0  0 10 $ onRuleProposed $ voteWith_ (unanimity `withQuorum` 1) $ assessOnTimeLimit date2
testVoteAssessOnTimeLimit5    = testVoteRule Pending $ voteGameTimed 10 0 10 $ onRuleProposed $ voteWith_ unanimity $ assessOnTimeLimit date3

testVoteRule s g = (_rStatus $ head $ _rules g) == s



