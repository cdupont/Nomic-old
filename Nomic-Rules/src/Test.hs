{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, NamedFieldPuns#-}

module Test where

import Rule
import Expression
import Control.Monad
import Evaluation
import Control.Monad.State.Lazy
import Data.Typeable
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import Data.Time
import Data.Maybe (fromJust)

testGame = Game { gameName      = "test",
                  rules         = [],
                  actionResults = [],
                  players       = [PlayerInfo 1 "coco"],
                  variables     = [],
                  events        = [],
                  outputs       = [],
                  victory       = []}

testRule = Rule  { rNumber       = 0,
                   rName         = "test", 
                   rDescription  = "test",
                   rProposedBy   = 0,
                   rRuleCode     = "makeNormalRule $ return ()",
                   rRuleFunc     = VoidRule $ return (),
                   rStatus       = Pending,
                   rejectedBy    = Nothing}

evalRuleFunc (VoidRule f) = evalState (evalExp f 0) testGame
execRuleFuncEvent (VoidRule f) e d = execState (evalExp f 0 >> (triggerEvent e d)) testGame
execRuleFuncGame (VoidRule f) g = execState (evalExp f 0) g
execRuleFuncEventGame (VoidRule f) e d g = execState (evalExp f 0 >> (triggerEvent e d)) g
execRuleFunc f = execRuleFuncGame f testGame

tests = [testVarEx1, testVarEx2, testVarEx3, testVarEx4, testVarEx5, testSingleInputEx,
    testSendMessageEx, testSendMessageEx2, testUserInputWriteEx, testActivateRuleEx,
    testAutoActivateEx, testUnanimityVoteEx, testTimeEventEx, testTimeEventEx2]
allTests = and $ tests

--Test variable creation
testVar1 :: RuleFunc
testVar1 = VoidRule $ do
   NewVar "toto" (1::Integer)
   return ()

testVarEx1 = variables (execRuleFunc testVar1) == [(Var 0 "toto" (1::Integer))]

--Test variable deleting
testVar2 :: RuleFunc
testVar2 = VoidRule $ do
   var <- newVar_ "toto" (1::Int)
   delVar var
   return ()

testVarEx2 = variables (execRuleFunc testVar2) == []

--Test variable reading
testVar3 :: RuleFunc
testVar3 = VoidRule $ do
   var <- newVar_ "toto" (1::Int)
   a <- readVar var
   case a of
      Just (1::Int) -> output "ok" 1
      Nothing -> output "nok" 1

testVarEx3 = outputs (execRuleFunc testVar3) == [(1,"ok")]

--Test variable writing
testVar4 :: RuleFunc
testVar4 = VoidRule $ do
   var <- newVar_ "toto" (1::Int)
   writeVar var (2::Int)
   a <- readVar var
   case a of
      Just (2::Int) -> output "ok" 1
      Nothing -> output "nok" 1

testVarEx4 = outputs (execRuleFunc testVar4) == [(1,"ok")]

--Test variable writing
testVar5 :: RuleFunc
testVar5 = VoidRule $ do
   var <- newVar_ "toto" ([]::[Int])
   writeVar var ([1]::[Int])
   a <- readVar var
   case a of
      Just (a::[Int]) -> do
         writeVar var (2:a)
         return ()
      Nothing -> output "nok" 1

testVarEx5 = variables (execRuleFunc testVar5) == [(Var 0 "toto" ([2,1]::[Int]))]

data Choice = Holland | Sarkozy deriving (Enum, Typeable, Show, Eq)

testSingleInput :: RuleFunc
testSingleInput = VoidRule $ do
    onEvent_ (InputChoice 1 "Vote for Holland or Sarkozy") h where
        h (InputChoiceData (a::Choice)) = output ("voted for " ++ (show a)) 1

testSingleInputEx = (outputs $ execRuleFuncEvent testSingleInput (InputChoice 1 "Vote for Holland or Sarkozy") (InputChoiceData Holland)) == [(1, "voted for Holland")]

testSendMessage :: RuleFunc
testSendMessage = VoidRule $ do
    onEvent_ (Message "msg") f
    SendMessage (Message "msg") "toto" where
        f (MessageData a) = output a 1

testSendMessageEx = outputs (execRuleFunc testSendMessage) == [(1,"toto")]

testSendMessage2 :: RuleFunc
testSendMessage2 = VoidRule $ do
    onEvent_ (Message "msg") f
    sendMessage_ (Message "msg") where
        f (MessageData a) = output a 1

testSendMessageEx2 = outputs (execRuleFunc testSendMessage) == [(1,"toto")]

data Choice2 = Me | You deriving (Enum, Typeable, Show, Eq)

testUserInputWrite :: RuleFunc
testUserInputWrite = VoidRule $ do
    var <- newVar_ "vote" (Nothing::Maybe Choice2)
    onEvent_ (Message "voted") h2
    onEvent_ (InputChoice  1 "Vote for") h1 where
        h1 (InputChoiceData (a::Choice2)) = do
            writeVar (V "vote") (Just a)
            SendMessage (Message "voted") ()
        h2 (MessageData ()) = do
            a <- readVar (V "vote")
            case a of
                Just (Just Me) -> output "voted Me" 1
                Nothing -> output "problem" 1

testUserInputWriteEx = (outputs $ execRuleFuncEvent testUserInputWrite (InputChoice 1 "Vote for") (InputChoiceData Me)) == [(1,"voted Me")]

testActivateRule :: RuleFunc
testActivateRule = VoidRule $ do
    a <- GetRules
    if (rStatus (head a) == Pending) then do
        ActivateRule $ rNumber (head a)
        return ()
        else return ()


testActivateRuleEx = rStatus (head $ rules (execRuleFuncGame testActivateRule testGame {rules=[testRule]}))  == Active

testAutoActivateEx = rStatus (head $ rules (execRuleFuncEventGame autoActivate RuleProposed (RuleProposedData testRule) (testGame {rules=[testRule]})))  == Active

unanimityRule = testRule {rName = "unanimityRule", rRuleFunc = unanimityVote, rNumber = 2, rStatus = Active}
gameUnanimity = testGame {rules=[unanimityRule]}

testUnanimityVote :: Game
testUnanimityVote = flip execState testGame $ do
    addPlayer (PlayerInfo 1 "coco")
    addPlayer (PlayerInfo 2 "jean paul")
    evAddRule unanimityRule
    evActivateRule (rNumber unanimityRule) 0
    evProposeRule testRule
    evInputChoice (InputChoice 1 "Vote for rule 0") For
    evInputChoice (InputChoice 2 "Vote for rule 0") For

testUnanimityVoteEx = (rStatus $ head $ rules testUnanimityVote) == Active

parse822Time :: String -> UTCTime
parse822Time = zonedTimeToUTC
              . fromJust
              . parseTime defaultTimeLocale rfc822DateFormat

date1 = parse822Time "Tue, 02 Sep 1997 09:00:00 -0400"
date2 = parse822Time "Tue, 02 Sep 1997 10:00:00 -0400"

testTimeEvent :: RuleFunc
testTimeEvent = VoidRule $ do
    onEvent_ (Time date1) f where
        f t = outputAll $ show date1

testTimeEventEx = (outputs $ execRuleFuncEvent testTimeEvent (Time date1) (TimeData date1)) == [(1,show date1)]

testTimeEvent2 :: Exp ()
testTimeEvent2 = do
    schedule [date1, date2] (outputAll . show)

testTimeEventEx2 = (outputs $ flip execState testGame (evalExp testTimeEvent2 0 >> gameEvs)) == [(1,show date2), (1,show date1)] where
    gameEvs = do
        evTriggerTime date1
        evTriggerTime date2

{-autoMetarulesR = testRule {rName = "autoMetaRules", rRuleFunc = autoMetarules, rNumber = 2, rStatus = Active}
gameautoMetarules = testGame {rules=[autoMetarulesR]}

testAutoMetarules :: Game
testAutoMetarules = flip execState testGame $ do
    evAddRule unanimityRule
    evActivateRule (rNumber unanimityRule) 0
    evProposeRule testRule
    evInputChoice (InputChoice 1 "Vote for rule test") For
    evInputChoice (InputChoice 2 "Vote for rule test") For

testAutoMetarulesEx = (rStatus $ head $ rules testUnanimityVote) == Active
-}


