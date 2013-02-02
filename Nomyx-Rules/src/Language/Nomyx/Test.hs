{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, NamedFieldPuns, GADTs#-}

module Language.Nomyx.Test where

import Language.Nomyx.Rule
import Language.Nomyx.Expression
import Language.Nomyx.Evaluation
import Control.Monad
import Control.Monad.State.Lazy
import Data.Typeable
import Data.Time
import Data.Maybe (fromJust)

date1 = parse822Time "Tue, 02 Sep 1997 09:00:00 -0400"
date2 = parse822Time "Tue, 02 Sep 1997 10:00:00 -0400"

testGame = Game { gameName      = "test",
                  rules         = [],
                  players       = [PlayerInfo 1 "coco"],
                  variables     = [],
                  events        = [],
                  outputs       = [],
                  victory       = [],
                  currentTime   = date1}

testRule = Rule  { rNumber       = 0,
                   rName         = "test",
                   rDescription  = "test",
                   rProposedBy   = 0,
                   rRuleCode     = "",
                   rRuleFunc     = VoidRule $ return (),
                   rStatus       = Pending,
                   rAssessedBy   = Nothing}

evalRuleFunc (VoidRule f) = evalState (evalExp f 0) testGame
execRuleFuncEvent (VoidRule f) e d = execState (evalExp f 0 >> (triggerEvent e d)) testGame
execRuleFuncGame (VoidRule f) g = execState (evalExp f 0) g
execRuleFuncEventGame (VoidRule f) e d g = execState (evalExp f 0 >> (triggerEvent e d)) g
execRuleFunc f = execRuleFuncGame f testGame

tests = [testVarEx1, testVarEx2, testVarEx3, testVarEx4, testVarEx5, testSingleInputEx, testInputStringEx,
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

data Choice = Holland | Sarkozy deriving (Enum, Typeable, Show, Eq, Bounded)

--mkInputChoiceEnum_ :: forall a. (Enum a, Bounded a, Typeable a, Eq a,  Show a) => String -> a -> PlayerNumber -> (a -> Exp ()) -> Exp EventNumber


testSingleInput :: RuleFunc
testSingleInput = VoidRule $ do
    onInputChoiceEnum_ "Vote for Holland or Sarkozy" Holland h 1 where
        h a = output ("voted for " ++ (show a)) 1

testSingleInputEx = (outputs $ execRuleFuncEvent testSingleInput (inputChoiceEnum 1 "Vote for Holland or Sarkozy" Holland) (InputChoiceData Holland)) == [(1, "voted for Holland")]

testInputString :: RuleFunc
testInputString = VoidRule $ do
    onInputString_ "Enter a number:" h 1 where
        h a = output ("You entered: " ++ a) 1

testInputStringEx = (outputs $ execRuleFuncEvent testInputString (inputString 1 "Enter a number:") (InputStringData "1")) == [(1, "You entered: 1")]


testSendMessage :: RuleFunc
testSendMessage = VoidRule $ do
    let msg = Message "msg" :: Event(Message String)
    onEvent_ msg f
    sendMessage msg "toto" where
        f (MessageData a :: EventData(Message String)) = output a 1

testSendMessageEx = outputs (execRuleFunc testSendMessage) == [(1,"toto")]

testSendMessage2 :: RuleFunc
testSendMessage2 = VoidRule $ do
    onEvent_ (Message "msg":: Event(Message ())) f
    sendMessage_ (Message "msg") where
        f (MessageData a) = output "Received" 1

testSendMessageEx2 = outputs (execRuleFunc testSendMessage2) == [(1,"Received")]

data Choice2 = Me | You deriving (Enum, Typeable, Show, Eq, Bounded)

testUserInputWrite :: RuleFunc
testUserInputWrite = VoidRule $ do
    var <- newVar_ "vote" (Nothing::Maybe Choice2)
    onEvent_ (Message "voted" :: Event (Message ())) h2
    onEvent_ (InputChoice 1 "Vote for" [Me, You] Me) h1 where
        h1 (InputChoiceData a :: EventData (InputChoice Choice2)) = do
            writeVar (V "vote") (Just a)
            SendMessage (Message "voted") ()
        h2 (MessageData _) = do
            a <- readVar (V "vote")
            case a of
                Just (Just Me) -> output "voted Me" 1
                Nothing -> output "problem" 1

testUserInputWriteEx = (outputs $ execRuleFuncEvent testUserInputWrite (InputChoice 1 "Vote for" [Me, You] Me) (InputChoiceData Me)) == [(1,"voted Me")]

testActivateRule :: RuleFunc
testActivateRule = VoidRule $ do
    a <- GetRules
    if (rStatus (head a) == Pending) then do
        ActivateRule $ rNumber (head a)
        return ()
        else return ()


testActivateRuleEx = rStatus (head $ rules (execRuleFuncGame testActivateRule testGame {rules=[testRule]}))  == Active

testAutoActivateEx = rStatus (head $ rules (execRuleFuncEventGame autoActivate (RuleEv Proposed) (RuleData testRule) (testGame {rules=[testRule]})))  == Active

unanimityRule = testRule {rName = "unanimityRule", rRuleFunc = RuleRule $ voteWith unanimity, rNumber = 2, rStatus = Active}
applicationMetaRuleRule = testRule {rName = "onRuleProposedUseMetaRules", rRuleFunc = onRuleProposed checkWithMetarules, rNumber = 3, rStatus = Active}
gameUnanimity = testGame {rules=[unanimityRule]}

testUnanimityVote :: Game
testUnanimityVote = flip execState testGame $ do
    addPlayer (PlayerInfo 1 "coco")
    addPlayer (PlayerInfo 2 "jean paul")
    evAddRule unanimityRule
    evActivateRule (rNumber unanimityRule) 0
    evAddRule applicationMetaRuleRule
    evActivateRule (rNumber applicationMetaRuleRule) 0
    evProposeRule testRule
    evInputChoice (InputChoice 1 "Vote for rule 0" [For, Against] For) For
    evInputChoice (InputChoice 2 "Vote for rule 0" [For, Against] For) For

testUnanimityVoteEx = (rStatus $ head $ rules testUnanimityVote) == Active

testTimeEvent :: RuleFunc
testTimeEvent = VoidRule $ do
    onEvent_ (Time date1) f where
        f t = outputAll $ show date1

testTimeEventEx = (outputs $ execRuleFuncEvent testTimeEvent (Time date1) (TimeData date1)) == [(1,show date1)]

testTimeEvent2 :: Exp ()
testTimeEvent2 = schedule' [date1, date2] (outputAll . show)

testTimeEventEx2 = (outputs $ flip execState testGame (evalExp testTimeEvent2 0 >> gameEvs)) == [(1,show date2), (1,show date1)] where
    gameEvs = do
        evTriggerTime date1
        evTriggerTime date2

timedUnanimityRule = testRule {rName = "unanimityRule", rRuleFunc = voteWithTimeLimit unanimity date1, rNumber = 2, rStatus = Active}
gameTimedUnanimity = testGame {rules=[timedUnanimityRule]}
testTimedUnanimityVote :: Game
testTimedUnanimityVote = flip execState testGame $ do
    addPlayer (PlayerInfo 1 "coco")
    addPlayer (PlayerInfo 2 "jean paul")
    evAddRule timedUnanimityRule
    evActivateRule (rNumber timedUnanimityRule) 0
    evAddRule applicationMetaRuleRule
    evActivateRule (rNumber applicationMetaRuleRule) 0
    evProposeRule testRule
    evInputChoice (InputChoice 1 "Vote for rule 0" [For, Against] For) Against
    evTriggerTime date1

testTimedUnanimityVoteEx = (rStatus $ head $ rules testTimedUnanimityVote) == Reject

--    now <- Rule.getCurrentTime
--    let oneDay = 24 * 60 * 60 :: NominalDiffTime


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


