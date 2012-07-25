{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, NamedFieldPuns#-}

module Test where

import Rule
import Expression
import Control.Monad
import Evaluation
import Control.Monad.State.Lazy
import Data.Typeable

testGame = Game { gameName      = "test",
                  rules         = [],
                  actionResults = [],
                  players       = [],
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

varTests = [test1, test2, test3, test4, testVarExec5]
otherTests = [test5, test7, test8, test9, test10]
allTests = and $ varTests ++ otherTests

--Test variable creation
testVar1 :: RuleFunc
testVar1 = VoidRule $ do
   NewVar "toto" (1::Integer)
   return ()

test1 = variables (execRuleFunc testVar1) == [(Var 0 "toto" (1::Integer))]

--Test variable deleting
testVar2 :: RuleFunc
testVar2 = VoidRule $ do
   var <- newVar_ "toto" (1::Int)
   delVar var
   return ()

test2 = variables (execRuleFunc testVar2) == []

--Test variable reading
testVar3 :: RuleFunc
testVar3 = VoidRule $ do
   var <- newVar_ "toto" (1::Int)
   a <- readVar var
   case a of
      Just (1::Int) -> output "ok" 1
      Nothing -> output "nok" 1

test3 = outputs (execRuleFunc testVar3) == [(1,"ok")]

--Test variable writing
testVar4 :: RuleFunc
testVar4 = VoidRule $ do
   var <- newVar_ "toto" (1::Int)
   writeVar var (2::Int)
   a <- readVar var
   case a of
      Just (2::Int) -> output "ok" 1
      Nothing -> output "nok" 1

test4 = outputs (execRuleFunc testVar4) == [(1,"ok")]

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

testVarExec5 = variables (execRuleFunc testVar5) == [(Var 0 "toto" ([2,1]::[Int]))]

data Choice = Holland | Sarkozy deriving (Enum, Typeable, Show, Eq)

testSingleInput :: RuleFunc
testSingleInput = VoidRule $ do
    onEvent_ (InputChoice 1 "Vote for Holland or Sarkozy") (\(InputChoiceData (a::Choice)) -> output ("voted for " ++ (show a)) 1)

test5 = (outputs $ execRuleFuncEvent testSingleInput (InputChoice 1 "Vote for Holland or Sarkozy") (InputChoiceData Holland)) == [(1, "voted for Holland")]

testSendMessage :: RuleFunc
testSendMessage = VoidRule $ do
    onEvent_ (Message "msg") f
    SendMessage "msg" "toto" where
        f (MessageData a) = output a 1

test7 = outputs (execRuleFunc testSendMessage) == [(1,"toto")]

data Choice2 = Me | You deriving (Enum, Typeable, Show, Eq)

testUserInputWrite :: RuleFunc
testUserInputWrite = VoidRule $ do
    var <- newVar_ "vote" (Nothing::Maybe Choice2)
    onEvent_ (Message "voted") h2
    onEvent_ (InputChoice  1 "Vote for") h1 where
        h1 (InputChoiceData (a::Choice2)) = do
            writeVar (V "vote") (Just a)
            SendMessage "voted" ()
        h2 (MessageData ()) = do
            a <- readVar (V "vote")
            case a of
                Just (Just Me) -> output "voted Me" 1
                Nothing -> output "problem" 1

test8 = (outputs $ execRuleFuncEvent testUserInputWrite (InputChoice 1 "Vote for") (InputChoiceData Me)) == [(1,"voted 1")]

testActivateRule :: RuleFunc
testActivateRule = VoidRule $ do
    a <- GetRules
    if (rStatus (head a) == Pending) then do
        ActivateRule $ rNumber (head a)
        return ()
        else return ()


test9 = rStatus (head $ rules (execRuleFuncGame testActivateRule testGame {rules=[testRule]}))  == Active

test10 = rStatus (head $ rules (execRuleFuncEventGame autoActivate RuleProposed (RuleProposedData testRule) (testGame {rules=[testRule]})))  == Active

unanimityRule = testRule {rName = "unanimityRule", rRuleFunc = unanimityVote, rNumber = 2, rStatus = Active}
gameUnanimity = testGame {rules=[unanimityRule]}

testUnanimityVote :: Game
testUnanimityVote = flip execState testGame $ do
    addPlayer (PlayerInfo 1 "coco")
    evAddRule unanimityRule
    evActivateRule (rNumber unanimityRule) 0
    evProposeRule testRule
    evInputChoice ((InputChoice 1 "Vote for rule test") :: InputChoice ForAgainst) For
