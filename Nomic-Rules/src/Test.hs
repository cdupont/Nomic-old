
module Test where

import Rule
import Expression
import Control.Monad
import Evaluation
import Control.Monad.State.Lazy

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

tests = [test1, test2, test3, test4, test5, test7, test8, test9, test10]
allTests = and tests

--Test variable creation
testVar1 :: RuleFunc
testVar1 = VoidRule $ do
   NewVar "toto" 1
   return ()

test1 = variables (execRuleFunc testVar1) == [(0, "toto", 1)]

--Test variable deleting
testVar2 :: RuleFunc
testVar2 = VoidRule $ do
   NewVar "toto" 1
   DelVar "toto"
   return ()

test2 = variables (execRuleFunc testVar2) == []

--Test variable reading
testVar3 :: RuleFunc
testVar3 = VoidRule $ do
   NewVar "toto" 1
   a <- ReadVar "toto"
   case a of
      Just 1 -> output "ok" 1
      Nothing -> output "nok" 1

test3 = outputs (execRuleFunc testVar3) == [(1,"ok")]

--Test variable writing
testVar4 :: RuleFunc
testVar4 = VoidRule $ do
   NewVar "toto" 1
   WriteVar "toto" 2
   a <- ReadVar "toto"
   case a of
      Just 2 -> output "ok" 1
      Nothing -> output "nok" 1

test4 = outputs (execRuleFunc testVar4) == [(1,"ok")]

testSingleInput :: RuleFunc
testSingleInput = VoidRule $ do
    OnEvent (InputChoice 1 "Vote for" ["Holland", "Sarkozy"]) (\(InputChoiceData a) -> output ("voted for " ++ (show a)) 1)

test5 = (outputs $ execRuleFuncEvent testSingleInput (InputChoice 1 "Vote for" ["Holland", "Sarkozy"]) (InputChoiceData 1)) == [(1,"voted for 1")]

--testUserEvent :: RuleFunc
--testUserEvent = makeNormalRule $ do
--    OnEvent (User 1 "Click") (\_ -> output "hello" 1)
--
--test6 = (outputs $ execRuleFuncEvent testUserEvent (UserEvent 1 "Click") Nothing) == [(1,"hello")]

testSendMessage :: RuleFunc
testSendMessage = VoidRule $ do
    OnEvent (Message "msg") (\(MessageData a) -> output a 1)
    SendMessage "msg" "toto"

test7 = outputs (execRuleFunc testSendMessage) == [(1,"toto")]
testUserInputWrite :: RuleFunc
testUserInputWrite = VoidRule $ do
    NewVar "vote" 1
    OnEvent (InputChoice  1 "Vote for" ["me", "you"]) h1
    OnEvent (Message "voted") h2 where
        h1 _ = do
            WriteVar "vote" 1
            SendMessage "voted" ""
        h2 _ = do
            a <- ReadVar "vote"
            case a of
                Just 1 -> output "voted 1" 1
                Nothing -> output "problem" 1

test8 = (outputs $ execRuleFuncEvent testUserInputWrite (InputChoice 1 "Vote for" ["me", "you"]) (InputChoiceData 1)) == [(1,"voted 1")]

testActivateRule :: RuleFunc
testActivateRule = VoidRule $ do
    a <- GetRules
    if (rStatus (head a) == Pending) then do
        ActivateRule $ rNumber (head a)
        return ()
        else return ()


test9 = rStatus (head $ rules (execRuleFuncGame testActivateRule testGame {rules=[testRule]}))  == Active

test10 = rStatus (head $ rules (execRuleFuncEventGame autoActivate RuleProposed (RuleProposedData testRule) (testGame {rules=[testRule]})))  == Active

