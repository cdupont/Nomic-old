
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


--Test variable creation/reading
testVar1 :: RuleFunc
testVar1 = RuleFunc (\_ -> do
   NewVar "toto" 1
   a <- ReadVar "toto"
   case a of
    Just 1 -> output "hello" 1
    Nothing -> const_ ()
   return $ a == a)

t = evalExp ((ruleFunc testVar1) Nothing) 0
t2 = execStateT t testGame
test1 = outputs t2 == ["hello"]

    --Just 1 -> OnEvent (Time 1) $ Output 1 "toto"
test2 :: RuleFunc
test2 = RuleFunc $ \r -> do
   --a <- (rRuleFunc r) test
   return True

testSingleInput :: RuleFunc
testSingleInput = makeNormalRule $ do
    s <- InputChoice (const_ "click here:") (const_ 1) (const_ ["click"])
    output "hello" 1
    --OnEvent (UserEvent 1 "Click") (output "hello" 1)

testUserEvent :: RuleFunc
testUserEvent = makeNormalRule $ do
    OnEvent (UserEvent 1 "Click") (output "hello" 1)
