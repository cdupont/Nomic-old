
module Test where

import Rule
import Expression
import Control.Monad

test :: RuleFunc
test = RuleFunc (\_ -> do
   NewVar "toto" 1
   a <- ReadVar "toto"
   case a of
    Just 1 -> OnEvent (Time 1) $ Output 1 "toto"
    Nothing -> const_ ()
   return $ a == a)

test2 :: RuleFunc
test2 = RuleFunc $ \r -> do
   --a <- (rRuleFunc r) test
   return True


