
module Test where

import Rule
import Expression

test :: RuleFunc
test = RuleFunc (\_ -> do
   newVar "toto" 1
   a <- readVar "toto"
   return $ a == a)

test2 :: RuleFunc
test2 = RuleFunc $ \r -> do
   --a <- (rRuleFunc r) test
   return True


