
-- | extensive testing.
-- to run all tests, run "allTests" and "testMultiX" 
module Test where

import Game
import Observable
import Rule
import Control.Monad.State
import Server
import System.IO
import Control.Concurrent.STM
import Control.Concurrent
import Multi 
import Data.Set
import Action
import Comm
import Test.QuickCheck
import Interpret
import Language.Haskell.Interpreter.Server
import NamedRule


test :: Eq a => (StateT s Comm a) -> s -> a -> IO Bool
test state g expected = do
   res <- runWithStdIO sHandle $ evalStateT state g
   return $ res == expected

   
-- Preliminaries
nr1 = NamedRule {rNumber=1, rName ="Rule1", rText="test de règle 1", rProposedBy=2, rule = r1, rStatus = Active, rejectedBy = Nothing}
nr2 = NamedRule {rNumber=2, rName ="Rule2", rText="test de règle 2", rProposedBy=2, rule = r2, rStatus = Active, rejectedBy = Nothing}
nr3 = NamedRule {rNumber=3, rName ="Rule3", rText="test de règle 3", rProposedBy=3, rule = r3, rStatus = Active, rejectedBy = Nothing}


rs = [nr1, nr2]

g = Game {gameName = "test", rules = rs, actionResults = []}

-- Test on Rules
defaultNRrule r = NamedRule { rNumber = 1,
                              rName = "",
                              rText = "",
                              rProposedBy = 0, 
                              rule = r,
                              rStatus = Active,
                              rejectedBy = Nothing}
                  
r1 = "MustBeEgalTo Legal"
r2 = "MustBeEgalTo (MustBeEgalTo Legal)"
r3 = "Legal"
r4 = "Illegal"
r5 = "TestRuleOver Legal"
r6 = "OfficialRule 1"           -- r6 == r1
r7 = "immutable 1"





rtest1 = test (isRuleLegal (defaultNRrule r1) nr1) g (Right False) --a program cannot contain a whole representation of itself.
rtest2 = test (isRuleLegal (defaultNRrule r2) nr1) g (Right True)  --r2 contains a representation of r1.
rtest3 = test (isRuleLegal (defaultNRrule (r1 ++ " `Rand` " ++ r1)) nr1) g (Right False)
rtest4 = test (isRuleLegal (defaultNRrule (r1 ++ "`Ror`" ++ r2)) nr1) g (Right True)
rtest5 = test (isRuleLegal (defaultNRrule r3) nr1) g (Right True)
rtest6 = test (isRuleLegal (defaultNRrule r4) nr1) g (Right False)
rtest7 = test (isRuleLegal (defaultNRrule r5) (defaultNRrule r1)) g (Right True)
rtest8 = test (isRuleLegal (defaultNRrule r6) (defaultNRrule r3)) g (Right True)
rtest9 = test (isRuleLegal (defaultNRrule r7) (defaultNRrule r4)) g (Right False)

ruleTestPassed = liftM and $ sequence [rtest1, rtest2, rtest3, rtest4, rtest5, rtest6, rtest7, rtest8, rtest9]


-- Some properties that rules must hold.

--  prop_autoContain r = do
-- 		a <- isRuleLegal p p
--                return $ not a
--   	where p = MustBeEgalTo r 

-- test on Observables

o1 = oRuleOfficial
o2 = oRuleProposedBy `oEqu` 1 `oAnd` oRuleOfficial
o3 = oRuleProposedBy `oEqu` 1 `oOr` oRuleOfficial
o4 = oRuleProposedBy `oEqu` 2 `oAnd` oNot oRuleOfficial
o5 = (oRuleProposedBy - 1) `oEqu` 2
o6 = oSelfNumber `oEqu` 1


test1 = test (evalObs o1 nr1 0) g (Right True)  --(nr1 is official)
test2 = test (evalObs o2 nr1 0) g (Right False) --(nr1 is official but it's not Player 1's turn)
test3 = test (evalObs o3 nr1 0) g (Right True)  --(nr1 is official or it's player 1 turn)
test4 = test (evalObs o4 nr1 0) g (Right False) --(nr1 is official and should not)
test5 = test (evalObs o5 nr1 0) g (Right False) --(nr1 is official and should not)
test6 = test (evalObs o1 nr3 0) g (Right False) --(nr1 is not official and should)
test7 = test (evalObs o6 nr3 1) g (Right True)

obsTestPassed = liftM and $ sequence [test1, test2, test3, test4, test5, test6, test7 ]

--testObs :: Obs a -> Either Actions a
--testObs o = test (evalObs o nr1 0) g

-- Test with combination of the 2

-- if this rule is officialized, Player 2 cannot play anymore
cr1 = "Cond (oRuleProposedBy `oEqu` 2) Illegal Legal" 

-- Only new rules are affected
cr2 = "Cond (oRuleOfficial) Legal Illegal" 

-- applicable only on turn 3
-- cr3 r = Cond (oTurn `oEqu` 3) r Legal

-- the total is that cr3 forbidden player 2 to play at turn 3.

crtest1 = test (isRuleLegal (defaultNRrule cr1) nr1) g (Right False) --it's Player 2 to play, cr1 forbids that
crtest2 = test (isRuleLegal (defaultNRrule cr2) nr1) g (Right True)  --nr1 is official (ie included in the current ruleset)
crtest3 = test (isRuleLegal (defaultNRrule cr2) nr3) g (Right False) --nr3 is not official

crTestPassed = liftM and $ sequence [crtest1, crtest2, crtest3]

--testRule :: String -> Either Actions Bool
--testRule r = test (isRuleLegal (defaultNRrule r) nr1) g

-- action test
g2 = Game {gameName = "test", rules = rs, actionResults = [Action 1 2 (Vote (Konst "Vote") (Konst 1)) (Just True)]}

ar1 = "Cond (Vote (Konst \"Vote\") (Konst 1) ) Legal Illegal"
atest1 = return $ findActionResult (Vote (Konst "Vote") (Konst 1))
                           NamedRule {rNumber=2, rName ="Rule1", rText="test de règle 1", rProposedBy=2, rule = ar1, rStatus = Active, rejectedBy = Nothing}
                           1
                           (actionResults g2) == Just (Action 1 2 (Vote (Konst "Vote") (Konst 1)) (Just True))

atest2 = test (evalObs (Vote (Konst "Vote") (Konst 1))
                 NamedRule {rNumber=2, rName ="Rule1", rText="test de règle 1", rProposedBy=2, rule = ar1, rStatus = Active, rejectedBy = Nothing}
                 1) g2 (Right True)

atest3 = test (isRuleLegal' (Cond (Vote (Konst "Vote") (Konst 1) ) Legal Illegal)
                     NamedRule {rNumber=2, rName ="Rule1", rText="test de règle 1", rProposedBy=2, rule = r1, rStatus = Pending, rejectedBy = Nothing}
                     1) g2 (Right True)

atest4 = test (isRuleLegal NamedRule {rNumber=1, rName ="Rule1", rText="test de règle 1", rProposedBy=2, rule = ar1, rStatus = Active, rejectedBy = Nothing}
                            NamedRule {rNumber=2, rName ="Rule1", rText="test de règle 1", rProposedBy=2, rule = r1, rStatus = Pending, rejectedBy = Nothing})
                     g2 (Right True)

actionTestPassed = liftM and $ sequence [atest1, atest2, atest3, atest4]

-- Other test


cnr1 = NamedRule {rNumber=1, rName ="Rule1", rText="test de règle 1", rProposedBy=1, rule = cr1, rStatus = Active, rejectedBy = Nothing}
cnr2 = NamedRule {rNumber=2, rName ="Rule2", rText="test de règle 2", rProposedBy=2, rule = cr2, rStatus = Active, rejectedBy = Nothing}

gs2 = Game {gameName="Jeu", rules = [cnr2], actionResults = []}



isRuleLegalToCurrentRuleSetTest1 = test (isLegal cnr2) gs2 (Right Nothing)

isRuleLegalToCurrentRuleSetTest2 = test (isLegal cnr1) gs2 (Right Nothing)

applyRuleToCurrentRuleSet :: IO Game
applyRuleToCurrentRuleSet = runWithStdIO sHandle $ execStateT (applyTo cnr1)  gs2 --empty the active ruleset because it's P2 turn
applyRuleToCurrentRuleSetTest = do g <- applyRuleToCurrentRuleSet
                                   return $ length ( activeRules g ) == 0

-- Visual test

playTest1 = runStateT (amend cnr1) gs2 -- the proposed rule is illegal (it is not allready official)
playTest2 = runStateT (amend cnr2) gs2 -- the proposed rule is legal (it is allready official). It is added to the ruleset. Its execution doesn't modify the ruleset.

voteTest = test (isRuleLegal (defaultNRrule ("voteRule 1")) nr1) g


-- monadic test in mono player

putChan gameChan h l = do
   putStrLn $ "-> " ++ l
   c <- atomically newTChan
   atomically $ writeTChan gameChan (h, c, l)
   threadDelay 10000
   putChar '\n'


noVote :: ServerState
noVote = do
            modify (\s @ Server { multi = m@Multi { games = g:gs}} ->
               s {multi = m{ games = g { rules = [NamedRule {rNumber=1, rName ="NoVote", rText="", rProposedBy=0, rule = "Legal", rStatus = Active, rejectedBy = Nothing},
                                                  NamedRule {rNumber=2, rName ="Immutable Rules", rText="The rule #1 must not be suppressed", rProposedBy=0, rule = "immutable 1", rStatus = Active, rejectedBy = Nothing}]}:gs }})



-- monadic test in multi player
testMulti1 :: IO Bool
testMulti1 = do
   gameChan <- atomically newTChan


   --opening a handle to simulate each player
   h1 <- openFile "/dev/stdout" ReadWriteMode
   h2 <- openFile "/dev/stdout" ReadWriteMode
   h3 <- openFile "/dev/stdout" ReadWriteMode
   let put1 = putChan gameChan h1
   let put2 = putChan gameChan h2
   let put3 = putChan gameChan h3
   sh <- sHandle
   debugChan <- atomically newTChan
   _ <- forkIO $ runMulti gameChan (return ()) debugChan

   put1 "newplayer"
   put1 "name j1"
   put1 "newgame g1"
   put1 "join g1"

   put1 "submitRule testRule3 testRuletext Legal"

   put2 "newplayer"
   put2 "name j2"
   put2 "join g1"
   put2 "submitRule testRule4 testRuletext \"eraseRule 3\""

   put1 "amendconstitution"
   put2 "submitRule testRule5 testRuletext Illegal"

   put1 "showallrules"

   put3 "newplayer"
   put3 "name j3"
   put3 "newgame g2"
   put3 "join g2"
   put3 "submitRule testRule3 testRuletext \"eraseRule 1\""

   put3 "showallrules"

   put1 "debug"

   s <- liftIO $ atomically $ readTChan debugChan
   return $ (show s) == (show $ endServer1 sh h1 h2 h3)

endServer1 :: ServerHandle -> Handle -> Handle -> Handle -> Server
endServer1 sh h1 h2 h3 = Server {
   multi = Multi {
      games = [Game { gameName = "g1",
                      rules = [nrVote,
                               nrImmutable,
                               NamedRule {rNumber=3, rName ="testRule3", rText="testRuletext", rProposedBy=1, rule = "Legal", rStatus = Pending, rejectedBy = Nothing},
                               NamedRule {rNumber=4, rName ="testRule4", rText="testRuletext", rProposedBy=2, rule = "eraseRule 3", rStatus = Pending, rejectedBy = Nothing},
                               NamedRule {rNumber=5, rName ="testRule5", rText="testRuletext", rProposedBy=2, rule = "Illegal", rStatus = Pending, rejectedBy = Nothing}],
                      actionResults = []},
               Game { gameName = "g2",
                      rules = [nrVote,
                               nrImmutable,
                               NamedRule {rNumber=3, rName ="testRule3", rText="testRuletext", rProposedBy=3, rule = "eraseRule 1", rStatus = Pending, rejectedBy = Nothing}],
                      actionResults = []}],
      players = [PlayerInfo { playerNumber = 1, playerName = Just "j1", inGame = Just "g1"},
                 PlayerInfo { playerNumber = 2, playerName = Just "j2", inGame = Just "g1"},
                 PlayerInfo { playerNumber = 3, playerName = Just "j3", inGame = Just "g2"}]},
   playerClients = fromList $ [PlayerClient { cplayerNumber = 1, handle = h1},
                               PlayerClient { cplayerNumber = 2, handle = h2},
                               PlayerClient { cplayerNumber = 3, handle = h3}],
   interpreterHandle = sh}



-- monadic test on actions
testMulti2 :: IO Bool
testMulti2 = do
   gameChan <- atomically newTChan

   --opening a handle to simulate each player
   h1 <- openFile "/dev/stdout" ReadWriteMode
   h2 <- openFile "/dev/stdout" ReadWriteMode
   let put1 = putChan gameChan h1
   let put2 = putChan gameChan h2
   sh <- sHandle
   debugChan <- atomically newTChan
   _ <- forkIO $ runMulti gameChan (return ()) debugChan

   put1 "newplayer"
   put1 "name j1"
   put1 "newgame g1"
   put1 "join g1"
   put1 "submitRule testRule3 testRuletext \"Legal\""
   --getLine
   put2 "newplayer"
   put2 "name j2"
   put2 "join g1"
   put2 "submitRule testRule4 testRuletext \"eraseRule 3\""
   --getLine
   put1 "showpendingactions"
   --getLine

   put1 "showmypendingactions"
   --getLine
   put1 "doaction 1 True"
   --getLine
   put1 "showcompletedactions"
   --getLine

   put1 "showpendingactions"
   --getLine

   put2 "doaction 1 True"
   put1 "amendconstitution"
   --getLine

   put1 "debug"

   let end =   Game { gameName = "g1",
                      rules = [nrVote,
                               nrImmutable,
                               NamedRule {rNumber=3, rName ="testRule3", rText="testRuletext", rProposedBy=1, rule = "Legal", rStatus = Active, rejectedBy = Nothing},
                               NamedRule {rNumber=4, rName ="testRule4", rText="testRuletext", rProposedBy=2, rule = "eraseRule 3", rStatus = Pending, rejectedBy = Nothing}],
                      actionResults = [Action 1 3 (Vote (Konst "Please vote") (Konst 2)) (Just True),
                                       Action 1 3 (Vote (Konst "Please vote") (Konst 1)) (Just True)]}

   s <- liftIO $ atomically $ readTChan debugChan
   putStrLn $ "Test result:" ++ show (head $ games $ multi s)
   return $ (show $ head $ games $ multi s) == (show $ end)


-- instances
instance Arbitrary Rule where
  arbitrary = sized (arbtree 0 maxkey)
           where maxkey  = 1000

arbtree :: Int -> Int -> Int -> Gen (Rule)
arbtree lo hi n
 | n <= 0        = elements [Legal, Illegal]
 | lo >= hi      = elements [Legal, Illegal]
 | otherwise     = do{ i  <- choose (lo,hi)
                    ; m  <- choose (1,30)
                     ; let (ml,mr)  | m==(1::Int)= (1,2)
                                    | m==2       = (2,1)
                                    | m==3       = (1,1)
                                    | otherwise  = (2,2)
                     ; l  <- arbtree lo (i-1) (n `div` ml)
                     ; r  <- arbtree (i+1) hi (n `div` mr)
                     ; return (Rand l r)
                     }

-- Gather all test

allTests = liftM and $ sequence [ruleTestPassed, obsTestPassed, crTestPassed, actionTestPassed,
                                applyRuleToCurrentRuleSetTest, testMulti1, testMulti2] -- must allways be True

