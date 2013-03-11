{-# LANGUAGE TupleSections, GADTs #-}

-- | This file gives a list of example rules that the players can submit.
--You can copy-paste them in the field "Code" of the web GUI.
--Don't hesitate to get inspiration from there and create your own rules!
module Language.Nomyx.Examples(nothing, helloWorld, accounts, createBankAccount, winXEcuPerDay, winXEcuOnRuleAccepted, moneyTransfer,
    delRule, voteWithMajority, king, makeKing, monarchy, revolution, victoryXRules, victoryXEcu, displayTime, noGroupVictory, iWin,
    module Data.Time.Recurrence, module Control.Monad, module Data.List, module Data.Time.Clock) where

import Language.Nomyx.Definition
import Language.Nomyx.Rule
import Language.Nomyx.Expression
import Data.Function
import Data.Time.Clock hiding (getCurrentTime)
import Data.Time.Recurrence hiding (filter)
import Control.Arrow
import Data.List
import Control.Monad

-- | A rule that does nothing
nothing :: RuleFunc
nothing = VoidRule $ return ()

-- | A rule that says hello to all players
helloWorld :: RuleFunc
helloWorld = VoidRule $ outputAll "hello, world!"

-- | account variable name and type
accounts :: V [(PlayerNumber, Int)]
accounts = V "Accounts"

-- | Create a bank account for each players
createBankAccount :: RuleFunc
createBankAccount = VoidRule $ createValueForEachPlayer_ accounts

-- | each player wins X Ecu each day
-- you can also try with "minutly" or "monthly" instead of "daily" and everything in the "time-recurrence" package
winXEcuPerDay :: Int -> RuleFunc
winXEcuPerDay x = VoidRule $ schedule_ (recur daily) $ modifyAllValues accounts (+x)

-- | a player wins X Ecu if a rule proposed is accepted
winXEcuOnRuleAccepted :: Int -> RuleFunc
winXEcuOnRuleAccepted x = VoidRule $ onEvent_ (RuleEv Activated) $ \(RuleData rule) -> modifyValueOfPlayer (rProposedBy rule) accounts (+x)

-- | a player can transfer money to another player
-- it does not accept new players or check if balance is positive, to keep the example simple
moneyTransfer :: RuleFunc
moneyTransfer = VoidRule $ do
    pls <- getAllPlayerNumbers
    when (length pls >= 2) $ forEachPlayer_ (selPlayer pls) where
       selPlayer pls src = onInputChoice_ "Transfer money to player: " (delete src $ sort pls) (selAmount src) src
       selAmount src dst = onInputStringOnce_ ("Select Amount to transfert to player: " ++ show dst) (transfer src dst) src
       transfer src dst amount = do
           modifyValueOfPlayer dst accounts (+ (read amount))
           modifyValueOfPlayer src accounts (\a -> a - (read amount))
           output ("You gave " ++ amount ++ " to " ++ show dst) src
           output (show src ++ " gaved you " ++ amount ++ " Ecus") dst


-- | delete a rule
delRule :: RuleNumber -> RuleFunc
delRule rn = VoidRule $ suppressRule rn >> autoDelete

-- | player pn is the king
makeKing :: PlayerNumber -> RuleFunc
makeKing pn = VoidRule $ newVar_ "King" pn >> return ()

king :: V PlayerNumber
king = V "King"

-- | Monarchy: only the king decides which rules to accept or reject
monarchy :: RuleFunc
monarchy = VoidRule $ onEvent_ (RuleEv Proposed) $ \(RuleData rule) -> do
    k <- readVar_ king
    onInputChoiceEnumOnce_ ("Your Royal Highness, do you accept rule " ++ (show $ rNumber rule) ++ "?") True (activateOrReject rule) k


-- | Revolution! Hail to the king!
-- This rule suppresses the democracy (usually rules 1 and 2), installs the king and activates monarchy.
revolution :: PlayerNumber -> RuleFunc
revolution player = VoidRule $ do
    suppressRule 1
    voidRule $ makeKing player
    addRuleParams_ "Monarchy" monarchy "monarchy" 1 "Monarchy: only the king can vote on new rules"
    activateRule_ 1
    --autoDelete

-- | set the victory for players having more than X accepted rules
victoryXRules :: Int -> RuleFunc
victoryXRules x = VoidRule $ onEvent_ (RuleEv Activated) $ \_ -> do
    rs <- getActiveRules
    let counts = map (rProposedBy . head &&& length) $ groupBy ((==) `on` rProposedBy) rs
    let victorious = map fst $ filter ((>= x) . snd) counts
    when (length victorious /= 0) $ setVictory victorious

victoryXEcu :: Int -> RuleFunc
victoryXEcu x = VoidRule $ onEvent_ (RuleEv Activated) $ \_ -> do
    as <- readVar_ accounts
    let victorious = map fst $ filter ((>= x) . snd) as
    if (length victorious /= 0) then setVictory victorious else return ()

-- | will display the time to all players in 5 seconds
displayTime :: RuleFunc
displayTime = VoidRule $ do
    t <- getCurrentTime
    onEventOnce_ (Time $ addUTCTime 5 t) $ \(TimeData t) -> outputAll $ show t

-- | Only one player can achieve victory: No group victory.
-- Forbidding group victory usually becomes necessary when lowering the voting quorum:
-- a coalition of players could simply force a "victory" rule and win the game.
noGroupVictory ::  RuleFunc
noGroupVictory = VoidRule $ onEvent_ Victory $ \(VictoryData ps) -> when (length ps >1) $ setVictory []

-- | Rule that state that you win. Good luck on having this accepted by other players ;)
iWin :: RuleFunc
iWin = VoidRule $ getSelfProposedByPlayer >>= giveVictory


-- a majority vote,
voteWithMajority :: RuleFunc
voteWithMajority = onRuleProposed $ voteWith (majority `withQuorum` 2) $ assessOnEveryVotes >> assessOnTimeLimit oneDay

-- | Change unanimity vote (usually rule 1) to absolute majority (half participants plus one)
returnToDemocracy :: RuleFunc
returnToDemocracy = VoidRule $ do
   suppressRule 1
   addRuleParams_ "vote with majority" voteWithMajority "voteTimeLimit" 1 "meta-rule: return true if a majority of players vote positively for a new rule"
   activateRule_ 1
   autoDelete


