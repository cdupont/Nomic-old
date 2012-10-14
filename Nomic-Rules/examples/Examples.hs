{-# LANGUAGE TupleSections, GADTs #-}

--This file gives a list of example rules that the players can submit.
--Don't hesitate to get inspiration from there and create your own!
module Examples where

import Language.Nomic.Rule
import Language.Nomic.Expression
import Data.Function
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import Data.Time
import Data.Time.Recurrence hiding (filter)
import Control.Arrow
import Data.List


--A rule that does nothing
nothing :: RuleFunc
nothing = VoidRule $ return ()

--A rule that says hello to every player
helloWorld :: RuleFunc
helloWorld = VoidRule $ getAllPlayerNumbers >>= mapM_ (output "hello")

--create a bank account for each players
createBankAccount :: RuleFunc
createBankAccount = VoidRule $ createValueForEachPlayer "Account"

account :: V [(Int, Int)]
account = (V "Account") :: V [(Int, Int)]

date1 = parse822Time "Tue, 02 Sep 2012 10:00:00 -0400"

--each player wins X Ecu each day
winXEcuPerDay :: Int -> RuleFunc
winXEcuPerDay x = VoidRule $ schedule_ (starting date1 $ recur daily) $ modifyAllValues "Account" (+x)

--a player wins X Ecu if a rule proposed is accepted
winXEcuOnRuleAccepted :: Int -> RuleFunc
winXEcuOnRuleAccepted x = VoidRule $ onEvent_ (RuleEv Activated) $ \(RuleData rule) -> modifyValueOfPlayer (rProposedBy rule) "Account" (+x)

--player pn is the king
makeKing :: Int -> RuleFunc
makeKing pn = VoidRule $ newVar_ "King" pn >> return ()

king :: V PlayerNumber
king = (V "King")

--Monarchy: only the king decides which rules to accept or reject
monarchy :: RuleFunc
monarchy = VoidRule $ onEvent_ (RuleEv Proposed) $ \(RuleData rule) -> do
    k <- readVar_ king
    onInputChoiceEnum_ ("Accept or reject rule " ++ (show $ rNumber rule)) True (activateOrReject rule) k


--set the victory for players having more than X accepted rules
victoryXRules :: Int -> RuleFunc
victoryXRules x = VoidRule $ do
    pns <- getAllPlayerNumbers
    rs <- getActiveRules
    let counts = map (\pn -> (pn, countForPlayer pn rs)) pns
    let winners = map fst $ filter ((>= x) . snd) counts
    setVictory winners where
        countForPlayer pn rs = length $ filter ((==pn) . rProposedBy) rs


victoryXRules' :: Int -> RuleFunc
victoryXRules' x = VoidRule $ do
    rs <- getActiveRules
    let counts = map (rProposedBy . head &&& length) $ groupBy ((==) `on` rProposedBy) rs
    setVictory $ map fst $ filter ((>= x) . snd) counts
