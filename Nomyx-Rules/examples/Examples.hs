{-# LANGUAGE TupleSections, GADTs #-}

-- | This file gives a list of example rules that the players can submit.
--You can copy-paste them in the field "Code" of the web GUI.
--Don't hesitate to get inspiration from there and create your own rules!
module Examples where

import Language.Nomyx.Rule
import Language.Nomyx.Expression
import Data.Function
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import qualified Data.Time.Clock as T
import Data.Time.Recurrence hiding (filter)
import Control.Arrow
import Data.List
import Debug.Trace
import Control.Monad

-- | A rule that does nothing
nothing :: RuleFunc
nothing = VoidRule $ return ()

-- | A rule that says hello to all players
helloWorld :: RuleFunc
helloWorld = VoidRule $ outputAll "hello"

-- | account variable name
accounts :: String
accounts = "Accounts"

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
moneyTransfer :: RuleFunc
moneyTransfer = VoidRule $ do
    pls <- getAllPlayerNumbers
    when (length pls >= 2) $ mapM_ (selPlayer pls) pls where
       selPlayer pls src = onInputChoice_ "Transfer money to player: " (delete src $ sort pls) (selAmount src) src
       selAmount src dst = onInputStringOnce_ ("Select Amount to transfert to player: " ++ show dst) (transfer src dst) src
       transfer src dst amount = do
           modifyValueOfPlayer dst accounts (+ (read amount))
           modifyValueOfPlayer src accounts (\a -> a - (read amount))
           output ("You gave " ++ amount ++ " to " ++ show dst) src
           output (show src ++ " gaved you " ++ amount ++ " Ecus") dst


-- | delete a rule
delRule :: RuleNumber -> RuleFunc
delRule rn = VoidRule $ suppressRule rn >> return ()

-- | Change unanimity vote (usually rule 2) to absolute majority (half participants plus one)
voteWithMajority :: RuleFunc
voteWithMajority = VoidRule $ do
   suppressRule 2
   addRuleParams_ "vote with majority" (vote majority) "vote majority" 2 "meta-rule: return true if a majority of players vote positively for a new rule"
   activateRule_ 2
   autoDelete


-- | player pn is the king
makeKing :: PlayerNumber -> RuleFunc
makeKing pn = VoidRule $ newVar_ "King" pn >> return ()

king :: V PlayerNumber
king = (V "King")

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
    suppressRule 2
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

-- | will display the time to all players in 5 seconds
displayTime :: RuleFunc
displayTime = VoidRule $ do
    t <- getCurrentTime
    onEventOnce_ (Time $ T.addUTCTime 5 t) $ \(TimeData t) -> outputAll $ show t

-- | Only one player can achieve victory: No group victory.
-- Forbidding group victory usually becomes necessary when lowering the voting quorum:
-- a coalition of players could simply force a "victory" rule and win the game.
onePlayerVictory ::  RuleFunc
onePlayerVictory = VoidRule $ onEvent_ Victory $ \(VictoryData ps) -> when (length ps >1) $ setVictory []
