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

-- | A rule that does nothing
nothing :: RuleFunc
nothing = VoidRule $ return ()

-- | A rule that says hello to all players
helloWorld :: RuleFunc
helloWorld = VoidRule $ outputAll "hello"

-- | Create a bank account for each players
createBankAccount :: RuleFunc
createBankAccount = VoidRule $ createValueForEachPlayer "Account"

account :: V [(Int, Int)]
account = (V "Account") :: V [(Int, Int)]

date1 = parse822Time "Tue, 02 Sep 2012 10:00:00 -0400"

-- | each player wins X Ecu each day
winXEcuPerDay :: Int -> RuleFunc
winXEcuPerDay x = VoidRule $ schedule_ (starting date1 $ recur daily) $ modifyAllValues "Account" (+x)

-- | a player wins X Ecu if a rule proposed is accepted
winXEcuOnRuleAccepted :: Int -> RuleFunc
winXEcuOnRuleAccepted x = VoidRule $ onEvent_ (RuleEv Activated) $ \(RuleData rule) -> modifyValueOfPlayer (rProposedBy rule) "Account" (+x)

-- | a player can transfer money to another player
moneyTransfer :: RuleFunc
moneyTransfer = VoidRule $ do
    pls <- getAllPlayerNumbers
    mapM_ (selPlayer pls) pls where
       selPlayer pls src = onInputChoice_ "Tranfer money to player: " pls (selAmount src) src
       selAmount src dst = onInputStringOnce_ ("Select Amount to transfert to player: " ++ show dst) (transfer src dst) src
       transfer src dst amount = do
           trace (" src = " ++ show src ++ " dst = " ++ show dst) $ modifyValueOfPlayer src "Account" (+ (read amount))
           modifyValueOfPlayer dst "Account" (\a -> a - (read amount))
           output (amount ++ " transfered to " ++ show dst) src
           output (show src ++ " transfered you " ++ amount) dst


-- | delete a rule
delRule :: RuleNumber -> RuleFunc
delRule rn = VoidRule $ suppressRule rn >> return ()

-- | player pn is the king
makeKing :: Int -> RuleFunc
makeKing pn = VoidRule $ newVar_ "King" pn >> return ()

king :: V PlayerNumber
king = (V "King")

-- | Monarchy: only the king decides which rules to accept or reject
monarchy :: RuleFunc
monarchy = VoidRule $ onEvent_ (RuleEv Proposed) $ \(RuleData rule) -> do
    k <- readVar_ king
    onInputChoiceEnumOnce_ ("Accept or reject rule " ++ (show $ rNumber rule)) True (activateOrReject rule) k


-- | set the victory for players having more than X accepted rules
victoryXRules :: Int -> RuleFunc
victoryXRules x = VoidRule $ onEvent_ (RuleEv Activated) $ \_ -> do
    rs <- getActiveRules
    let counts = map (rProposedBy . head &&& length) $ groupBy ((==) `on` rProposedBy) rs
    setVictory $ map fst $ filter ((>= x) . snd) counts

-- | display the time to all players in 5 seconds
displayTime :: RuleFunc
displayTime = VoidRule $ do
    t <- getCurrentTime
    onEvent_ (Time (T.addUTCTime 5 t)) $ \(TimeData t) -> outputAll $ show t
