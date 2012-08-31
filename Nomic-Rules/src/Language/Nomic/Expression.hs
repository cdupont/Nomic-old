
{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, GADTs,
    UndecidableInstances, DeriveDataTypeable, FlexibleContexts,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies,
    TypeSynonymInstances, TemplateHaskell, ExistentialQuantification,
    TypeFamilies, ScopedTypeVariables, StandaloneDeriving, NamedFieldPuns #-}

-- | This module containt the type definitions necessary to build a Nomic rule. 
module Language.Nomic.Expression where

--import Happstack.State
import Data.Typeable
import Data.Ratio
import Control.Monad.State
import Data.List
import Control.Concurrent.STM
import Language.Haskell.Interpreter.Server
import Data.Time

type PlayerNumber = Int
type PlayerName = String
type RuleNumber = Int
type RuleName = String
type RuleText = String
type RuleCode = String
type EventNumber = Int
type VarName = String
type GameName = String
type Code = String

-- * Expression

-- | an Exp allows the player's rule to have access to the state of the game.
-- | it is a compositional algebra defined with a GADT.
data Exp a where
     NewVar     :: (Typeable a, Show a, Eq a) => VarName -> a -> Exp (Maybe (V a))
     DelVar     :: (V a) -> Exp Bool
     ReadVar    :: (Typeable a, Show a, Eq a) => (V a) -> Exp (Maybe a)
     WriteVar   :: (Typeable a, Show a, Eq a) => (V a) -> a -> Exp Bool
     OnEvent    :: (Event e) => e -> ((EventNumber, EventData e) -> Exp ()) -> Exp EventNumber
     DelEvent   :: EventNumber -> Exp Bool
     DelAllEvents:: (Event e) => e -> Exp ()
     SendMessage :: (Typeable a, Show a, Eq a) => Message a -> a -> Exp ()
     Output     :: PlayerNumber -> String -> Exp ()
     ProposeRule :: Rule -> Exp Bool
     ActivateRule :: RuleNumber -> Exp Bool
     RejectRule :: RuleNumber -> Exp Bool
     AddRule    :: Rule -> Exp Bool
     DelRule    :: RuleNumber -> Exp Bool
     ModifyRule :: RuleNumber -> Rule -> Exp Bool
     GetRules   :: Exp [Rule]
     SetVictory :: [PlayerNumber] -> Exp ()
     GetPlayers :: Exp [PlayerInfo]
     Const      :: a -> Exp a
     Bind       :: Exp a -> (a -> Exp b) -> Exp b
     CurrentTime:: Exp (UTCTime)
     deriving (Typeable)

instance Monad Exp where
   return = Const
   (>>=) = Bind




-- * Variables

-- | a container for a variable name and type
data V a = V String

-- | stores the variable's data
data Var = forall a . (Typeable a, Show a, Eq a) =>
        Var { vPlayerNumber :: Int,
              vName :: String,
              vData :: a}

instance Show Var where
    show (Var a b c) = (show a) ++ " " ++ (show b) ++ " " ++ (show c)

instance Eq Var where
    Var a b c == Var d e f = (a,b,c) === (d,e,f)

type Output = (PlayerNumber, String)


-- * Events

-- | Define the events and their related data
class (Eq e, Typeable e, Show e) => Event e where
    data EventData e

-- | Groups of events
data PlayerEvent = Arrive | Leave deriving (Typeable, Show, Eq)
data RuleEvent = Proposed | Activated | Rejected | Added | Modified | Deleted deriving (Typeable, Show, Eq)
data InputEvent = Choice | Numeric

-- | events types
data Player         = Player PlayerEvent deriving (Typeable, Show, Eq)
data Time           = Time UTCTime       deriving (Typeable, Show, Eq)
data EvRule         = EvRule RuleEvent   deriving (Typeable, Show, Eq)
data Message m      = Message String     deriving (Typeable, Show, Eq)
data Enum c => InputChoice c    = InputChoice PlayerNumber String    deriving (Typeable, Show, Eq)
data InputString    = InputString PlayerNumber String    deriving (Typeable, Show, Eq)
data Victory        = Victory            deriving (Typeable, Show, Eq)

-- | event instances
instance Event Player                                  where data EventData Player          = PlayerData {playerData :: PlayerInfo}
instance Event Time                                    where data EventData Time            = TimeData {timeData :: UTCTime}
instance Event EvRule                                  where data EventData EvRule          = RuleData {ruleData :: Rule}
instance (Typeable m) => Event (Message m)             where data EventData (Message m)     = MessageData {messageData :: m}
instance (Enum c, Typeable c) => Event (InputChoice c) where data EventData (InputChoice c) = InputChoiceData {inputChoiceData :: c}
instance Event InputString                             where data EventData (InputString)   = InputStringData {inputStringData :: String}
instance Event Victory                                 where data EventData Victory         = VictoryData {victoryData :: [PlayerInfo]}

instance (Event e) => Typeable (EventData e) where
    typeOf _  = mkTyConApp (mkTyCon( ("Expression.EventData (" ++ (show $ typeOf (undefined::e))) ++ ")" )) []

-- | event structure for storage
data EventHandler = forall e . (Event e) =>
     EH {eventNumber :: EventNumber,
         ruleNumber :: RuleNumber,
         event :: e,
         handler :: (EventNumber, EventData e) -> Exp ()} deriving Typeable

instance Show EventHandler where
    show (EH en rn e _) = (show en) ++ " " ++ (show rn) ++ " (" ++ (show e) ++")"

-- * Rule

-- | type of rule to assess the legality of a given parameter
type OneParamRule a = a -> Exp RuleResponse

-- | a rule can assess the legality either immediatly of later through a messsage
data RuleResponse = BoolResp {boolResp :: Bool}
                  | MsgResp  {msgResp :: Message Bool}

-- | type of rule that just mofify the game state
type NoParamRule = Exp ()

-- | the different types of rules
data RuleFunc =
      RuleRule   {ruleRule   :: OneParamRule Rule}
    | PlayerRule {playerRule :: OneParamRule PlayerInfo}
    | VoidRule   {voidRule   :: NoParamRule} deriving (Typeable)


instance Show RuleFunc where
    show _ = "RuleFunc"


-- | An informationnal structure about a rule:
data Rule = Rule { rNumber       :: RuleNumber,       -- number of the rule (must be unique) TO CHECK
                   rName         :: RuleName,         -- short name of the rule 
                   rDescription  :: String,           -- description of the rule
                   rProposedBy   :: PlayerNumber,     -- player proposing the rule
                   rRuleCode     :: Code,             -- code of the rule as a string
                   rRuleFunc     :: RuleFunc,         -- function representing the rule (interpreted from rRuleCode)
                   rStatus       :: RuleStatus,       -- status of the rule
                   rAssessedBy    :: Maybe RuleNumber} -- which rule accepted or rejected this rule
                   deriving (Typeable, Show)

instance Eq Rule where
    (Rule {rNumber=r1}) == (Rule {rNumber=r2}) = r1 == r2

instance Ord Rule where
     (Rule {rNumber=r1}) <= (Rule {rNumber=r2}) = r1 <= r2

-- | the status of a rule.
data RuleStatus = Active      -- Active rules forms the current Constitution
                | Pending     -- Proposed rules
                | Reject      -- Rejected rules
                deriving (Eq, Show, Typeable)

-- * Player

-- | informations on players
data PlayerInfo = PlayerInfo { playerNumber :: PlayerNumber,
                               playerName   :: String}
                               deriving (Eq, Typeable, Show)

-- * Game
           
-- | The state of the game:
data Game = Game { gameName      :: GameName,
                   rules         :: [Rule],
                   players       :: [PlayerInfo],
                   variables     :: [Var],
                   events        :: [EventHandler],
                   outputs       :: [Output],
                   victory       :: [PlayerNumber],
                   currentTime   :: UTCTime}
                   deriving (Typeable)

instance Show Game where
    show (Game { gameName, rules, players, variables, events, outputs, victory}) =
        "Game Name = " ++ (show gameName) ++ "\n Rules = " ++ (concat $ intersperse "\n " $ map show rules) ++ "\n Players = " ++ (show players) ++ "\n Variables = " ++
        (show variables) ++ "\n Events = " ++ (show events) ++ "\n Outputs = " ++ (show outputs) ++ "\n Victory = " ++ (show victory)


-- | an equality that tests also the types.
(===) :: (Typeable a, Typeable b, Eq b) => a -> b -> Bool
(===) x y = cast x == Just y



