
{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, GADTs,
    UndecidableInstances, DeriveDataTypeable, FlexibleContexts,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies,
    TypeSynonymInstances, TemplateHaskell, ExistentialQuantification,
    TypeFamilies, ScopedTypeVariables, StandaloneDeriving, NamedFieldPuns #-}

-- test
-- | This module defines an Obs, which are everything that can be observed by a player'r rules over the state of the game.
module Expression where

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

data PlayerInfo = PlayerInfo { playerNumber :: PlayerNumber,
                               playerName   :: String}
                               deriving (Eq, Typeable, Show)

type GameName = String
type Code = String

--a container for a variable name and type
data V a = V String

-- stores the variable's data
data Var = forall a . (Typeable a, Show a, Eq a) =>
        Var { vPlayerNumber :: Int,
              vName :: String,
              vData :: a}

instance Show Var where
    show (Var a b c) = (show a) ++ " " ++ (show b) ++ " " ++ (show c)

type Output = (PlayerNumber, String)

--Define the events and their related data
class (Eq e, Typeable e, Show e) => Event e where
    data EventData e

data PlayerEvent = Arrive | Leave deriving (Typeable, Show, Eq)
data RuleEvent = Proposed | Activated | Rejected | Added | Modified | Deleted deriving (Typeable, Show, Eq)
data InputEvent = Choice | Numeric

data Player         = Player PlayerEvent deriving (Typeable, Show, Eq)
data Time           = Time UTCTime       deriving (Typeable, Show, Eq)
data EvRule         = EvRule RuleEvent   deriving (Typeable, Show, Eq)
data Message m      = Message String     deriving (Typeable, Show, Eq)
data Enum c => InputChoice c    = InputChoice PlayerNumber String    deriving (Typeable, Show, Eq)
data InputString    = InputString PlayerNumber String    deriving (Typeable, Show, Eq)
data Victory        = Victory            deriving (Typeable, Show, Eq)

instance Event Player                                  where data EventData Player          = PlayerData {playerData :: PlayerInfo}
instance Event Time                                    where data EventData Time            = TimeData {timeData :: UTCTime}
instance Event EvRule                                  where data EventData EvRule          = RuleData {ruleData :: Rule}
instance (Typeable m) => Event (Message m)             where data EventData (Message m)     = MessageData {messageData :: m}
instance (Enum c, Typeable c) => Event (InputChoice c) where data EventData (InputChoice c) = InputChoiceData {inputChoiceData :: c}
instance Event InputString                             where data EventData (InputString)   = InputStringData {inputStringData :: String}
instance Event Victory                                 where data EventData Victory         = VictoryData {victoryData :: [PlayerInfo]}

instance (Event e) => Typeable (EventData e) where
    typeOf _  = mkTyConApp (mkTyCon( ("Expression.EventData (" ++ (show $ typeOf (undefined::e))) ++ ")" )) []

data EventHandler = forall e . (Event e) =>
     EH {eventNumber :: EventNumber,
         ruleNumber :: RuleNumber,
         event :: e,
         handler :: (EventNumber, EventData e) -> Exp ()} deriving Typeable

instance Show EventHandler where
    show (EH en rn e _) = (show en) ++ " " ++ (show rn) ++ " (" ++ (show e) ++")"
           
-- | The state of the game:
data Game = Game { gameName      :: GameName,
                   rules         :: [Rule],
                   players       :: [PlayerInfo],
                   variables     :: [Var],
                   events        :: [EventHandler],
                   outputs       :: [Output],
                   victory       :: [PlayerNumber]}
                   deriving (Typeable)

instance Show Game where
    show (Game { gameName, rules, players, variables, events, outputs, victory}) =
        "Game Name = " ++ (show gameName) ++ "\n Rules = " ++ (show rules) ++ "\n Players = " ++ (show players) ++ "\n Variables = " ++
        (show variables) ++ "\n Events = " ++ (show events) ++ "\n Outputs = " ++ (show outputs) ++ "\n Victory = " ++ (show victory)

-- type of rule to assess the legality of a given parameter
type OneParamRule a = a -> Exp RuleResponse

--a rule can assess the legality either immediatly of later through a messsage
data RuleResponse = BoolResp {boolResp :: Bool}
                  | MsgResp  {msgResp :: Message Bool}

-- type of rule that just mofify the game state
type NoParamRule = Exp ()

-- the different types of rules
data RuleFunc =
      RuleRule   {ruleRule   :: OneParamRule Rule}
    | PlayerRule {playerRule :: OneParamRule PlayerInfo}
    | VoidRule   {voidRule   :: NoParamRule}


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


-- | the status of a rule.
data RuleStatus = Active      -- Active rules forms the current Constitution
                | Pending     -- Proposed rules
                | Reject      -- Rejected rules
                deriving (Eq, Show, Typeable)

type ActionNumber = Int
type ActionResult = String

--ActionType contains the reason for an action, the player asked, and the list of choices.
data ActionType = ActionType { reason  :: String,
                               player  :: PlayerNumber,
                               choices :: [String] }
   deriving (Eq, Show, Typeable)

-- | an action is a part of a rule that needs a player's input.
data Action = Action { aRuleNumber :: RuleNumber,
                       action :: ActionType,
                       result :: Maybe ActionResult}
                       deriving (Eq, Show, Typeable)


-- | A data type to hide away communication functions.
data Communication = Communication {cin :: TChan String, cout :: TChan String, hserver :: ServerHandle}

-- | A State monad used to avoid passing all around a Handle on which performing IO.
-- Comm must be used in replacement for IO in return types.
type Comm = StateT Communication IO

-- | an Exp allows the player's rule to have access to the state of the game.
-- | it is a compositional algebra defined with a GADT.
data Exp a where
     NewVar     :: (Typeable a, Show a, Eq a) => VarName -> a -> Exp (Maybe (V a))
     DelVar     :: (V a) -> Exp Bool
     ReadVar    :: (Typeable a, Show a, Eq a) => (V a) -> Exp (Maybe a)
     WriteVar   :: (Typeable a, Show a, Eq a) => (V a) -> a -> Exp Bool
     OnEvent    :: (Event e) => e -> ((EventNumber, EventData e) -> Exp ()) -> Exp EventNumber
     DelEvent   :: EventNumber -> Exp Bool
     SendMessage :: (Typeable a, Show a, Eq a) => Message a -> a -> Exp ()
     Const      :: a -> Exp a
     Bind       :: Exp a -> (a -> Exp b) -> Exp b
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
     deriving (Typeable)

instance Monad Exp where
   return = Const
   (>>=) = Bind



--instance Version (Exp ())
--instance Serialize (Exp ()) where
--           getCopy = undefined --contain $ (Const ())
--           putCopy e = contain $ safePut (1::Int)

--instance Version RuleStatus
-- $(deriveSerialize ''RuleStatus)

instance Eq Var where
    Var a b c == Var d e f = (a,b,c) === (d,e,f)



-- | an equality that tests also the types.
(===) :: (Typeable a, Typeable b, Eq b) => a -> b -> Bool
(===) x y = cast x == Just y



