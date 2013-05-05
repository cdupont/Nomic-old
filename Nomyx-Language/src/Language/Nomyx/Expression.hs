
{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, GADTs,
    UndecidableInstances, DeriveDataTypeable, FlexibleContexts,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies,
    TypeSynonymInstances, TemplateHaskell, ExistentialQuantification,
    TypeFamilies, ScopedTypeVariables, StandaloneDeriving, NamedFieldPuns,
    EmptyDataDecls, QuasiQuotes #-}

-- | This module containt the type definitions necessary to build a Nomic rule. 
module Language.Nomyx.Expression where

import Data.Typeable
import Data.List
import Data.Time
import Control.Applicative hiding (Const)
import Data.Lens.Template
import Data.Lens.Common
import Data.Boolean
import Debug.Trace.Helpers (traceM)
import Data.Data (Data)

type PlayerNumber = Int
type PlayerName = String
type RuleNumber = Int
type RuleName = String
type RuleText = String
type RuleCode = String
type EventNumber = Int
type EventName = String
type VarName = String
type GameName = String
type Code = String

-- * Nomyx Expression

-- | A Nomex (Nomyx Expression) allows the players to write rules.
-- | within the rules, you can access and modify the state of the game.
-- | It is a compositional algebra defined with a GADT.
data Nomex a where
   NewVar       :: (Typeable a, Show a, Eq a) => VarName           -> a              -> Nomex (Maybe (V a))
   ReadVar      :: (Typeable a, Show a, Eq a) => (V a)             -> Nomex (Maybe a)
   WriteVar     :: (Typeable a, Show a, Eq a) => (V a)             -> a              -> Nomex Bool
   DelVar       ::                               (V a)             -> Nomex Bool
   OnEvent      :: (Typeable e, Show e, Eq e) => Event e           -> ((EventNumber, EventData e) -> Nomex ()) -> Nomex EventNumber
   DelEvent     ::                               EventNumber       -> Nomex Bool
   DelAllEvents :: (Typeable e, Show e, Eq e) => Event e           -> Nomex ()
   SendMessage  :: (Typeable a, Show a, Eq a) => Event (Message a) -> a              -> Nomex ()
   Output       ::                               PlayerNumber      -> String         -> Nomex ()
   ProposeRule  ::                               Rule              -> Nomex Bool
   ActivateRule ::                               RuleNumber        -> Nomex Bool
   RejectRule   ::                               RuleNumber        -> Nomex Bool
   AddRule      ::                               Rule              -> Nomex Bool
   DelRule      ::                               RuleNumber        -> Nomex Bool
   ModifyRule   ::                               RuleNumber        -> Rule           -> Nomex Bool
   GetRules     ::                               Nomex [Rule]
   SetVictory   ::                               [PlayerNumber]    -> Nomex ()
   GetPlayers   ::                               Nomex [PlayerInfo]
   Const        ::                               a                 -> Nomex a
   Bind         ::                               Nomex a           -> (a -> Nomex b) -> Nomex b
   CurrentTime  ::                               Nomex UTCTime
   SelfRuleNumber ::                             Nomex RuleNumber
   deriving (Typeable)

     
instance Monad Nomex where
   return = Const
   (>>=) = Bind
   
instance Functor Nomex where
  fmap f e = Bind e $ Const . f

instance Applicative Nomex where
  pure = Const
  f <*> a = do
     f' <- f
     a' <- a
     return $ f' a'

instance Show a => Show (Nomex a) where
   show _ = "Nomex" -- ++ (show a)


-- * Variables

-- | a container for a variable name and type
data V a = V {varName :: String} deriving (Typeable)

-- | stores the variable's data
data Var = forall a . (Typeable a, Show a, Eq a) =>
        Var { _vRuleNumber :: Int,
              _vName :: String,
              vData :: a}

instance Show Var where
    show (Var a b c) = (show a) ++ " " ++ (show b) ++ " " ++ (show c)

instance Eq Var where
    Var a b c == Var d e f = (a,b,c) === (d,e,f)

type Output = (PlayerNumber, String)


-- * Events

-- | events types
data Player = Arrive | Leave deriving (Typeable, Show, Eq)
data RuleEvent = Proposed | Activated | Rejected | Added | Modified | Deleted deriving (Typeable, Show, Eq)
data Time           deriving Typeable
data EvRule         deriving Typeable
data Message m      deriving Typeable
data InputChoice c  deriving Typeable
data InputString    deriving Typeable
data Victory        deriving Typeable

-- | events names
data Event a where
    Player      :: Player ->                 Event Player
    RuleEv      :: RuleEvent ->              Event RuleEvent
    Time        :: UTCTime ->                Event Time
    Message     :: String ->                 Event (Message m)
    InputChoice :: (Eq c, Show c) => PlayerNumber -> String -> [c] -> c -> Event (InputChoice c)
    InputString :: PlayerNumber -> String -> Event InputString
    Victory     ::                           Event Victory

-- | data associated with each events
data EventData a where
    PlayerData      :: {playerData :: PlayerInfo}         -> EventData Player
    RuleData        :: {ruleData :: Rule}                 -> EventData RuleEvent
    TimeData        :: {timeData :: UTCTime}              -> EventData Time
    MessageData     :: (Show m) => {messageData :: m}     -> EventData (Message m)
    InputChoiceData :: (Show c) => {inputChoiceData :: c} -> EventData (InputChoice c)
    InputStringData :: {inputStringData :: String}        -> EventData InputString
    VictoryData     :: {victoryData :: [PlayerInfo]}      -> EventData Victory

deriving instance             Typeable1 EventData
deriving instance             Typeable1 Event
deriving instance (Show a) => Show      (Event a)
deriving instance (Show a) => Show      (EventData a)
deriving instance (Show a) => Show      (Message a)
deriving instance (Show a) => Show      (InputChoice a)
deriving instance             Show      Time
deriving instance             Show      InputString
deriving instance             Show      Victory
deriving instance             Eq        Time
deriving instance             Eq        Victory
deriving instance             Eq        EvRule
deriving instance             Eq        (InputChoice a)
deriving instance             Eq        InputString
deriving instance             Eq        (Message m)
deriving instance (Eq e) =>   Eq        (Event e)


data EventHandler where
    EH :: (Typeable e, Show e, Eq e) =>
        {_eventNumber :: EventNumber,
         _ruleNumber  :: RuleNumber,
         --eventName   :: EventName,
         event       :: Event e,
         handler     :: (EventNumber, EventData e) -> Nomex ()} -> EventHandler

instance Show EventHandler where
    show (EH en rn e _) = (show en) ++ " " ++ " " ++ (show rn) ++ " (" ++ (show e) ++"),\n"

instance Eq EventHandler where
    (EH {_eventNumber=e1}) == (EH {_eventNumber=e2}) = e1 == e2

instance Ord EventHandler where
    (EH {_eventNumber=e1}) <= (EH {_eventNumber=e2}) = e1 <= e2

-- * Rule

-- | Type of a rule function.
type RuleFunc = Nomex RuleResp

-- | Return type of a rule function.
-- it can be either nothing or another rule.
data RuleResp =
      Void
    | Meta (Rule -> Nomex BoolResp)
    deriving (Typeable)
--An extended type for booleans supporting immediate or delayed response (through a message)
data BoolResp = BoolResp Bool
              | MsgResp (Event (Message Bool))


instance Show RuleResp where
   show _ = "RuleResp"


  
-- | An informationnal structure about a rule
data Rule = Rule { _rNumber       :: RuleNumber,       -- number of the rule (must be unique) TO CHECK
                   _rName         :: RuleName,         -- short name of the rule 
                   _rDescription  :: String,           -- description of the rule
                   _rProposedBy   :: PlayerNumber,     -- player proposing the rule
                   _rRuleCode     :: Code,             -- code of the rule as a string
                   _rRuleFunc     :: RuleFunc,         -- function representing the rule (interpreted from rRuleCode)
                   _rStatus       :: RuleStatus,       -- status of the rule
                   _rAssessedBy   :: Maybe RuleNumber} -- which rule accepted or rejected this rule
                   deriving (Typeable, Show)

instance Eq Rule where
    (Rule {_rNumber=r1}) == (Rule {_rNumber=r2}) = r1 == r2

instance Ord Rule where
     (Rule {_rNumber=r1}) <= (Rule {_rNumber=r2}) = r1 <= r2

-- | the status of a rule.
data RuleStatus = Active      -- Active rules forms the current Constitution
                | Pending     -- Proposed rules
                | Reject      -- Rejected rules
                deriving (Eq, Show, Typeable)
                
data SubmitRule = SubmitRule RuleName String RuleCode deriving (Show, Read, Eq, Ord, Data, Typeable)


-- * Player

-- | informations on players
data PlayerInfo = PlayerInfo { _playerNumber :: PlayerNumber,
                               _playerName   :: String}
                               deriving (Eq, Typeable, Show)

-- * Game
           
-- | The state of the game:
data Game = Game { _gameName      :: GameName,
                   _gameDesc      :: GameDesc,
                   _rules         :: [Rule],
                   _players       :: [PlayerInfo],
                   _variables     :: [Var],
                   _events        :: [EventHandler],
                   _outputs       :: [Output],
                   _victory       :: [PlayerNumber],
                   _currentTime   :: UTCTime}
                   deriving (Typeable)
                   
data GameDesc = GameDesc { _desc :: String, _agora :: String} deriving (Eq, Show, Read)

instance Show Game where
    show (Game { _gameName, _rules, _players, _variables, _events, _outputs, _victory, _currentTime}) =
        "Game Name = " ++ (show _gameName) ++ "\n Rules = " ++ (concat $ intersperse "\n " $ map show _rules) ++ "\n Players = " ++ (show _players) ++ "\n Variables = " ++
        (show _variables) ++ "\n Events = " ++ (show _events) ++ "\n Outputs = " ++ (show _outputs) ++ "\n Victory = " ++ (show _victory) ++ "\n currentTime = " ++ (show _currentTime) ++ "\n"

instance Eq Game where
   (Game {_gameName=gn1}) == (Game {_gameName=gn2}) = gn1 == gn2

instance Ord Game where
   compare (Game {_gameName=gn1}) (Game {_gameName=gn2}) = compare gn1 gn2


-- | an equality that tests also the types.
(===) :: (Typeable a, Typeable b, Eq b) => a -> b -> Bool
(===) x y = cast x == Just y

-- | Replaces all instances of a value in a list by another value.
replaceWith :: (a -> Bool)   -- ^ Value to search
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replaceWith f y = map (\z -> if f z then y else z)


    
$( makeLenses [''Game, ''GameDesc, ''Rule, ''PlayerInfo, ''EventHandler, ''Var] )

