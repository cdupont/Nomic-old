{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}


-- | This module containt the type definitions necessary to build a Nomic rule. 
module Language.Nomyx.Expression where

import Data.Typeable
import Data.Time
import Control.Applicative hiding (Const)
import Data.Lens.Template
import Control.Monad.Error

type PlayerNumber = Int
type PlayerName = String
type RuleNumber = Int
type RuleName = String
type RuleDesc = String
type RuleText = String
type RuleCode = String
type EventNumber = Int
type EventName = String
type VarName = String
type Code = String
type OutputNumber = Int

-- * Nomyx Expression

data Eff = Effect | NoEffect
type Effect = 'Effect
type NoEffect = 'NoEffect

-- | A Nomex (Nomyx Expression) allows the players to write rules.
-- within the rules, you can access and modify the state of the game.
type Nomex = Exp Effect

-- | A NomexNE (Nomyx Expression No Effect) is a specialisation of the type that guaranties
-- that the instructions will have no effects.
type NomexNE = Exp NoEffect

data Exp :: Eff -> * -> *   where
   --Variable management
   NewVar         :: (Typeable a, Show a, Eq a) => VarName -> a -> Nomex (Maybe (V a))
   ReadVar        :: (Typeable a, Show a, Eq a) => (V a) -> Exp NoEffect (Maybe a)
   WriteVar       :: (Typeable a, Show a, Eq a) => (V a) -> a -> Nomex Bool
   DelVar         ::                               (V a) -> Nomex Bool
   --Events management
   OnEvent        :: (Typeable e, Show e, Eq e) => Event e -> ((EventNumber, EventData e) -> Nomex ()) -> Nomex EventNumber
   DelEvent       :: EventNumber -> Nomex Bool
   DelAllEvents   :: (Typeable e, Show e, Eq e) => Event e -> Nomex ()
   SendMessage    :: (Typeable a, Show a, Eq a) => Event (Message a) -> a -> Nomex ()
   --Rules management
   ProposeRule    :: Rule -> Nomex Bool
   ActivateRule   :: RuleNumber -> Nomex Bool
   RejectRule     :: RuleNumber -> Nomex Bool
   AddRule        :: Rule -> Nomex Bool
   ModifyRule     :: RuleNumber -> Rule -> Nomex Bool
   GetRules       :: NomexNE [Rule]
   --Players management
   GetPlayers     :: NomexNE [PlayerInfo]
   SetPlayerName  :: PlayerNumber -> PlayerName -> Nomex Bool
   DelPlayer      :: PlayerNumber -> Nomex Bool
   --Output
   NewOutput      :: (Maybe PlayerNumber) -> (NomexNE String) -> Nomex OutputNumber
   GetOutput      :: OutputNumber -> NomexNE (Maybe String)
   UpdateOutput   :: OutputNumber -> (NomexNE String) -> Nomex Bool
   DelOutput      :: OutputNumber -> Nomex Bool
   --Mileacenous
   SetVictory     :: NomexNE [PlayerNumber] -> Nomex ()
   CurrentTime    :: NomexNE UTCTime
   SelfRuleNumber :: NomexNE RuleNumber
   --Monadic bindings
   Return         :: a -> Exp e a
   Bind           :: Exp e a -> (a -> Exp e b) -> Exp e b
   ThrowError     :: String -> Exp Effect a
   CatchError     :: Nomex a -> (String -> Nomex a) -> Nomex a
   LiftEffect     :: NomexNE a -> Nomex a

instance Typeable1 (Exp a) where
    typeOf1 _
      = mkTyConApp
          (mkTyCon3
             "main"
             "Language.Nomyx.Expression"
             "Nomex")
          []

liftEffect :: NomexNE a -> Nomex a
liftEffect = LiftEffect

--{-# NOINLINE vTyCon #-}
--vTyCon :: TyCon
--vTyCon = mkTyCon3 "Language.Nomyx.Expression" "Nomex"
--
--instance Typeable1 Nomex where
--    typeOf1 _ = mkTyConApp vTyCon []

--vTyCon :: TyCon
--vTyCon = mkTyCon "Language.Nomyx.Expression"
--
--instance Typeable (Nomex a) where
--   typeOf (NewVar _ _)        = typeOf (undefined :: Nomex (Maybe (V (a))))
--   typeOf (ReadVar _)         = typeOf (undefined :: Nomex (Maybe a))
--   typeOf (WriteVar _ _)      = typeOf (undefined :: Nomex Bool)
--   typeOf (DelVar _)          = typeOf (undefined :: Nomex Bool)
--   typeOf (OnEvent _ _)       = typeOf (undefined :: Nomex EventNumber)
--   typeOf (DelEvent _)        = typeOf (undefined :: Nomex Bool)
--   typeOf (DelAllEvents _)    = typeOf (undefined :: Nomex ())
--   typeOf (SendMessage _ _)   = typeOf (undefined :: Nomex ())
--   typeOf (ProposeRule _)     = typeOf (undefined :: Nomex Bool)
--   typeOf (ActivateRule _)    = typeOf (undefined :: Nomex Bool)
--   typeOf (RejectRule _)      = typeOf (undefined :: Nomex Bool)
--   typeOf (AddRule _)         = typeOf (undefined :: Nomex Bool)
--   typeOf (ModifyRule _ _)    = typeOf (undefined :: Nomex Bool)
--   typeOf (GetRules)          = typeOf (undefined :: Nomex [Rule])
--   typeOf (GetPlayers)        = typeOf (undefined :: Nomex [PlayerInfo])
--   typeOf (SetPlayerName _ _) = typeOf (undefined :: Nomex Bool)
--   typeOf (DelPlayer _)       = typeOf (undefined :: Nomex Bool)
--   typeOf (NewOutput _ _)     = typeOf (undefined :: Nomex OutputNumber)
--   typeOf (GetOutput _)       = typeOf (undefined :: Nomex (Maybe String))
--   typeOf (UpdateOutput _ _)  = typeOf (undefined :: Nomex Bool)
--   typeOf (DelOutput _)       = typeOf (undefined :: Nomex Bool)
--   typeOf (SetVictory _)      = typeOf (undefined :: Nomex ())
--   typeOf (CurrentTime)       = typeOf (undefined :: Nomex UTCTime)
--   typeOf (SelfRuleNumber)    = typeOf (undefined :: Nomex RuleNumber)
--   typeOf (Return _)          = typeOf (undefined :: Nomex a)
--   typeOf (Bind _ _)          = typeOf (undefined :: Nomex a)
--   typeOf (ThrowError _)      = typeOf (undefined :: Nomex a)
--   typeOf (CatchError _ _)    = typeOf (undefined :: Nomex a)
   
     
instance Monad (Exp a) where
   return = Return
   (>>=) = Bind
   
instance Functor (Exp a) where
   fmap f e = Bind e $ Return . f

instance Applicative (Exp a) where
   pure = Return
   f <*> a = do
      f' <- f
      a' <- a
      return $ f' a'

instance MonadError String Nomex where
   throwError = ThrowError
   catchError = CatchError

instance Typeable a => Show (Exp r a) where
   show e = "<" ++ (show $ typeOf e) ++ ">"


-- * Variables

-- | a container for a variable name and type
data V a = V {varName :: String} deriving Typeable

-- * Events

-- | events types
data Player = Arrive | Leave deriving (Typeable, Show, Eq)
data RuleEvent = Proposed | Activated | Rejected | Added | Modified | Deleted deriving (Typeable, Show, Eq)
data Time           deriving Typeable
data EvRule         deriving Typeable
data Message m      deriving Typeable
data Victory        deriving Typeable
data Input a = Input PlayerNumber String (InputForm a)
data InputForm a = Radio [(a, String)]
                 | Text
                 | TextArea
                 | Button
                 | Checkbox [(a, String)]

-- | events names
data Event a where
    Player      :: Player ->                     Event Player
    RuleEv      :: RuleEvent ->                  Event RuleEvent
    Time        :: UTCTime ->                    Event Time
    Message     :: String ->                     Event (Message m)
    InputEv     :: (Eq a, Show a, Typeable a) => Input a -> Event (Input a)
    Victory     ::                               Event Victory

-- data sent back by inputs
data InputData a = RadioData a
                 | CheckboxData [a]
                 | TextData String
                 | TextAreaData String
                 | ButtonData

                  
-- | data associated with each events
data EventData a where
    PlayerData  ::             {playerData :: PlayerInfo}    -> EventData Player
    RuleData    ::             {ruleData :: Rule}            -> EventData RuleEvent
    TimeData    ::             {timeData :: UTCTime}         -> EventData Time
    MessageData :: (Show m) => {messageData :: m}            -> EventData (Message m)
    InputData   :: (Show a) => {inputData :: InputData a}    -> EventData (Input a)
    VictoryData ::             {victoryData :: [PlayerInfo]} -> EventData Victory

deriving instance             Typeable1 EventData
deriving instance             Typeable1 Event
deriving instance             Typeable1 Input
deriving instance             Typeable1 InputForm
deriving instance (Show a) => Show      (Event a)
deriving instance (Show a) => Show      (InputForm a)
deriving instance (Show a) => Show      (Input a)
deriving instance (Show a) => Show      (EventData a)
deriving instance (Show a) => Show      (InputData a)
deriving instance (Show a) => Show      (Message a)
deriving instance             Show      Time
deriving instance             Show      Victory
deriving instance             Eq        Time
deriving instance             Eq        Victory
deriving instance             Eq        EvRule
deriving instance             Eq        (Message m)
deriving instance (Eq e) =>   Eq        (Event e)
deriving instance (Eq e) =>   Eq        (Input e)
deriving instance (Eq e) =>   Eq        (InputForm e)


type Msg a = Event (Message a)
type MsgData a = EventData (Message a)

-- * Rule

-- | Type of a rule function.
type RuleFunc = Nomex RuleResp

-- | Return type of a rule function.
-- it can be either nothing or another rule.
data RuleResp =
      Void
    | Meta (Rule -> NomexNE BoolResp)
    deriving (Typeable)
--An extended type for booleans supporting immediate or delayed response (through a message)
data BoolResp = BoolResp Bool
              | MsgResp (Msg Bool)

instance Show RuleResp where
   show _ = "RuleResp"
  
-- | An informationnal structure about a rule
data Rule = Rule { _rNumber      :: RuleNumber,       -- number of the rule (must be unique) TO CHECK
                   _rName        :: RuleName,         -- short name of the rule 
                   _rDescription :: String,           -- description of the rule
                   _rProposedBy  :: PlayerNumber,     -- player proposing the rule
                   _rRuleCode    :: Code,             -- code of the rule as a string
                   _rRuleFunc    :: RuleFunc,         -- function representing the rule (interpreted from rRuleCode)
                   _rStatus      :: RuleStatus,       -- status of the rule
                   _rAssessedBy  :: Maybe RuleNumber} -- which rule accepted or rejected this rule
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
          
-- * Player

-- | informations on players
data PlayerInfo = PlayerInfo { _playerNumber :: PlayerNumber,
                               _playerName   :: String,
                               _playAs       :: Maybe PlayerNumber}
                               deriving (Eq, Typeable, Show)

instance Ord PlayerInfo where
   h <= g = (_playerNumber h) <= (_playerNumber g)


partial :: String -> Nomex (Maybe a) -> Nomex a
partial s nm = do
   m <- nm
   case m of
      Just a -> return a
      Nothing -> throwError s

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

$( makeLenses [''Rule, ''PlayerInfo] )

