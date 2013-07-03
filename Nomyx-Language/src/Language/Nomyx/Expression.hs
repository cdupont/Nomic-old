
{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, GADTs,
    UndecidableInstances, DeriveDataTypeable, FlexibleContexts,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies,
    TypeSynonymInstances, TemplateHaskell, ExistentialQuantification,
    TypeFamilies, ScopedTypeVariables, StandaloneDeriving, NamedFieldPuns,
    EmptyDataDecls, QuasiQuotes #-}

-- | This module containt the type definitions necessary to build a Nomic rule. 
module Language.Nomyx.Expression where

import Data.Typeable
import Data.Time
import Control.Applicative hiding (Const)
import Data.Lens.Template
import Data.Data (Data)
import GHC.Read (readListPrecDefault, readListDefault, Read(..), lexP, parens)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec (prec)
import Text.Read.Lex (Lexeme(..))
import Text.ParserCombinators.ReadPrec (reset)
import GHC.Show (showList__)
import Control.Monad.Error
import Language.Nomyx.Utils ((===))
import Data.List (intersperse)

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
type GameName = String
type Code = String
type Output = (PlayerNumber, String)
type Log = (Maybe PlayerNumber, String)

-- * Nomyx Expression

-- | A Nomex (Nomyx Expression) allows the players to write rules.
-- | within the rules, you can access and modify the state of the game.
-- | It is a compositional algebra defined with a GADT.
data Nomex a where
   --Variable management
   NewVar       :: (Typeable a, Show a, Eq a) => VarName           -> a              -> Nomex (Maybe (V a))
   ReadVar      :: (Typeable a, Show a, Eq a) => (V a)             -> Nomex (Maybe a)
   WriteVar     :: (Typeable a, Show a, Eq a) => (V a)             -> a              -> Nomex Bool
   DelVar       ::                               (V a)             -> Nomex Bool
   --Events management
   OnEvent      :: (Typeable e, Show e, Eq e) => Event e           -> ((EventNumber, EventData e) -> Nomex ()) -> Nomex EventNumber
   DelEvent     ::                               EventNumber       -> Nomex Bool
   DelAllEvents :: (Typeable e, Show e, Eq e) => Event e           -> Nomex ()
   SendMessage  :: (Typeable a, Show a, Eq a) => Event (Message a) -> a              -> Nomex ()
   --Rules management
   ProposeRule  ::                               Rule              -> Nomex Bool
   ActivateRule ::                               RuleNumber        -> Nomex Bool
   RejectRule   ::                               RuleNumber        -> Nomex Bool
   AddRule      ::                               Rule              -> Nomex Bool
   ModifyRule   ::                               RuleNumber        -> Rule           -> Nomex Bool
   GetRules     ::                               Nomex [Rule]
   --Players management
   GetPlayers   ::                               Nomex [PlayerInfo]
   SetPlayerName::                               PlayerNumber      -> PlayerName     -> Nomex Bool
   DelPlayer    ::                               PlayerNumber      -> Nomex Bool
   --Mileacenous
   SetVictory   ::                               [PlayerNumber]    -> Nomex ()
   Output       ::                               PlayerNumber      -> String         -> Nomex ()
   CurrentTime  ::                               Nomex UTCTime
   SelfRuleNumber ::                             Nomex RuleNumber
   --Monadic bindings
   Return       ::                               a                 -> Nomex a
   Bind         ::                               Nomex a           -> (a -> Nomex b) -> Nomex b
   ThrowError   ::                               String            -> Nomex a
   CatchError   ::                               Nomex a           -> (String -> Nomex a) -> Nomex a
   deriving (Typeable)
     
instance Monad Nomex where
   return = Return
   (>>=) = Bind
   
instance Functor Nomex where
   fmap f e = Bind e $ Return . f

instance Applicative Nomex where
   pure = Return
   f <*> a = do
      f' <- f
      a' <- a
      return $ f' a'

instance MonadError String Nomex where
   throwError = ThrowError
   catchError = CatchError

instance Typeable a => Show (Nomex a) where
   show e = '<' : (show . typeOf) e ++ ">"

    
-- * Variables

-- | a container for a variable name and type
data V a = V {varName :: String} deriving (Typeable)

-- | stores the variable's data
data Var = forall a . (Typeable a, Show a, Eq a) =>
        Var { _vRuleNumber :: RuleNumber,
              _vName       :: String,
              vData        :: a}

instance Show Var where
    show (Var a b c) = "Rule number = " ++ (show a) ++ ", Name = " ++ (show b) ++ ", Value = " ++ (show c) ++ "\n"

instance Eq Var where
    Var a b c == Var d e f = (a,b,c) === (d,e,f)

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
data InputData a = RadioData a
                 | TextData String
                 | TextAreaData String
                 | ButtonData ()
                 | CheckboxData [a]
data InputResultSerialized = RadioDataSer Int
                 | TextDataSer String
                 | TextAreaDataSer String
                 | ButtonDataSer ()
                 | CheckboxDataSer [Int]
                   deriving (Show, Read, Eq, Ord)
-- | events names
data Event a where
    Player      :: Player ->                 Event Player
    RuleEv      :: RuleEvent ->              Event RuleEvent
    Time        :: UTCTime ->                Event Time
    Message     :: String ->                 Event (Message m)
    InputEv     :: (Eq a, Show a, Typeable a) => Input a -> Event (Input a)
    Victory     ::                           Event Victory


-- | data associated with each events
data EventData a where
    PlayerData      :: {playerData :: PlayerInfo}         -> EventData Player
    RuleData        :: {ruleData :: Rule}                 -> EventData RuleEvent
    TimeData        :: {timeData :: UTCTime}              -> EventData Time
    MessageData     :: (Show m) => {messageData :: m}     -> EventData (Message m)
    InputData       :: (Show a) => {inputData :: InputData a}       -> EventData (Input a)
    VictoryData     :: {victoryData :: [PlayerInfo]}      -> EventData Victory

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

data EventStatus = EvActive | EvDeleted deriving (Eq, Show)

data EventHandler where
    EH :: (Typeable e, Show e, Eq e) =>
        {_eventNumber :: EventNumber,
         _ruleNumber  :: RuleNumber,
         event        :: Event e,
         handler      :: (EventNumber, EventData e) -> Nomex (),
         _evStatus    :: EventStatus} -> EventHandler

instance Show EventHandler where
    show (EH en rn e _ _) = (show en) ++ " " ++ (show rn) ++ " (" ++ (show e) ++"),\n"

instance Eq EventHandler where
    (EH {_eventNumber=e1}) == (EH {_eventNumber=e2}) = e1 == e2

instance Ord EventHandler where
    (EH {_eventNumber=e1}) <= (EH {_eventNumber=e2}) = e1 <= e2

type Msg a = Event (Message a)
type MsgData a = EventData (Message a)

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
              | MsgResp (Msg Bool)


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
                
data SubmitRule = SubmitRule RuleName RuleDesc RuleCode deriving (Show, Read, Eq, Ord, Data, Typeable)


-- * Player

-- | informations on players
data PlayerInfo = PlayerInfo { _playerNumber :: PlayerNumber,
                               _playerName   :: String}
                               deriving (Eq, Typeable, Show)

-- * Game

           
-- | The state of the game:
data Game = Game { _gameName    :: GameName,
                   _gameDesc    :: GameDesc,
                   _rules       :: [Rule],
                   _players     :: [PlayerInfo],
                   _variables   :: [Var],
                   _events      :: [EventHandler],
                   _outputs     :: [Output],
                   _victory     :: [PlayerNumber],
                   _log         :: [Log],
                   _currentTime :: UTCTime
                 }
                   deriving (Typeable)
                   
data GameDesc = GameDesc { _desc :: String, _agora :: String} deriving (Eq, Show, Read, Ord)

instance Eq Game where
   (Game {_gameName=gn1}) == (Game {_gameName=gn2}) = gn1 == gn2

instance Ord Game where
   compare (Game {_gameName=gn1}) (Game {_gameName=gn2}) = compare gn1 gn2

--Game is not serializable in its entierety. We serialize only the adequate parts.   
instance Read Game where
  readPrec = parens $ ReadPrec.prec 11 $ do
     Ident "Game" <- lexP;
     Punc "{" <- lexP;
     Ident "_gameName" <- lexP;
     Punc "=" <- lexP;
     name <- reset readPrec;
     Punc "," <- lexP;
     Ident "_gameDesc" <- lexP;
     Punc "=" <- lexP;
     desc <- reset readPrec;
     Punc "," <- lexP;
     Ident "_currentTime" <- lexP;
     Punc "=" <- lexP;
     time <- reset readPrec;
     Punc "}" <- lexP;
     return $ Game name desc [] [] [] [] [] [] [] time
  readList = readListDefault
  readListPrec = readListPrecDefault

instance Show Game where
   showsPrec p(Game name desc _ _ _ _ _ _ _ time) = showParen (p >= 11) $
      showString "Game {" .
      showString "_gameName = " .
      showsPrec 0 name .
      showString ", " .
      showString "_gameDesc = " .
      showsPrec 0 desc .
      showString ", " .
      showString "_currentTime = " .
      showsPrec 0 time .
      showString "}"
   showList = showList__ (showsPrec 0)

displayGame :: Game -> String
displayGame (Game { _gameName, _rules, _players, _variables, _events, _outputs, _victory, _currentTime}) =
        "Game Name = " ++ (show _gameName) ++ "\n Rules = " ++ (concat $ intersperse "\n " $ map show _rules) ++ "\n Players = " ++ (show _players) ++ "\n Variables = " ++
        (show _variables) ++ "\n Events = " ++ (show _events) ++ "\n Outputs = " ++ (show _outputs) ++ "\n Victory = " ++ (show _victory) ++ "\n currentTime = " ++ (show _currentTime) ++ "\n"

$( makeLenses [''Game, ''GameDesc, ''Rule, ''PlayerInfo, ''EventHandler, ''Var] )

