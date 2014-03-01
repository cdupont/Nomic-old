{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module implements game engine (for the nomyx language, see Language.Nomyx)
module Language.Nomyx.Engine.Game where

import Prelude hiding (log)
import Data.List
import Language.Nomyx.Expression
import Language.Nomyx.Engine.Utils
import Data.Lens.Template
import Data.Time
import Data.Typeable
import GHC.Show (showList__)
import GHC.Read (readListPrecDefault, readListDefault, Read(..), lexP, parens)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec (prec)
import Text.Read.Lex (Lexeme(..))
import Text.ParserCombinators.ReadPrec (reset)
import Data.Data


-- * Game

type GameName = String

-- | The state of the game:
data Game = Game { _gameName    :: GameName,
                   _gameDesc    :: GameDesc,
                   _rules       :: [Rule],
                   _players     :: [PlayerInfo],
                   _variables   :: [Var],
                   _events      :: [EventHandler],
                   _outputs     :: [Output],
                   _victory     :: Maybe VictoryCond,
                   _logs        :: [Log],
                   _currentTime :: UTCTime,
                   _simu        :: Maybe Simulation
                 }
                   deriving (Typeable)

data Simulation = Simulation { _ofGame    :: GameName,
                               _ownedBy   :: PlayerNumber,
                               _startedAt :: UTCTime}
                               deriving (Typeable, Show, Read, Eq)

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
     Ident "_simu" <- lexP;
     Punc "=" <- lexP;
     simu <- reset readPrec;
     Punc "," <- lexP;
     Ident "_currentTime" <- lexP;
     Punc "=" <- lexP;
     time <- reset readPrec;
     Punc "}" <- lexP;
     return $ Game name desc [] [] [] [] [] Nothing [] time simu
  readList = readListDefault
  readListPrec = readListPrecDefault

instance Show Game where
   showsPrec p(Game name desc _ _ _ _ _ _ _ time simu) = showParen (p >= 11) $
      showString "Game {" .
      showString "_gameName = " .
      showsPrec 0 name .
      showString ", " .
      showString "_gameDesc = " .
      showsPrec 0 desc .
      showString ", " .
      showString "_simu = " .
      showsPrec 0 simu .
      showString ", " .
      showString "_currentTime = " .
      showsPrec 0 time .
      showString "}"
   showList = showList__ (showsPrec 0)


displayGame :: Game -> String
displayGame (Game { _gameName, _rules, _players, _variables, _events, _outputs, _victory, _simu, _currentTime}) =
        "Game Name = " ++ (show _gameName) ++
        "\n Rules = " ++ (concat $ intersperse "\n " $ map show _rules) ++
        "\n Players = " ++ (show _players) ++
        "\n Variables = " ++ (show _variables) ++
        "\n Events = " ++ (show _events) ++
        "\n Outputs = " ++ (show _outputs) ++
        "\n Victory = " ++ (show _victory) ++
        "\n Simulation = " ++ (show _simu) ++
        "\n currentTime = " ++ (show _currentTime) ++ "\n"

emptyGame name desc date = Game {
    _gameName      = name,
    _gameDesc      = desc,
    _rules         = [],
    _players       = [],
    _variables     = [],
    _events        = [],
    _outputs       = [],
    _victory       = Nothing,
    _logs          = [],
    _simu          = Nothing,
    _currentTime   = date}


-- * Variables

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

data EventHandler where
    EH :: (Typeable e, Show e, Eq e) =>
        {_eventNumber :: EventNumber,
         _ruleNumber  :: RuleNumber,
         event        :: Event e,
         handler      :: (EventNumber, EventData e) -> Nomex (),
         _evStatus    :: Status} -> EventHandler

data Status = SActive | SDeleted deriving (Eq, Show)

instance Show EventHandler where
    show (EH en rn e _ s) = (show en) ++ " " ++ (show rn) ++ " (" ++ (show e) ++"), status = " ++ (show s)

instance Eq EventHandler where
    (EH {_eventNumber=e1}) == (EH {_eventNumber=e2}) = e1 == e2

instance Ord EventHandler where
    (EH {_eventNumber=e1}) <= (EH {_eventNumber=e2}) = e1 <= e2


-- * Outputs

data Output = Output { _outputNumber  :: OutputNumber,         -- number of the output
                       _oRuleNumber   :: RuleNumber,           -- rule that triggered the output
                       _oPlayerNumber :: (Maybe PlayerNumber), -- player to display the output to (Nothing means display to all players)
                       _output        :: NomexNE String,       -- output string
                       _oStatus       :: Status}               -- status of the output
                       deriving (Show)

-- * Logs

data Log = Log { _lPlayerNumber :: Maybe PlayerNumber,
                 _lTime         :: UTCTime,
                 _lMsg          :: String}
                 deriving (Show)

-- * Rules

data SubmitRule = SubmitRule RuleName RuleDesc RuleCode deriving (Show, Read, Eq, Ord, Data, Typeable)


-- * Victory

data VictoryCond = VictoryCond RuleNumber (NomexNE [PlayerNumber]) deriving (Show, Typeable)


$( makeLenses [''Game, ''Simulation, ''GameDesc, ''EventHandler, ''Var, ''Output] )



