
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
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

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

-- * Expression

-- | an Exp allows the player's rule to have access to the state of the game.
-- | it is a compositional algebra defined with a GADT.
data Exp a where
     NewVar       :: (Typeable a, Show a, Eq a) => VarName -> a -> Exp (Maybe (V a))
     DelVar       :: (V a) -> Exp Bool
     ReadVar      :: (Typeable a, Show a, Eq a) => (V a) -> Exp (Maybe a)
     WriteVar     :: (Typeable a, Show a, Eq a) => (V a) -> a -> Exp Bool
     OnEvent      :: (Typeable e, Show e, Eq e) => Event e -> ((EventNumber, EventData e) -> Exp ()) -> Exp EventNumber
     DelEvent     :: EventNumber -> Exp Bool
     DelAllEvents :: (Typeable e, Show e, Eq e) => Event e -> Exp ()
     SendMessage  :: (Typeable a, Show a, Eq a) => Event (Message a) -> a -> Exp ()
     Output       :: PlayerNumber -> String -> Exp ()
     ProposeRule  :: Rule -> Exp Bool
     ActivateRule :: RuleNumber -> Exp Bool
     RejectRule   :: RuleNumber -> Exp Bool
     AddRule      :: Rule -> Exp Bool
     DelRule      :: RuleNumber -> Exp Bool
     ModifyRule   :: RuleNumber -> Rule -> Exp Bool
     GetRules     :: Exp [Rule]
     SetVictory   :: [PlayerNumber] -> Exp ()
     GetPlayers   :: Exp [PlayerInfo]
     Const        :: a -> Exp a
     Bind         :: Exp a -> (a -> Exp b) -> Exp b
     CurrentTime  :: Exp UTCTime
     SelfRuleNumber :: Exp RuleNumber
     deriving (Typeable)

instance Monad Exp where
   return = Const
   (>>=) = Bind
   
instance Functor Exp where
  fmap f e = Bind e $ Const . f

instance Applicative Exp where
  pure = Const
  f <*> a = do
     f' <- f
     a' <- a
     return $ f' a'

-- * Variables

-- | a container for a variable name and type
data V a = V {varName :: String} deriving (Typeable)

-- | stores the variable's data
data Var = forall a . (Typeable a, Show a, Eq a) =>
        Var { vRuleNumber :: Int,
              vName :: String,
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

deriving instance Typeable1 EventData
deriving instance Typeable1 Event
deriving instance (Show a) => Show (Event a)
deriving instance Show Time
deriving instance (Show a) => Show (Message a)
deriving instance (Show a) => Show (InputChoice a)
deriving instance Show InputString
deriving instance Show Victory
deriving instance Eq Time
deriving instance Eq Victory
deriving instance Eq EvRule
deriving instance Eq (InputChoice a)
deriving instance Eq InputString
deriving instance Eq (Message m)
deriving instance (Eq e) => Eq (Event e)
deriving instance (Show a) => Show (EventData a)

data EventHandler where
    EH :: (Typeable e, Show e, Eq e) =>
        {eventNumber :: EventNumber,
         ruleNumber  :: RuleNumber,
         --eventName   :: EventName,
         event       :: Event e,
         handler     :: (EventNumber, EventData e) -> Exp ()} -> EventHandler

instance Show EventHandler where
    show (EH en rn e _) = (show en) ++ " " ++ " " ++ (show rn) ++ " (" ++ (show e) ++"),\n"

instance Eq EventHandler where
    (EH {eventNumber=e1}) == (EH {eventNumber=e2}) = e1 == e2

instance Ord EventHandler where
    (EH {eventNumber=e1}) <= (EH {eventNumber=e2}) = e1 <= e2

-- * Rule

-- | type of rule to assess the legality of a given parameter
type OneParamRule a = a -> Exp RuleResponse

-- | type of rule that just mofify the game state
type NoParamRule = Exp ()

-- | a rule can assess the legality either immediatly of later through a messsage
data RuleResponse = BoolResp {boolResp :: Bool}
                  | MsgResp  {msgResp :: Event (Message Bool)}

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
                   gameDesc      :: GameDesc,
                   rules         :: [Rule],
                   players       :: [PlayerInfo],
                   variables     :: [Var],
                   events        :: [EventHandler],
                   outputs       :: [Output],
                   victory       :: [PlayerNumber],
                   currentTime   :: UTCTime}
                   deriving (Typeable)
                   
data GameDesc = GameDesc { desc :: String, agora :: String} deriving (Eq, Show, Read)

instance Show Game where
    show (Game { gameName, rules, players, variables, events, outputs, victory, currentTime}) =
        "Game Name = " ++ (show gameName) ++ "\n Rules = " ++ (concat $ intersperse "\n " $ map show rules) ++ "\n Players = " ++ (show players) ++ "\n Variables = " ++
        (show variables) ++ "\n Events = " ++ (show events) ++ "\n Outputs = " ++ (show outputs) ++ "\n Victory = " ++ (show victory) ++ "\n currentTime = " ++ (show currentTime) ++ "\n"

instance Eq Game where
   (Game {gameName=gn1}) == (Game {gameName=gn2}) = gn1 == gn2

instance Ord Game where
   compare (Game {gameName=gn1}) (Game {gameName=gn2}) = compare gn1 gn2
   

-- | an equality that tests also the types.
(===) :: (Typeable a, Typeable b, Eq b) => a -> b -> Bool
(===) x y = cast x == Just y

-- | Replaces all instances of a value in a list by another value.
replaceWith :: (a -> Bool)   -- ^ Value to search
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replaceWith f y = map (\z -> if f z then y else z)
