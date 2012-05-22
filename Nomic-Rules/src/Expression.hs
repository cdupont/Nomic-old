
{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, GADTs,
    UndecidableInstances, DeriveDataTypeable, FlexibleContexts,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies,
    TypeSynonymInstances, TemplateHaskell #-}

-- test
-- | This module defines an Obs, which are everything that can be observed by a player'r rules over the state of the game.
module Expression where

import Happstack.State
import Data.Typeable
--import Data.Binary.Get
--import Data.Binary.Put
import Data.Ratio
import Control.Monad.State
import Data.List
import Control.Concurrent.STM
import Language.Haskell.Interpreter.Server

type PlayerNumber = Int
type PlayerName = String
type RuleNumber = Int
type RuleName = String
type RuleText = String
type RuleCode = String


data PlayerInfo = PlayerInfo { playerNumber :: PlayerNumber,
                               playerName   :: String}
                               deriving (Eq, Typeable)

type GameName = String
type Code = String
type Variable = (RuleNumber, String, Int)
type Output = (PlayerNumber, String)

-- | The state of the game:
data Game = Game { gameName      :: GameName,
                   rules         :: [Rule],
                   actionResults :: [Action],
                   players       :: [PlayerInfo],
                   variables     :: [Variable],
                   events        :: [Event],
                   outputs       :: [Output],
                   victory       :: [PlayerNumber]}
                   deriving (Typeable)



-- | An informationnal structure about a rule:
data Rule = Rule { rNumber       :: RuleNumber,       -- number of the rule (must be unique) TO CHECK
                   rName         :: RuleName,         -- short name of the rule 
                   rDescription  :: String,           -- description of the rule
                   rProposedBy   :: PlayerNumber,     -- player proposing the rule
                   rRuleCode     :: Code,           -- code of the rule as a string
                   rRuleFunc     :: RuleFunc,         -- function representing the rule (interpreted from rRuleCode)
                   rStatus       :: RuleStatus,       -- status of the rule
                   rejectedBy    :: Maybe RuleNumber} -- who rejected this rule
                   deriving (Typeable)

-- | the status of a rule.
data RuleStatus = Active      -- The current Constitution
                | Pending     -- Proposed rules
                | Rejected    -- Proposed and rejected rules
                | Suppressed  -- Once Active but suppressed rules
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
     NewVar     :: String -> Int -> Exp Bool
     DelVar     :: String -> Exp Bool
     ReadVar    :: String -> Exp (Maybe Int)
     WriteVar   :: String -> Int -> Exp Bool
     OnEvent    :: EventEnum -> Exp () -> Exp ()
     SendMessage :: String -> Exp ()
     Const      :: a -> Exp a
     InputChoice:: Exp String -> Exp PlayerNumber -> Exp [String] -> Exp String
     Bind       :: Exp a -> (a -> Exp b) -> Exp b
     Output     :: PlayerNumber -> String -> Exp ()
     AddRule    :: Rule -> Exp Bool
     DelRule    :: RuleNumber -> Exp Bool
     ModifyRule :: RuleNumber -> Rule -> Exp Bool
     GetRules   :: Exp [Rule]
     SetVictory :: [PlayerNumber] -> Exp ()
     GetPlayers :: Exp [PlayerInfo]
     deriving (Typeable)

instance Version (Exp ())
instance Serialize (Exp ()) where
           getCopy = undefined --contain $ (Const ())
           putCopy e = contain $ safePut (1::Int)




--     Equ        :: (Eq a, Show a, Typeable a) => Exp a -> Exp a -> Exp Bool
--     Plus       :: (Num a) => Exp a -> Exp a -> Exp a
--     Minus      :: (Num a) => Exp a -> Exp a -> Exp a
--     Time       :: (Num a) => Exp a -> Exp a -> Exp a
--     Div        :: (Fractional a) => Exp a -> Exp a -> tVa
--     And        :: Exp Bool -> Exp Bool -> Exp Bool
--     Not        :: Exp Bool -> Exp Bool
--     If         :: Exp Bool -> Exp a -> Exp a -> Exp a
--     Lt         :: Exp a => Exp a -> Exp a -> Exp Bool
--     Map        :: (Exp a -> Exp b) -> Exp [a] -> Exp [b]
--     Foldr      :: (Exp a -> Exp b -> Exp b) -> Exp b -> Exp [a] -> Exp b
--     Cons       :: (Eq a, Show a) => Exp a -> Exp [a] -> Exp [a]
--     Nil        :: Exp [a]

type Event = (RuleNumber, EventEnum, Exp ())

data EventEnum = NewPlayer PlayerNumber
           | PlayerLeave PlayerNumber
           | Time Int
           | RuleAdded RuleNumber
           | RuleSuppressed RuleNumber
           | RuleModified RuleNumber
           | Message String
           | UserEvent PlayerNumber String
           | Victory [PlayerNumber]
           deriving (Eq, Show, Typeable)


instance Monad Exp where
   return = Const
   (>>=) = Bind


newtype RuleFunc = RuleFunc {ruleFunc :: Maybe Rule -> (Exp Bool)} deriving (Typeable)

type MetaRule = Rule -> Exp Bool
type NormalRule = Exp ()
data RuleFunc2 = MR MetaRule | NR NormalRule

giveVictory :: NormalRule
giveVictory = SetVictory []

g :: RuleFunc2
g = NR giveVictory


-- A rule can change the state of the game
-- A rule can assess the legality of something
class (Monad ruleType) => (Rule2 ruleType s) where
    changeState :: ruleType ()
    legal :: s -> ruleType Bool
    legal _ = return True


instance Version RuleStatus
$(deriveSerialize ''RuleStatus)




--type OB = Exp Bool
--instance Version OB
-- $(deriveSerialize ''OB)

--TODO: finish this as it may avoid good serialization of actions and rules.
--instance Serialize (Obs Bool) where
--           getCopy = contain $ return $ Konst True
--           putCopy = contain . (\_ -> return ())
--
--instance Methods (Obs Bool) where
--   methods _ = []
--
--instance Component (Obs Bool) where
--    type Dependencies (Obs Bool) = End
--    initialValue = undefined

--type OS = Obs String
--instance Version OS
-- $(deriveSerialize ''OB)

----TODO: finish this as it may avoid good serialization of actions and rules.
--instance Serialize (Obs String) where
--           getCopy = contain $ return $ Konst ""
--           putCopy = contain . (\_ -> return ())

--instance Methods (Obs String) where
--   methods _ = []

--instance Component (Obs String) where
--    type Dependencies (Obs String) = End
--    initialValue = undefined

-- $(mkMethods ''OB [])
--instance Methods (Obs Bool) where
--    methods _ = [Update (\ SuccVal -> succVal),
--                -- Update (\ MySucc -> mySucc),
--                 Query (\ GetVal -> getVal)]

--            putCopy _       = contain
--                            (case inp[a1Q4] of {
--                               Obs arg[a1Q5]
--                                 -> do { Data.Binary.Put.putWord8 0;
--                                         safePut arg[a1Q5] } })
--            getCopy = contain .
--                          (do { c[a1Q6] <- Data.Binary.Get.getWord8;
--                                case c[a1Q6] of {
--                                  0 -> do { arg[a1Q7] <- safeGet;
--                                            return (Obs arg[a1Q7]) }
--                                  _ -> error "Wrong serialization type" }})

--infixl 9 !, !.
--infix  4 ==., /=., <., <=., >., >=.
--infixl 3 &&.
--infixl 2 ||.
--infixr 5 .:.
--
---- | helpers
--proposedBy      = ProposedBy
--ruleNumber      = RuleNumber
--ruleOfficial    = Official
--selfNumber      = SelfNumber
--not_            = Not
--(==.)           = Equ
--(&&.)           = And
--if_             = If
const_           = Const
--(.:.)           = Cons
--(<>.)           = Nil
--foldr_          = Foldr
--map_            = Map
--and_            = foldr_ (&&.) true
--or_             = foldr_ (||.) false
--
---- | True term.
--true :: Obs Bool
--true = konst True
--
---- | False term.
--false :: Obs Bool
--false = konst False
--
--
---- | Logical OR.
--(||.) :: Obs Bool -> Obs Bool -> Obs Bool
--(||.) a b = not_ $ not_ a &&. not_ b
--
---- | True iff the predicate is true for all elements.
--all_ :: (Obs a -> Obs Bool) -> Obs [a] -> Obs Bool
--all_ f = and_ . map_ f
--
---- | True iff the predicate is true for any element.
--any_ :: (Obs a -> Obs Bool) -> Obs [a] -> Obs Bool
--any_ f = or_ . map_ f
--
---- Logical implication (if a then b).
--imply :: Obs Bool -> Obs Bool -> Obs Bool
--imply = (||.) . not_
--
---- | Not equal.
--(/=.) :: (Eq a, Show a, Typeable a) =>  Obs a -> Obs a -> Obs Bool
--a /=. b = not_ (a ==. b)
--
---- | Less than.
--(<.) :: (Ord a) => Obs a -> Obs a -> Obs Bool
--(<.) = Lt
--
---- | Greater than.
--(>.) :: (Ord a) => Obs a -> Obs a -> Obs Bool
--a >. b = b <. a
--
---- | Less than or equal.
--(<=.) :: (Ord a) => Obs a -> Obs a -> Obs Bool
--a <=. b =  not_ (a >. b)
--
---- | Greater than or equal.
--(>=.) :: (Ord a) => Obs a -> Obs a -> Obs Bool
--a >=. b = not_ (a <. b)
--
---- | Returns the minimum of two numbers.
--min_ :: (Ord a) => Obs a -> Obs a -> Obs a
--min_ a b = if_ (a <=. b) a b
--
---- | Returns the minimum of a list of numbers.
----minimum_ :: Obs [Obs a] -> Obs a
----minimum_ = foldl1_ min_
--
---- | Returns the maximum of two numbers.
--max_ :: (Ord a) => Obs a -> Obs a -> Obs a
--max_ a b = if_ (a >=. b) a b
--
---- | Returns the maximum of a list of numbers.
----maximum_ :: OrdE a => Obs [a] -> Obs a
----maximum_ = foldl_ max_
--
---- | Limits between min and max.
--limit :: (Ord a) => Obs a -> Obs a -> Obs a -> Obs a
--limit a b i = max_ min $ min_ max i
--  where
--  min = min_ a b
--  max = max_ a b
--
--genericLength_  = foldr_ (const (+1)) 0
--sum_ as         = foldr_ (+) 0
--
--filter_ :: (Eq a, Show a) => (Obs a -> Obs Bool) -> Obs [a] -> Obs [a]
--filter_ p = foldr_ (\x xs -> If (p x) xs (x .:. xs) ) Nil




--oVoteReason :: Obs String -> Obs PlayerNumber -> Obs Bool
--oVoteReason s pn = oTwoChoiceVote s pn ==. for
--
--oVote :: Obs PlayerNumber -> Obs Bool
--oVote = oVoteReason (Konst "Please Vote")
--
--oAllVote :: Obs [Bool]
--oAllVote = Map oVote AllPlayers
--
--oUnanimityVote :: Obs Bool
--oUnanimityVote = and_ oAllVote
--
--oGetPositiveVotes :: Obs [Bool]
--oGetPositiveVotes = filter_ (==. true) oAllVote
--
--oQuorumVote :: (Num a, Ord a, Typeable a) => Obs a -> Obs Bool
--oQuorumVote q = genericLength_ oGetPositiveVotes >. q
--
--fors     = filter_ (==. for)
--againsts = filter_ (==. against)
--blanks   = filter_ (==. blank)
--
--oPercentageVote :: (Fractional p, Ord p, Typeable p) => Obs [String] -> Obs p -> Obs Bool
--oPercentageVote l p = (nbFors / (nbFors + nbAgainst)) >=. p
--   where nbFors    = genericLength_ $ fors l
--         nbAgainst = genericLength_ $ againsts l
--
----oGetQuorum :: (Num a, Ord a) => Obs a -> Obs Bool
----oGetQuorum p =
--
--erase :: Obs Int -> Obs Bool
--erase n = ruleNumber /=. n
--
--autoErase :: Obs Bool
--autoErase = erase selfNumber
--
--instance Bounded a => Bounded (Obs a) where
--   minBound = Konst minBound
--   maxBound = Konst maxBound
--
--instance (Num a, Ord a, Typeable a) => Num (Obs a) where
--    (+) = Plus
--    (*) = Time
--    (-) = Minus
--    negate a = 0 - a
--    abs a = if_ (a <. 0) (negate a) a
--    signum a = if_ (a ==. 0) 0 $ if_ (a <. 0) (-1) 1
--    fromInteger = konst . fromInteger
--
--instance (Ord a, Num a, Fractional a, Typeable a) => Fractional (Obs a) where
--  (Konst a) / (Konst b) = konst $ a / b
--  a / b = Div a b
--  recip a = 1 / a
--  fromRational r = Konst $ fromInteger (numerator r) / fromInteger (denominator r)
--
--
---- instance Functor (Obs) where
----     fmap f RuleProposedBy = f 
--
---- TODO: implement with awesomePrelude
----instance Num a => Enum (Obs a) where
----   succ a = a + (Konst 1)
----   pred a = a - (Konst 1)
----   toEnum a = (Konst a)
----   fromEnum (Konst a) = a
----   enumFrom (OInt a) = map toEnum [a..]
----   enumFromThen a b = AllPlayers
----   enumFromTo (OInt a) (OInt b) = map toEnum [a..b]
----   enumFromThenTo (OInt a) (OInt b) (OInt c) = map toEnum [a, b..c]
--
--
--instance (Show t) => Show (Obs t) where
--     show ProposedBy  = "ProposedBy"
--     show RuleNumber  = "RuleNumber"
--     show SelfNumber  = "SelfNumber"
--     show Official    = "Official"
--     show (Equ a b)   = show a ++ " Eq " ++ show b
--     show (Plus a b)  = show a ++ " Plus " ++ show b
--     show (Minus a b) = show a ++ " Minus " ++ show b
--     show (Time a b)  = show a ++ " Time " ++ show b
--     show (Konst a)   = " (Konst " ++ show a ++ ")"
--     show (And a b)   = show a ++ " And " ++ show b
--     show (Not a)     = " (Not " ++ show a ++ ")"
--     show (If a b c)  = "If " ++ show a ++ " Then " ++ show b ++ " Else " ++ show c
--     show (InputChoice a b c)  = "InputChoice " ++ show a ++ show b ++ show b
--     show (Cons a b)  = "Cons " ++ show a ++ show b
--     show (Nil)       = "Nil "
--     --show (Map f as)  = "Map (function) " ++ (show as)
--
----deriving instance (Show a) => Show (Obs a)
--
----deriving instance (Read a) => Read (Obs a)
--
--instance Typeable1 Obs where
--    typeOf1 _ = mkTyConApp (mkTyCon "Obs") []
--
---- | an equality that tests also the types.
--(===) :: (Typeable a, Typeable b, Eq b) => a -> b -> Bool
--(===) x y = cast x == Just y
--
--
--instance Eq t => Eq (Exp t) where
--     ProposedBy == ProposedBy = True
--     RuleNumber == RuleNumber = True
--     SelfNumber == SelfNumber = True
--     Official == Official     = True
--     Equ a b == Equ c d       = (a,b) === (c,d)	--i'm obliged to used this === because due to Equ's type, the compiler can't infer that the types of a and c are egual (resp. b and d).
--     Plus a b == Plus c d     = (a,b) == (c,d)	
--     Minus a b == Minus c d   = (a,b) == (c,d)	
--     Time a b == Time c d     = (a,b) == (c,d)	
--     And a b == And c d       = (a,b) == (c,d)	
--     Not a == Not b           = a == b
--     Konst a == Konst b       = a == b
--     If a b c == If d e f     = (a,b,c) == (d,e,f)	
--     InputChoice a b c == InputChoice d e f  = (a,b,c) == (d,e,f)		
--     Nil == Nil               = True	
--     Cons a b == Cons c d     = (a,b) == (c,d)
--     _ == _                   = False


-- | Replaces all instances of a value in a list by another value.
replaceWith :: (a -> Bool)   -- ^ Value to search
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replaceWith f y = map (\z -> if f z then y else z)


---- TODO simplify...
--evalObs (Map f obs) nr sn = liftE (map (eval . f . Konst)) (evalObs obs nr sn)
--   >>= either (return . Left)
--              (sequence >=> return . T.sequenceA)
--   where eval a = evalObs a nr sn
--
--
--evalObs (Foldr f obs lobs) nr sn = liftE2 (\a b -> eval $ foldr f (Konst a) (map Konst b)) (evalObs obs nr sn) (evalObs lobs nr sn)
--   >>= either (return . Left)
--              id
--   where eval a = evalObs a nr sn
