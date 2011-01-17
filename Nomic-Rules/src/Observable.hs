
{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, GADTs, UndecidableInstances,
             StandaloneDeriving, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeSynonymInstances #-}

-- | This module defines an Obs, which are everything that can be observed by a player'r rules over the state of the game.
module Observable where

import Happstack.State
import Data.Typeable
import Data.Binary.Get
import Data.Binary.Put

type PlayerNumber = Int
type PlayerName = String
type RuleNumber = Int


-- | an Obs allows the player's rule to have access to the state of the game.
-- | it is a compositional algebra defined with a GADT.
data Obs a where
     ProposedBy :: Obs PlayerNumber -- The player that proposed the tested rule
     RuleNumber :: Obs RuleNumber   -- The number of the tested rule
     SelfNumber :: Obs RuleNumber   -- The number of the testing rule
     Official   :: Obs Bool         -- whereas the tested rule is official
     AllPlayers :: Obs [PlayerNumber]
     Equ        :: (Eq a, Show a, Typeable a) => Obs a -> Obs a -> Obs Bool
     Plus       :: (Num a) => Obs a -> Obs a -> Obs a
     Time       :: (Num a) => Obs a -> Obs a -> Obs a
     Minus      :: (Num a) => Obs a -> Obs a -> Obs a
     And        :: Obs Bool -> Obs Bool -> Obs Bool
     Or         :: Obs Bool -> Obs Bool -> Obs Bool
     Not        :: Obs Bool -> Obs Bool
     If         :: Obs Bool -> Obs a -> Obs a -> Obs a
     Lt         :: Ord a => Obs a -> Obs a -> Obs Bool
     Konst      :: a -> Obs a
     Map        :: (Obs a -> Obs b) -> Obs [a] -> Obs [b]
     Foldr      :: (Obs a -> Obs b -> Obs b) -> Obs b -> Obs [a] -> Obs b
     Cons       :: (Eq a, Show a) => Obs a -> Obs [a] -> Obs [a]
     Nil        :: Obs [a]
     InputChoice:: Obs String -> Obs PlayerNumber -> Obs [String] -> Obs String


type OB = Obs Bool
instance Version OB
-- $(deriveSerialize ''OB)

--TODO: finish this as it may avoid good serialization of actions and rules.
instance Serialize (Obs Bool) where
           getCopy = contain $ return $ Konst True
           putCopy = contain . (\_ -> return ())

instance Methods (Obs Bool) where
   methods _ = []

instance Component (Obs Bool) where
    type Dependencies (Obs Bool) = End
    initialValue = undefined

type OS = Obs String
instance Version OS
-- $(deriveSerialize ''OB)

--TODO: finish this as it may avoid good serialization of actions and rules.
instance Serialize (Obs String) where
           getCopy = contain $ return $ Konst ""
           putCopy = contain . (\_ -> return ())

instance Methods (Obs String) where
   methods _ = []

instance Component (Obs String) where
    type Dependencies (Obs String) = End
    initialValue = undefined

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

-- | helpers
oRuleProposedBy = ProposedBy
oRuleNumber     = RuleNumber
oRuleOfficial   = Official
--oNbPlayer       = OpZero "NbPlayer"
oSelfNumber     = SelfNumber
oNot            = Not
oEqu            = Equ
oPlus           = Plus
oTime           = Time
oMinus          = Minus
oAnd            = And
oOr             = Or
oIf             = If
oConst          = Konst
--oVote           = Vote
oListAnd        = Foldr oAnd (oConst True) 
oListOr         = Foldr oOr (oConst False)
oCons           = Cons
oNil            = Nil
--oLength a       = Foldr (oConst True) a

--oSum :: Num a => Obs [a] -> Obs a
--oSum as = Foldr


oTwoChoiceVote s pn   = InputChoice s pn (Cons (Konst "For") (Cons (Konst "Against") Nil))
oThreeChoiceVote s pn = InputChoice s pn (Cons (Konst "For") (Cons (Konst "Against") (Cons (Konst "Blank") Nil)))

oVoteReason :: Obs String -> Obs PlayerNumber -> Obs Bool
oVoteReason s pn = (oTwoChoiceVote s pn) `oEqu` (Konst "For")

oVote :: Obs PlayerNumber -> Obs Bool
oVote pn         = oVoteReason (Konst "Please Vote") pn

--oEnumFromTo     = OpBi "enumFromTo"


instance Bounded a => Bounded (Obs a) where
   minBound = Konst $ minBound
   maxBound = Konst $ minBound

instance (Num a) => Num (Obs a) where
    (+) = oPlus
    (*) = oTime
    (-) = oMinus
    abs = id --TODO correct
    signum = const 1
    fromInteger = oConst . fromInteger

-- instance Functor (Obs) where
--     fmap f RuleProposedBy = f 

-- TODO: implement with awesomePrelude
--instance Num a => Enum (Obs a) where
--   succ a = a + (Konst 1)
--   pred a = a - (Konst 1)
--   toEnum a = (Konst a)
--   fromEnum (Konst a) = a
--   enumFrom (OInt a) = map toEnum [a..]
--   enumFromThen a b = AllPlayers
--   enumFromTo (OInt a) (OInt b) = map toEnum [a..b]
--   enumFromThenTo (OInt a) (OInt b) (OInt c) = map toEnum [a, b..c]


instance (Show t) => Show (Obs t) where
     show ProposedBy  = "ProposedBy"
     show RuleNumber  = "RuleNumber"
     show SelfNumber  = "SelfNumber"
     show Official    = "Official"
     show (Equ a b)   = (show a) ++ " Eq " ++ (show b)
     show (Plus a b)  = (show a) ++ " Plus " ++ (show b)
     show (Minus a b) = (show a) ++ " Minus " ++ (show b)
     show (Time a b)  = (show a) ++ " Time " ++ (show b)
     show (Konst a)   = " (Konst " ++ (show a) ++ ")"
     show (And a b)   = (show a) ++ " And " ++ (show b)
     show (Or a b)    = (show a) ++ " Or " ++ (show b)
     show (Not a)     = " (Not " ++ (show a) ++ ")"
     show (If a b c)  = "If " ++ (show a) ++ " Then " ++ (show b) ++ " Else " ++ (show c)
     show (InputChoice a b c)  = "InputChoice " ++ (show a) ++ (show b) ++ (show b)
     show (Cons a b)  = "Cons " ++ (show a) ++ (show b)
     show (Nil)       = "Nil "

--deriving instance (Show a) => Show (Obs a)

--deriving instance (Read a) => Read (Obs a)

instance Typeable1 Obs where
    typeOf1 _ = mkTyConApp (mkTyCon "Obs") []

-- | an equality that tests also the types.
(===) :: (Typeable a, Typeable b, Eq b) => a -> b -> Bool
(===) x y = cast x == Just y


instance Eq t => Eq (Obs t) where
     ProposedBy == ProposedBy = True
     RuleNumber == RuleNumber = True
     SelfNumber == SelfNumber = True
     Official == Official     = True
     Equ a b == Equ c d       = (a,b) === (c,d)	--i'm obliged to used this === because due to Equ's type, the compiler can't infer that the types of a and c are egual (resp. b and d).
     Plus a b == Plus c d     = (a,b) == (c,d)	
     Minus a b == Minus c d   = (a,b) == (c,d)	
     Time a b == Time c d     = (a,b) == (c,d)	
     And a b == And c d       = (a,b) == (c,d)	
     Or a b == Or c d         = (a,b) == (c,d)	
     Not a == Not b           = a == b
     Konst a == Konst b       = a == b
     If a b c == If d e f     = (a,b,c) == (d,e,f)	
     InputChoice a b c == InputChoice d e f  = (a,b,c) == (d,e,f)		
     Nil == Nil               = True	
     Cons a b == Cons c d     = (a,b) == (c,d)
     _ == _                   = False


                                  
     
