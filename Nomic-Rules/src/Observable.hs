
{-# LANGUAGE NoMonomorphismRestriction,
             FlexibleInstances,
             GADTs,
             StandaloneDeriving#-}

-- | This module defines an Obs, which are everything that can be observed by a player'r rules over the state of the game.
module Observable where

import Data.Typeable


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
     Konst      :: a -> Obs a
     Map        :: (Obs a -> Obs b) -> Obs [a] -> Obs [b]
     Foldr      :: (Obs a -> Obs b -> Obs b) -> Obs b -> Obs [a] -> Obs b
     Vote       :: Obs String -> Obs Int -> Obs Bool



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
oVote           = Vote
oListAnd        = Foldr oAnd (oConst True) 
oListOr         = Foldr oOr (oConst False)


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

--instance Enum Obs where      --TODO correct
--   succ a = a + (oConst 1)
--   pred a = a - (oConst 1)
--   toEnum a = (oInt a)
--   fromEnum (OInt a) = a
--   enumFrom (OInt a) = map toEnum [a..]
--   enumFromThen (OInt a) (OInt b) = map toEnum [a, b..]
--   enumFromTo (OInt a) (OInt b) = map toEnum [a..b]
--   enumFromThenTo (OInt a) (OInt b) (OInt c) = map toEnum [a, b..c]


instance Show t => Show (Obs t) where
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
     show (Vote a b)  = "Vote " ++ (show a) ++ (show b)

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
     Vote a b == Vote c d     = (a,b) == (c,d)	
     _ == _                   = False


                                  
     
