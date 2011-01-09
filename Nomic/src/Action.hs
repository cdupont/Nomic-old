{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators #-}
-- | This module defines actions.
module Action where

import Observable
import Control.Applicative
import Data.List
import NamedRule
import Happstack.State
import Data.Typeable
import Data.Monoid


type ActionNumber = Int
type ActionResult = Bool

-- | an action is a part of a rule that needs a player's input.
data Action = Action { testing :: RuleNumber,
                       tested :: RuleNumber,
                       action :: Obs Bool,
                       result :: Maybe ActionResult}  -- TODO: generalize.
                       deriving (Eq, Show, Typeable)



instance Version Action
$(deriveSerialize ''Action)

-- | find the result of an action (the Obs) in the list.
findActionResult :: Obs Bool -> NamedRule -> RuleNumber -> [Action] -> Maybe Action
findActionResult o nr sn as = find (\Action { testing = testing,
                                              tested = tested,
                                              action = action} -> testing == sn && tested == rNumber nr && action == o) as

-- instances


instance Monoid a => Applicative (Either a) where
        pure x = Right x
        (Right f) <*> (Right x) = Right $ f x
        (Right _) <*> (Left u) = Left u
        (Left u) <*> (Right _) = Left u
        (Left u) <*> (Left v) = Left $ u `mappend` v

instance Monad (Either [Action]) where
        return x = Right x
        (Right x) >>= f = f x
        (Left u) >>= _ = Left u


