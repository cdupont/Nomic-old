{-# LANGUAGE FlexibleInstances, StandaloneDeriving#-}

-- | This module defines actions.
module Action where

import Observable
import Control.Applicative
import Data.List
import NamedRule



type ActionNumber = Int

-- | an action is a part of a rule that needs a player's input.
data Action = Action { testing :: RuleNumber,
                       tested :: RuleNumber,
                       action :: Obs Bool,
                       result :: Maybe Bool}  -- TODO: generalize.
                       deriving Show


type Actions = [Action]


-- | find the result of an action (the Obs) in the list.
findActionResult :: Obs Bool -> NamedRule -> RuleNumber -> Actions -> Maybe Action
findActionResult o nr sn as = find (\Action { testing = testing,
                                              tested = tested,
                                              action = action} -> testing == sn && tested == rNumber nr && action == o) as

-- instances

deriving instance Eq Action

instance Applicative (Either Actions) where
        pure x = Right x
        (Right f) <*> (Right x) = Right $ f x
        (Right _) <*> (Left u) = Left u
        (Left u) <*> (Right _) = Left u
        (Left u) <*> (Left v) = Left $ u ++ v


