{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators, TypeSynonymInstances, GADTs #-}
-- | This module defines actions.
module Action where

import Observable
import Data.List
import NamedRule
import Happstack.State
import Data.Typeable
import Control.Monad.State.Class (MonadState(..))
import Data.Binary.Put
import Data.Binary.Get

type ActionType = Obs String
type ActionNumber = Int
type ActionResult = String


-- | an action is a part of a rule that needs a player's input.
data Action = Action { testing :: RuleNumber,
                       tested :: RuleNumber,
                       action :: ActionType,
                       result :: Maybe ActionResult}
                       deriving (Eq, Show, Typeable)



instance Version Action
$(deriveSerialize ''Action)

instance Version ActionType
-- $(deriveSerialize ''OB)

--TODO: finish this as it may avoid good serialization of actions and rules.
instance Serialize ActionType where
           getCopy = contain $ getc
           putCopy = contain . putc


putc :: Obs String -> Data.Binary.Put.Put
putc (InputChoice (Konst s) (Konst pn) (Konst ss)) = safePut s >> safePut pn >> safePut ss

getc :: Data.Binary.Get.Get ActionType
getc = do
   s <- safeGet
   pn <- safeGet
   ss <- safeGet
   return $ InputChoice (Konst s) (Konst pn) (Konst ss)

instance Methods ActionType where
   methods _ = []

--instance Component (Obs String) where
--    type Dependencies (Obs String) = End
--    initialValue = undefined


-- | find the result of an action (the Obs) in the list.
findActionResult :: ActionType -> NamedRule -> RuleNumber -> [Action] -> Maybe Action
findActionResult o nr sn as = find (\Action { testing = testing,
                                              tested = tested,
                                              action = action} -> testing == sn &&
                                                                  tested == rNumber nr &&
                                                                  action == o) as

-- instances


--instance Monoid a => Applicative (Either a) where
--        pure x = Right x
--        (Right f) <*> (Right x) = Right $ f x
--        (Right _) <*> (Left u) = Left u
--        (Left u) <*> (Right _) = Left u
--        (Left u) <*> (Left v) = Left $ u `mappend` v

instance Monad (Either [Action]) where
        return x = Right x
        (Right x) >>= f = f x
        (Left u) >>= _ = Left u


