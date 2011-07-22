{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators, TypeSynonymInstances, GADTs #-}
-- | This module defines actions.
module Action where

import Data.List
import Happstack.State
import Expression


instance Version Action
$(deriveSerialize ''Action)

instance Version ActionType
$(deriveSerialize ''ActionType)

----TODO: finish this as it may avoid good serialization of actions and rules.
--instance Serialize ActionType where
--           getCopy = contain $ getc
--           putCopy (ActionType r pn cs) = contain $ (safePut r >>> safePut pn >> safe
--
--
--putc :: Obs String -> Data.Binary.Put.Put
--putc (InputChoice (Konst s) (Konst pn) (Konst ss)) = safePut s >> safePut pn >> safePut ss
--putc _ = return ()
--
--getc :: Data.Binary.Get.Get ActionType
--getc = do
--   s <- safeGet
--   pn <- safeGet
--   ss <- safeGet
--   return $ InputChoice (Konst s) (Konst pn) (Konst ss)
--
--instance Methods ActionType where
--   methods _ = []

--instance Component (Obs String) where
--    type Dependencies (Obs String) = End
--    initialValue = undefined


-- | find the result of an action (the Exp) in the list.
findActionResult :: ActionType -> RuleNumber -> [Action] -> Maybe Action
findActionResult a ruleNumber as = find (\Action { aRuleNumber = rn,
                                                   action = action} -> ruleNumber == rn &&
                                                                       action == a) as

-- instances


--instance Monoid a => Applicative (Either a) where
--        pure x = Right x
--        (Right f) <*> (Right x) = Right $ f x
--        (Right _) <*> (Left u) = Left u
--        (Left u) <*> (Right _) = Left u
--        (Left u) <*> (Left v) = Left $ u `mappend` v

instance Monad (Either [Action]) where
        return = Right
        (Right x) >>= f = f x
        (Left u) >>= _ = Left u


