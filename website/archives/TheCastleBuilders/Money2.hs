module Money2 (accounts, balance, modifyBalance, hasBalance, offer, transfer) where
 
import Control.Monad.Error
 
import Control.Monad
import Data.Maybe (fromMaybe)
 
import Language.Nomyx
import Language.Nomyx.Examples (accounts)
import Language.Nomyx.Rule
 
import Prelude
 
-- | Retrieve a player's balance
balance :: PlayerNumber -> Nomex Int
balance pn = liftM (fromMaybe 0) $ getValueOfPlayer pn accounts
 
-- | Modifies a player's balance
modifyBalance :: (Int -> Int) -> PlayerNumber -> Nomex ()
modifyBalance f pn = modifyValueOfPlayer pn accounts f
 
-- | Check to see if a player has enough money
hasBalance :: Int -> PlayerNumber -> Nomex Bool
hasBalance amount pn = fmap (>= amount) $ balance pn
 
-- | `transform from to amount` transfers `amount` money from player `from`, to player `to`.
-- 
-- Throws an error on a negative amount.
transfer :: PlayerNumber -> PlayerNumber -> Int -> Nomex ()
transfer from to amount | amount < 0 = throwError "Money.transfer: amount must not be negative."
transfer from to amount = do
  enough <- hasBalance amount from
  if enough then do
    modifyBalance (subtract amount) from
    modifyBalance (subtract amount) to
  else oneTimeMessage from $ "Transaction failed: You don't have " ++ show amount ++ "."
 where oneTimeMessage pn msg = do
         on <- newOutput msg pn
         onInputButton_ "Ok" (\() -> delOutput on >> return ()) pn
 
-- | `offer description action price player` displays a button for `player` offering them the choice
-- to buy perform `action player` for a `price`.
-- 
-- Throws an error on a negative `price`
offer :: String -> (PlayerNumber -> Nomex ()) -> Int -> PlayerNumber -> Nomex ()
offer descr action price pn | price < 0 = throwError "Money.offer: price must not be negative."
offer descr action price pn = onInputButton_ (descr ++ "\t" ++ show price) (const $ buy price action) pn where
   buy price action = do
      enough <- hasBalance price pn
      when enough $ modifyBalance (subtract price) pn >> action pn
