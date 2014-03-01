    
-- | All the building blocks to allow rules to produce outputs.
module Language.Nomyx.Outputs (
   OutputNumber,
   newOutput,
   outputAll, outputAll_,
   getOutput, getOutput_,
   updateOutput,
   delOutput,
   displayVar, displaySimpleVar,
   displayArrayVar, showArrayVar
   ) where

import Language.Nomyx.Expression
import Language.Nomyx.Variables
import Data.Typeable
import Control.Monad.State


-- * Outputs

-- | outputs a message to one player
newOutput :: NomexNE String -> (Maybe PlayerNumber) -> Nomex OutputNumber
newOutput ns mpn = liftEffect ns >>= NewOutput mpn

-- | output a message to all players
outputAll :: NomexNE String -> Nomex OutputNumber
outputAll ns = newOutput ns Nothing

outputAll_ :: String -> Nomex ()
outputAll_ s = void $ newOutput (return s) Nothing

getOutput :: OutputNumber -> NomexNE (Maybe String)
getOutput on = GetOutput on

getOutput_ :: OutputNumber -> Nomex String
getOutput_ on = partial "getOutput_ : Output number not existing" $ liftEffect $ getOutput on

updateOutput :: OutputNumber -> NomexNE String -> Nomex Bool
updateOutput on ns = liftEffect ns >>= UpdateOutput on

delOutput :: OutputNumber -> Nomex Bool
delOutput = DelOutput
              
-- permanently display a variable (update display when variable is updated)
displayVar :: (Typeable a, Show a, Eq a) => (Maybe PlayerNumber) -> MsgVar a -> (a -> NomexNE String) -> Nomex EventNumber
displayVar mpn mv dis = onMsgVarChange
   mv
   (\a -> newOutput (dis a) mpn)
   (\a n -> void $ updateOutput n (dis a))
   (\on -> void $ delOutput on)

displaySimpleVar :: (Typeable a, Show a, Eq a) => (Maybe PlayerNumber) -> NomexNE String -> MsgVar a -> Nomex EventNumber
displaySimpleVar mpn ntitle mv = do
   let title a = do
        t <- ntitle
        return $ (t ++ ": " ++ (show a) ++ "\n")
   displayVar mpn mv title

displayArrayVar :: (Typeable a, Show a, Eq a, Typeable i, Show i, Eq i) => (Maybe PlayerNumber) -> NomexNE String -> ArrayVar i a -> Nomex EventNumber
displayArrayVar mpn ntitle mv = displayVar mpn mv (showArrayVar ntitle)

showArrayVar :: (Show a, Show i) => NomexNE String -> [(i,a)] -> NomexNE String
showArrayVar title l = do
   t <- title
   return $ t ++ "\n" ++ concatMap (\(i,a) -> (show i) ++ "\t" ++ (show a) ++ "\n") l

