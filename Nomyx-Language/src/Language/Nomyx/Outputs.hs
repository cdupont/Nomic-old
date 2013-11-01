
-- | All the building blocks to allow rules to produce outputs.
module Language.Nomyx.Outputs where
--   OutputNumber,
--   newOutput, newOutput_,
--   outputAll, outputAll_, outputAll',
--   getOutput, getOutput_,
--   updateOutput, updateOutput_, 
--   delOutput, delOutput_,
--   displayVar, displaySimpleVar,
--   displayArrayVar, showArrayVar,

import Language.Nomyx.Expression
import Language.Nomyx.Variables
import Data.Typeable
import Control.Monad.State


-- * Outputs

-- | outputs a message to one player
newOutput :: Nomex String -> (Maybe PlayerNumber) -> Nomex OutputNumber
newOutput ns mpn = ns >>= NewOutput mpn

newOutput_ :: Nomex String -> (Maybe PlayerNumber) -> Nomex ()
newOutput_ ns pn = void $ newOutput ns pn

-- | output a message to all players
outputAll :: Nomex String -> Nomex OutputNumber
outputAll ns = newOutput ns Nothing

outputAll_ :: Nomex String -> Nomex ()
outputAll_ ns = newOutput_ ns Nothing

outputAll' :: String -> Nomex ()
outputAll' s = newOutput_ (return s) Nothing

getOutput :: OutputNumber -> Nomex (Maybe String)
getOutput on = GetOutput on

getOutput_ :: OutputNumber -> Nomex String
getOutput_ on = partial "getOutput_ : Output number not existing" $ getOutput on

updateOutput :: OutputNumber -> Nomex String -> Nomex Bool
updateOutput on ns = ns >>= UpdateOutput on

updateOutput_ :: OutputNumber -> Nomex String -> Nomex ()
updateOutput_ on ns = void $ updateOutput on ns

delOutput :: OutputNumber -> Nomex Bool
delOutput = DelOutput

delOutput_ :: OutputNumber -> Nomex ()
delOutput_ on = void $ delOutput on

-- permanently display a variable (update display when variable is updated)
displayVar :: (Typeable a, Show a, Eq a) => (Maybe PlayerNumber) -> MsgVar a -> (a -> Nomex String) -> Nomex ()
displayVar mpn mv dis = onMsgVarEvent
   mv
   (\a -> newOutput (dis a) mpn)
   (\a n -> updateOutput_ n (dis a))
   delOutput_

displaySimpleVar :: (Typeable a, Show a, Eq a) => (Maybe PlayerNumber) -> Nomex String -> MsgVar a -> Nomex ()
displaySimpleVar mpn ntitle mv = do
   let title a = do
        t <- ntitle
        return $ (t ++ ": " ++ (show a) ++ "\n")
   displayVar mpn mv title

displayArrayVar :: (Typeable a, Show a, Eq a, Typeable i, Show i, Eq i) => (Maybe PlayerNumber) -> Nomex String -> ArrayVar i a -> Nomex ()
displayArrayVar mpn ntitle mv = displayVar mpn mv (showArrayVar ntitle)

showArrayVar :: (Show a, Show i) => Nomex String -> [(i,a)] -> Nomex String
showArrayVar title l = do
   t <- title
   return $ t ++ "\n" ++ concatMap (\(i,a) -> (show i) ++ "\t" ++ (show a) ++ "\n") l

