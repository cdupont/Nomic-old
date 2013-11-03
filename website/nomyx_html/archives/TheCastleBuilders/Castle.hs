{-# LANGUAGE TupleSections, DeriveDataTypeable, RankNTypes, ScopedTypeVariables #-}


module Castle where

import Prelude
import Language.Nomyx
import Data.Typeable
import qualified Money2
import Language.Nomyx.Examples (accounts)

-- | castle structure
data Castle = Castle { towers :: Int, dungeon :: Bool }
              deriving (Typeable, Show, Eq)

-- | castle variable name and type
castles :: MsgVar [(PlayerNumber, Castle)]
castles = msgVar "Castles"

-- | Create a castle for each players
createCastles :: RuleFunc
createCastles = voidRule $ createValueForEachPlayer' (Castle 0 False) castles

-- | Permanently display the castles
displayCastles :: RuleFunc
displayCastles = voidRule $ forEachPlayer_ displayPlayerCastles

displayPlayerCastles :: PlayerNumber -> Nomex ()
displayPlayerCastles pn = do
   sp <- showPlayer
   let displayCastle (i, (Castle towers dungeon)) = (sp i) ++ "\t" ++ (show towers) ++ " towers and " ++ (if dungeon then "a" else "no") ++ " dungeon\n"
   displayVar pn castles (\l -> "Castles:\n" ++ concatMap displayCastle l) where

-- | build some castle parts (towers, dungeon)
buildCastleParts :: Int -> Int -> RuleFunc
buildCastleParts towerPrice dungeonPrice = voidRule $ do
    forEachPlayer_ (askBuildTowers >^> askBuildDungeon)
    where askBuildTowers = Money2.offer "Build Tower" (build (\(Castle t d) -> Castle (t+1) d)) towerPrice
          askBuildDungeon = Money2.offer "Build Dungeon" (build (\(Castle t d) -> Castle t True)) dungeonPrice
          build f pn = modifyValueOfPlayer' pn castles f
          (>^>) = liftM2 (>>)

-- generalized version
modifyValueOfPlayer' :: (Eq a, Show a, Typeable a) => PlayerNumber -> MsgVar [(PlayerNumber, a)] -> (a -> a) -> Nomex ()
modifyValueOfPlayer' pn var f = modifyMsgVar var . map $ \(pn', val) -> if pn == pn' then (pn', f val) else (pn', val)


-- | custom version of createValueForEachPlayer
createValueForEachPlayer' :: forall a. (Typeable a, Show a, Eq a) => a -> MsgVar [(Int, a)] -> Nomex ()
createValueForEachPlayer' initialValue mv = do
    pns <- getAllPlayerNumbers
    v <- newMsgVar_ (getMsgVarName mv) $ map (,initialValue::a) pns
    forEachPlayer (const $ return ())
                  (\p -> modifyMsgVar v ((p, initialValue) : ))
                  (\p -> modifyMsgVar v $ filter $ (/= p) . fst)      
                  import Language.Nomyx
