-----------------------------------------------------------------------------
--
-- Module      :  Utils
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
    
module Utils where

import Data.Maybe
import Data.Char
import Control.Monad.State
import Types
import Language.Nomyx
import Language.Nomyx.Game
import Control.Applicative
import Data.Lens
import Control.Category hiding ((.))

   
-- | this function will return just a if it can cast it to an a.
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Replaces all instances of a value in a list by another value.
replace :: Eq a => a   -- ^ Value to search
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)

yes = ["o", "oui", "y", "yes", "v", "vrai", "true"]
toLowerS = map toLower
isYes a = toLowerS a `elem` yes

-- | generic function to say things on transformers like GameState, ServerState etc.
say :: String -> StateT a IO ()
say = lift . putStrLn

findPlayer :: PlayerName -> StateT Multi IO (Maybe PlayerMulti)
findPlayer name = find ((==) name . getL mPlayerName) <$> gets _mPlayers

findPlayer' :: PlayerNumber -> StateT Multi IO (Maybe PlayerMulti)
findPlayer' pn = find ((==) pn . getL mPlayerNumber) <$> gets _mPlayers

nomyxURL :: Network -> String
nomyxURL (Network host port) = "http://" ++ host ++ ":" ++ (show port)

getPlayersName :: PlayerNumber -> Multi -> PlayerName
getPlayersName pn multi = do
   case find (\(PlayerMulti n _ _ _ _ _) -> n==pn) (mPlayers ^$ multi) of
      Nothing -> error "getPlayersName: No player by that number"
      Just pm -> mPlayerName ^$ pm

getPlayersName' :: Game -> PlayerNumber -> PlayerName
getPlayersName' g pn = do
   case find ((==pn) . getL playerNumber) (_players g) of
      Nothing -> error "getPlayersName: No player by that number in that game"
      Just pm -> _playerName pm

-- | returns the game the player is in
getPlayersGame :: PlayerNumber -> Multi -> Maybe LoggedGame
getPlayersGame pn multi = do
        pi <- find ((==pn) . getL mPlayerNumber) (mPlayers ^$ multi)
        gn <- _viewingGame pi
        find ((== gn) . getL (game >>> gameName)) (_games multi)

getPlayersNameMay :: Game -> PlayerNumber -> Maybe PlayerName
getPlayersNameMay g pn = do
   case find ((==pn) . getL playerNumber) (_players g) of
      Nothing -> Nothing
      Just pm -> Just $ _playerName pm




-- | finds the corresponding game in the multistate and replaces it.
modifyGame :: LoggedGame -> StateT Multi IO ()
modifyGame lg = do
   gs <- access games
   case find (== lg) gs of
      Nothing -> error "modifyGame: No game by that name"
      Just oldg -> do
         let newgs = replace oldg lg gs
         games ~= newgs
         return ()



execWithMulti :: UTCTime -> StateT Multi IO () -> Multi -> IO Multi
execWithMulti t ms m = do
   let setTime g = (game >>> currentTime) ^= t $ g
   let m' = games `modL` (map setTime) $ m
   execStateT ms m'





