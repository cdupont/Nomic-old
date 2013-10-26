{-# LANGUAGE GADTs, ScopedTypeVariables, TupleSections #-}

-- | All the building blocks to allow rules to manage players.
module Language.Nomyx.Players where

import Language.Nomyx.Expression
import Language.Nomyx.Events
import Language.Nomyx.Variables
import Language.Nomyx.Rule
import Data.Typeable
import Data.List
import Data.Lens
import Control.Applicative
import Control.Arrow

-- * Players

-- | get all the players
getPlayers :: Nomex [PlayerInfo]
getPlayers = GetPlayers

-- | Get a specific player
getPlayer :: PlayerNumber -> Nomex (Maybe PlayerInfo)
getPlayer pn = do
   pls <- GetPlayers
   return $ find ((== pn) . getL playerNumber) pls

-- | Set the name of a player
getPlayerName :: PlayerNumber -> Nomex (Maybe PlayerName)
getPlayerName pn = do
  p <- getPlayer pn
  return $ _playerName <$> p

-- | Set the name of a player
setPlayerName :: PlayerNumber -> PlayerName -> Nomex Bool
setPlayerName = SetPlayerName

modifyPlayerName :: PlayerNumber -> (PlayerName -> PlayerName) -> Nomex Bool
modifyPlayerName pn f = do
   mn <- getPlayerName pn
   case mn of
      Just name -> setPlayerName pn (f name)
      Nothing -> return False


-- | Get the total number of players
getPlayersNumber :: Nomex Int
getPlayersNumber = length <$> getPlayers

-- | Get all the players number
getAllPlayerNumbers :: Nomex [PlayerNumber]
getAllPlayerNumbers = map _playerNumber <$> getPlayers

-- | Remove the player from the game (kick)
delPlayer :: PlayerNumber -> Nomex Bool
delPlayer = DelPlayer


-- | perform an action for each current players, new players and leaving players
forEachPlayer :: (PlayerNumber -> Nomex ()) -> (PlayerNumber -> Nomex ()) -> (PlayerNumber -> Nomex ()) -> Nomex ()
forEachPlayer action actionWhenArrive actionWhenLeave = do
    pns <- getAllPlayerNumbers
    mapM_ action pns
    onEvent_ (Player Arrive) $ \(PlayerData p) -> actionWhenArrive $ _playerNumber p
    onEvent_ (Player Leave) $ \(PlayerData p) -> actionWhenLeave $ _playerNumber p

-- | perform the same action for each players, including new players
forEachPlayer_ :: (PlayerNumber -> Nomex ()) -> Nomex ()
forEachPlayer_ action = forEachPlayer action action (\_ -> return ())

-- | create a value initialized for each players
--manages players joining and leaving
createValueForEachPlayer :: forall a. (Typeable a, Show a, Eq a) => a -> MsgVar [(PlayerNumber, a)] -> Nomex ()
createValueForEachPlayer initialValue mv = do
    pns <- getAllPlayerNumbers
    v <- newMsgVar_ (getMsgVarName mv) $ map (,initialValue::a) pns
    forEachPlayer (const $ return ())
                  (\p -> modifyMsgVar v ((p, initialValue) : ))
                  (\p -> modifyMsgVar v $ filter $ (/= p) . fst)

-- | create a value initialized for each players initialized to zero
--manages players joining and leaving
createValueForEachPlayer_ :: MsgVar [(PlayerNumber, Int)] -> Nomex ()
createValueForEachPlayer_ = createValueForEachPlayer 0

getValueOfPlayer :: forall a. (Typeable a, Show a, Eq a) => PlayerNumber -> MsgVar [(PlayerNumber, a)] -> Nomex (Maybe a)
getValueOfPlayer pn var = do
   value <- readMsgVar_ var
   return $ lookup pn value

modifyValueOfPlayer :: (Eq a, Show a, Typeable a) => PlayerNumber -> MsgVar [(PlayerNumber, a)] -> (a -> a) -> Nomex ()
modifyValueOfPlayer pn var f = modifyMsgVar var $ map $ (\(a,b) -> if a == pn then (a, f b) else (a,b))

modifyAllValues :: (Eq a, Show a, Typeable a) => MsgVar [(PlayerNumber, a)] -> (a -> a) -> Nomex ()
modifyAllValues var f = modifyMsgVar var $ map $ second f

-- | show a player name based on his number
showPlayer :: PlayerNumber -> Nomex String
showPlayer pn = do
   mn <- getPlayerName pn
   case mn of
      Just name -> return name
      Nothing -> return ("Player " ++ (show pn))


-- | set victory to a list of players
setVictory :: [PlayerNumber] -> Nomex ()
setVictory = SetVictory

-- | give victory to one player
giveVictory :: PlayerNumber -> Nomex ()
giveVictory pn = SetVictory [pn]


getSelfProposedByPlayer :: Nomex PlayerNumber
getSelfProposedByPlayer = getSelfRule >>= return . _rProposedBy

