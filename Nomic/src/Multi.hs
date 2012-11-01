{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators,
    TypeSynonymInstances, FlexibleInstances, GADTs #-}

-- | This module manages multi-player games and commands.
module Multi where
--Multi, PlayerPassword, GetMulti(..), FindPlayer(..), PlayerMulti(..), GetNewPlayerNumber(..), NewPlayerU(..),
--    listGame, newGame, joinGame, leaveGame, subscribeGame, unsubscribeGame, showSubscribtion, showSubGame, newPlayer,
--    submitRule, myCatch, submitRuleI, showConstitution, showAllRules, listPlayers, amendConstitution, showPendingActions,
--    showMyPendingActions, doActionsI, doAction', showCompletedActions, quit, getPendingActions, doAction, games, getPlayersGame,
--    getPlayersName) where


import Prelude hiding (catch)
import Data.List
import Control.Monad.State
import Game
import Utils
import Interpret
import Safe
import Control.Monad.CatchIO
import System.IO.Error hiding (catch)
import Data.Typeable
import Control.Monad.Reader
import Data.Function (on)
import Debug.Trace.Helpers()
import Language.Nomic.Expression
import Data.Time
import Language.Haskell.Interpreter.Server
import Language.Nomic.Evaluation

type PlayerPassword = String

data PlayerMulti = PlayerMulti   { mPlayerNumber :: PlayerNumber,
                                   mPlayerName :: PlayerName,
                                   mPassword :: PlayerPassword,
                                   inGame :: Maybe GameName}
                                   deriving (Eq, Show, Typeable)

--- | A structure to hold the active games and players
data Multi = Multi { games   :: [Game],
                     mPlayers :: [PlayerMulti]}
                     deriving (Eq, Typeable)

instance Show Multi where
   show Multi{games=gs, mPlayers=mps} = show (sort gs) ++ "\n" ++ show (sort mps)

defaultMulti :: Multi
defaultMulti = Multi [] []


-- | A State to pass around active games and players.
-- Furthermore, the output are to be made with Comm to output to the right console.
type MultiState = StateT Multi IO ()

type MultiStateWith a = StateT Multi IO a

-- | An helper function that makes it very clear how to use the state transformer MultiState.
runWithMulti :: Multi -> MultiState -> IO Multi
runWithMulti = flip execStateT


-- | this function will ask you to re-enter your data if it cannot cast it to an a.
--myCatch :: (PlayerNumber -> Comm ()) -> PlayerNumber -> IO ()
--myCatch f pn = catch (f pn) eHandler where
--   eHandler e 
--      | isUserError e = putCom "Aborted with UserError" >> return ()
--      | otherwise     = putCom "Aborted with IOError"   >> return ()

-- | helper function to change a player's ingame status.
mayJoinGame :: Maybe GameName -> PlayerNumber -> [PlayerMulti] -> [PlayerMulti]
mayJoinGame maybename pn pl = case find (\(PlayerMulti mypn _ _ _) -> mypn == pn) pl of
                     Just o -> replace o o{ inGame = maybename} pl
                     Nothing -> pl

newPlayerU :: PlayerMulti -> StateT Multi IO ()
newPlayerU pm = do
   pms <- gets mPlayers
   modify (\multi -> multi { mPlayers = pm : pms})

findPlayer :: PlayerName -> StateT Multi IO (Maybe PlayerMulti)
findPlayer name =  fmap (find (\PlayerMulti {mPlayerName = pn} -> pn==name)) (gets mPlayers)

getNewPlayerNumber :: StateT Multi IO PlayerNumber
getNewPlayerNumber = do
   ps <- gets mPlayers
   return $ length ps + 1


addNewGame :: Game -> StateT Multi IO ()
addNewGame new = modify (\multi@Multi {games=gs} -> multi {games =  new:gs})

getGameByName :: GameName -> StateT Multi IO (Maybe Game)
getGameByName gn =  fmap (find (\(Game {gameName = n}) -> n==gn)) (gets games)

joinGamePlayer :: PlayerNumber -> GameName -> StateT Multi IO ()
joinGamePlayer pn game = modify (\multi -> multi {mPlayers = mayJoinGame (Just game) pn (mPlayers multi)})

leaveGameU :: PlayerNumber -> StateT Multi IO ()
leaveGameU pn = modify (\multi -> multi {mPlayers = mayJoinGame Nothing pn (mPlayers multi)})


--newPlayer :: Comm PlayerNumber
--newPlayer = do
--   name <- putGetComm "Please enter your name:"
--   --find that name among the list
--   mpn <- query $ FindPlayer name
--   pn <- case mpn of
--      Just pl -> do
--         putCom $ "Welcome back, " ++ mPlayerName pl
--         pwd <- putGetComm  "Please enter your password:"
--         case pwd == mPassword pl of
--            True -> do
--               putCom "password OK"
--               return $ mPlayerNumber pl
--            False -> do
--               putCom "password false, please re-enter"
--               newPlayer
--      Nothing -> do
--         putCom "New player"
--         pwd <- putGetComm "Please create a password:"
--         --add the new player to the list
--         pn <- query GetNewPlayerNumber --CDU to check
--         update $ NewPlayerU PlayerMulti { mPlayerNumber = pn, mPlayerName = name, mPassword = pwd, inGame = Nothing}
--         return pn

--   putCom "Entering multi game..."
--   putCom "If your lost, try the help command"
--   return pn


-- | list the active games
listGame :: PlayerNumber -> StateT Multi IO ()
listGame _ = do
   gs <- gets games
   case length gs of
      0 -> say "No active games"
      _ -> do
         say "Active games:"
         say $ concatMap (\g -> gameName g ++ "\n") gs  


---- | sets the name of a player   
--newName :: String -> PlayerNumber -> MultiState
--newName name pn = do
--   m <- get
--   case uniqueName name m of
--      True -> do say $ "setting your new name to: " ++ name
--                 modify (\multi -> multi {players = setName name pn (players multi)})
--      False -> say $ "this name is already used"


--uniqueName :: String -> Multi -> Bool
--uniqueName s m = null $ filter (\p -> playerName p == Just s) (players m)


-- | starts a new game
newGame :: String -> PlayerNumber -> StateT Multi IO ()
newGame name _ = do
   gs <- gets games
   case null $ filter (\p -> gameName p == name) gs of
      True -> do
         say $ "Creating a new game of name: " ++ name
         t <- liftIO getCurrentTime
         -- create a game with zero players
         modify (\m -> m {games = (initialGame name t):gs})
      False -> say $ "this name is already used" 

uniqueGame :: String -> [Game] -> Bool
uniqueGame s gs = null $ filter (\p -> gameName p == s) gs

-- | join a game.
joinGame :: GameName -> PlayerNumber -> StateT Multi IO ()
joinGame game pn = do
   mg <- getGameByName game
   case mg of
      Nothing -> say $ "No game by that name"
      Just g -> do
         say "subscribing first."
         subscribeGame (gameName g) pn      
         say $ "Joining game: " ++ game
         joinGamePlayer pn game


-- | leave a game (you remain subscribed).
leaveGame :: PlayerNumber -> StateT Multi IO  ()
leaveGame pn = do
   leaveGameU pn
   say "You left the game (you remain subscribed)." 


-- | subcribe to a game.
subscribeGame :: GameName -> PlayerNumber -> StateT Multi IO ()
subscribeGame game pn = do
   m <- get
   inGameDo game $ do
      g <- get
      case find (\(PlayerInfo  { playerNumber=mypn}) -> mypn == pn ) (players g) of
         Just _ -> say "Already subscribed!"
         Nothing -> do
            say $ "Subscribing to game: " ++ game
            put g {players = PlayerInfo { playerNumber = pn,
                                          playerName = getPlayersName pn m} : (players g)}


-- | subcribe to a game.
unsubscribeGame :: GameName -> PlayerNumber -> StateT Multi IO ()
unsubscribeGame game pn = inGameDo game $ do
   g <- get
   case find (\(PlayerInfo  { playerNumber=mypn}) -> mypn == pn ) (players g) of
      Nothing -> say "Not subscribed!"
      Just _ -> do
         say $ "Unsubscribing to game: " ++ game
         put g {players = filter (\PlayerInfo { playerNumber = mypn} -> mypn /= pn) (players g)}


showSubGame :: GameName -> PlayerNumber -> StateT Multi IO  ()
showSubGame g _ = inGameDo g $ do
   ps <- gets players
   say $ concatMap show ps

showSubscribtion :: PlayerNumber -> StateT Multi IO  ()
showSubscribtion pn = inPlayersGameDo pn $ do
   ps <- gets players
   say $ concatMap show ps


-- | insert a rule in pending rules.
-- the rules are numbered incrementaly.
submitRule :: String -> String -> String -> PlayerNumber -> ServerHandle -> StateT Multi IO  ()
submitRule name text rule pn sh = inPlayersGameDo pn $ do
   --input the new rule (may fail if ill-formed)
   rs <- gets rules
   mnr <- enterRule (length rs + 1) name text rule pn sh
   case mnr of
      Just nr -> do
         r <- liftT $ evProposeRule nr
         if r == True then say $ "Your rule has been added to pending rules."
             else say $ "Error: Rule could not be proposed"
         return ()
      Nothing -> say $ "Please try again."

inputChoiceResult :: (Eq c, Show c, Typeable c, Read c) => Event(InputChoice c) -> c -> StateT Multi IO  ()
inputChoiceResult ic@(InputChoice pn s cs def) d = inPlayersGameDo pn $ liftT $ evInputChoice ic d

inputChoiceResult' :: EventNumber -> Int -> PlayerNumber -> StateT Multi IO  ()
inputChoiceResult' eventNumber choiceIndex pn = inPlayersGameDo pn $ liftT $ triggerChoice eventNumber choiceIndex


--submitRuleI :: PlayerNumber -> Comm ()
--submitRuleI pn = inPlayersGameDo pn $ do
--   rs <- gets rules
--   name <- lift $ putGetComm "Please enter the rule name:"
--   text <- lift $ putGetComm "Please enter a short description of the rule:"
--   rule <- lift $ putGetComm "Please enter the rule's code:"
--   mnr  <- lift $ enterRule (length rs + 1) name text rule pn
--   case mnr of
--      Just nr -> do
--         modify (\gs@Game {rules=myrs} -> gs {rules = nr:myrs})
--         say $ "Your rule has been added to pending rules."
--      Nothing -> say "Please try again."

-- | finds the corresponding game in the multistate and replaces it.
modifyGame :: Game -> MultiState
modifyGame g = do
   Multi gs ps <- get
   case find (\myg -> gameName g == gameName myg) gs of
      Nothing -> error "modifyGame: No game by that name"
      Just oldg -> do
         let newgs = replace oldg g gs
         put $ Multi newgs ps

-- | reads a rule.
enterRule :: RuleNumber -> String -> String -> String -> PlayerNumber -> ServerHandle -> StateT Game IO (Maybe Rule)
enterRule num name text ruleText pn sh = do
   mrr <- lift $ maybeReadRule ruleText sh
   case mrr of
      Just ruleFunc -> return $ Just Rule {rNumber = num,
                      rName = name,
                      rDescription = text,
                      rProposedBy = pn,
                      rRuleCode = ruleText,
                      rRuleFunc = ruleFunc,
                      rStatus = Pending,
                      rAssessedBy = Nothing}
      Nothing -> return Nothing
        



-- | show the constitution.
showConstitution :: PlayerNumber -> StateT Multi IO ()
showConstitution pn = inPlayersGameDo pn $ get >>= (say  .  show  .  activeRules)


-- | show every rules (including pendings and deleted)
showAllRules :: PlayerNumber -> StateT Multi IO ()	 
showAllRules pn = inPlayersGameDo pn $ get >>= (say . show . rules)

-- | show players      
--listPlayers :: PlayerNumber -> StateT Multi Comm ()
--listPlayers _ = do
--   ps <- gets players
--   case length ps of
--      0 -> say "No players"
--      _ -> do
--         say "Players:"
--         say $ concatMap displayPlayer $ sort ps  

displayPlayer :: PlayerMulti -> String
displayPlayer (PlayerMulti pn name _ (Just game)) = show pn ++ ": " ++ name ++ " in game: " ++ game ++ "\n"
displayPlayer (PlayerMulti pn name _ Nothing)     = show pn ++ ": " ++ name ++ "\n"



-- | propose all pending rules to the current constitution
--amendConstitution :: PlayerNumber -> Comm ()
--amendConstitution pn = inPlayersGameDo pn proposeAllPendings


-- | actions

-- | get all pending actions, deduced from the pending rules.
--getPendingActions :: PlayerNumber -> Comm [Action]
--getPendingActions pn = stateMultiToComm $ do
--   multi <- get
--   let mg = getPlayersGame pn multi
--   case mg of
--      Nothing -> do
--         say "You must be in a game"
--         return []
--      Just g -> lift $ evalStateT pendingActions g


-- | show all pending actions, deduced from the pending rules.
--showPendingActions :: PlayerNumber -> Comm ()
--showPendingActions pn = inPlayersGameDo pn $ do
--   pa <- pendingActions
--   sas <- showActions pa
--   say sas

-- | show only my pending actions, deduced from the pending rules.
--showMyPendingActions :: PlayerNumber -> Comm ()
--showMyPendingActions pn = inPlayersGameDo pn $ do
--   ppa <- playersPendingActions pn
--   sas <- showActions ppa
--   say sas

-- | show all already completed actions.
--showCompletedActions :: PlayerNumber -> Comm ()
--showCompletedActions pn = inPlayersGameDo pn $ do
--   ar <- gets actionResults
--   sas <- showActions ar
--   say sas


-- | ask the player to fulfill his actions in interactive mode
--doActionsI :: PlayerNumber -> Comm ()
--doActionsI pn = inPlayersGameDo pn $ do
--   ppa <- playersPendingActions pn
--   case ppa of
--      [] -> say "No pending actions"
--      _:_ -> do
--         ars <- mapM enterActionResult' ppa
--         modify (\gs@Game {actionResults=oldars} -> gs {actionResults = ars ++ oldars})


-- | fulfill an action
--doAction :: String -> String -> PlayerNumber -> Comm ()  --TODO: solve big conccurency problem. actions to do may change between show and this command.
--doAction actionNumber result pn = inPlayersGameDo pn $ do
--   --input the new rule (may fail if ill-formed)
--   case maybeRead actionNumber of
--      Just an -> do
--         case maybeRead result of
--            Just r -> enterActionResult an r pn
--            Nothing -> say $ "Cannot read result"
--      Nothing -> say $ "Cannot read action number"

-- | fulfill an action
--doAction' :: ActionNumber -> ActionResult -> PlayerNumber -> Comm ()  --TODO: solve big conccurency problem. actions to do may change between show and this command.
--doAction' actionNumber result pn = inPlayersGameDo pn $ enterActionResult actionNumber result pn

-- | create an action
--enterActionResult :: ActionNumber -> ActionResult -> PlayerNumber -> GameState
--enterActionResult actionNumber result pn = do
--   ppa <- playersPendingActions pn
--   case ppa `atMay` (actionNumber - 1) of
--      Just a -> do
--         modify (\gs@Game {actionResults=ars} -> gs {actionResults = (a {result = Just result}):ars})
--         say "Your action result has been stored."
--      Nothing -> say "No pending action by that number"

 
--enterActionResult' :: Action -> GameStateWith Action
--enterActionResult' a = do
--   s <- showAction a
--   say s
--   r <- lift $ safeRead "Please enter your result for this action:"
--   return a {result = Just r}




-- | quit the game
quit :: PlayerNumber -> IO ()
quit _ = putStrLn "quit"

-- | Utility functions

-- | replace the player's name in the list
setName :: String -> PlayerNumber -> [PlayerMulti] -> [PlayerMulti]
setName name pn pl = case find (\(PlayerMulti h _ _ _) -> h == pn) pl of
                        Just o -> replace o o{ mPlayerName = name} pl
                        Nothing -> pl 


-- | returns the game the player is in						
getPlayersGame :: PlayerNumber -> Multi -> Maybe Game
getPlayersGame pn multi = do
        pi <- find (\(PlayerMulti n _ _ _) -> n==pn) (mPlayers multi)
        gn <- inGame pi
        find (\(Game {gameName=name}) -> name==gn) (games multi)

getPlayersName :: PlayerNumber -> Multi -> PlayerName
getPlayersName pn multi = do
   case find (\(PlayerMulti n _ _ _) -> n==pn) (mPlayers multi) of
      Nothing -> error "getPlayersName: No player by that number"
      Just pm -> mPlayerName pm


-- | this function apply the given game actions to the game the player is in.
inPlayersGameDo :: PlayerNumber -> StateT Game IO () -> StateT Multi IO ()
inPlayersGameDo pn action = do
   multi <- get
   let mg = getPlayersGame pn multi
   case mg of
      Nothing -> say "You must be in a game"
      Just g -> do
         myg <- lift $ runWithGame g action
         modifyGame myg

inGameDo :: GameName -> StateT Game IO () -> StateT Multi IO ()
inGameDo game action = do
   gs <- gets games
   case find (\(Game {gameName =n}) -> n==game) gs of
      Nothing -> say "No game by that name"
      Just g -> do
         myg <- lift $ runWithGame g action
         modifyGame myg

instance Ord PlayerMulti where
  (<=) = (<=) `on` mPlayerNumber


