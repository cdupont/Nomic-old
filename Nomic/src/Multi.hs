{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators,
    TypeSynonymInstances, FlexibleInstances #-}

-- | This module manages multi-player games and commands.
module Multi (Multi, PlayerPassword, GetMulti(..), FindPlayer(..), PlayerMulti(..), GetNewPlayerNumber(..), NewPlayerU(..),
    listGame, newGame, joinGame, leaveGame, subscribeGame, unsubscribeGame, showSubscribtion, showSubGame, newPlayer,
    submitRule, myCatch, submitRuleI, showConstitution, showAllRules, listPlayers, amendConstitution, showPendingActions,
    showMyPendingActions, doActionsI, doAction', showCompletedActions, quit, getPendingActions, doAction, games, getPlayersGame,
    getPlayersName) where


import Prelude hiding (catch)
import Data.List
import Control.Monad.State
import Game
import Utils
import Interpret
import Comm
import Action
import Safe
import NamedRule
import Control.Monad.CatchIO
import System.IO.Error hiding (catch)
import Happstack.State
import Data.Typeable
import Control.Monad.Reader
import Data.Function (on)
import Debug.Trace.Helpers()
import Expression

type PlayerPassword = String

data PlayerMulti = PlayerMulti   { mPlayerNumber :: PlayerNumber,
                                   mPlayerName :: PlayerName,
                                   mPassword :: PlayerPassword,
                                   inGame :: Maybe GameName}
                                   deriving (Eq, Show, Typeable)

instance Version PlayerMulti
$(deriveSerialize ''PlayerMulti)
$(mkMethods ''PlayerMulti [])

instance Component PlayerMulti where
    type Dependencies PlayerMulti = End
    initialValue =  PlayerMulti   { mPlayerNumber = 0,
                                    mPlayerName   = "",
                                    mPassword     = "",
                                    inGame = Nothing}


-- | A structure to hold the active games and players
data Multi = Multi { games   :: [Game],
                     mPlayers :: [PlayerMulti]}
                     deriving (Eq, Typeable)

instance Version Multi
$(deriveSerialize ''Multi)



instance Component Multi where
    type Dependencies Multi = End
    initialValue = defaultMulti

instance Show Multi where
   show Multi{games=gs, mPlayers=mps} = show (sort gs) ++ "\n" ++ show (sort mps)

defaultMulti :: Multi
defaultMulti = Multi [] []

queryMulti :: Query Multi Multi
queryMulti = ask



-- | A State to pass around active games and players.
-- Furthermore, the output are to be made with Comm to output to the right console.
type MultiState = StateT Multi Comm ()

type MultiStateWith a = StateT Multi Comm a

-- | An helper function that makes it very clear how to use the state transformer MultiState.
runWithMulti :: Multi -> MultiState -> Comm Multi
runWithMulti = flip execStateT

		
-- | this function will ask you to re-enter your data if it cannot cast it to an a.
myCatch :: (PlayerNumber -> Comm ()) -> PlayerNumber -> Comm ()
myCatch f pn = catch (f pn) eHandler where
   eHandler e 
      | isUserError e = putCom "Aborted with UserError" >> return ()
      | otherwise     = putCom "Aborted with IOError"   >> return ()

-- | helper function to change a player's ingame status.				 
mayJoinGame :: Maybe GameName -> PlayerNumber -> [PlayerMulti] -> [PlayerMulti]
mayJoinGame maybename pn pl = case find (\(PlayerMulti mypn _ _ _) -> mypn == pn) pl of
                     Just o -> replace o o{ inGame = maybename} pl
                     Nothing -> pl

newPlayerU :: PlayerMulti -> Update Multi ()
newPlayerU pm = do
   pms <- gets mPlayers
   modify (\multi -> multi { mPlayers = pm : pms})

findPlayer :: PlayerName -> Query Multi (Maybe PlayerMulti)
findPlayer name =  fmap (find (\PlayerMulti {mPlayerName = pn} -> pn==name)) (asks mPlayers)

getNewPlayerNumber :: Query Multi PlayerNumber
getNewPlayerNumber = do
   ps <- asks mPlayers
   return $ length ps + 1

getMulti :: Query Multi Multi
getMulti = ask

setMulti :: Multi -> Update Multi ()
setMulti = put

getGames :: Query Multi [Game]
getGames = asks games

getPlayersMulti :: Query Multi [PlayerMulti]
getPlayersMulti = asks mPlayers

addNewGame :: Game -> Update Multi ()
addNewGame new = modify (\multi@Multi {games=gs} -> multi {games =  new:gs})

getGameByName :: GameName -> Query Multi (Maybe Game)
getGameByName gn =  fmap (find (\(Game {gameName = n}) -> n==gn)) (asks games)

joinGamePlayer :: PlayerNumber -> GameName -> Update Multi ()
joinGamePlayer pn game = modify (\multi -> multi {mPlayers = mayJoinGame (Just game) pn (mPlayers multi)})

leaveGameU :: PlayerNumber -> Update Multi ()
leaveGameU pn = modify (\multi -> multi {mPlayers = mayJoinGame Nothing pn (mPlayers multi)})

$(mkMethods ''Multi ['queryMulti,
                     'newPlayerU,
                     'findPlayer,
                     'getNewPlayerNumber,
                     'getGames,
                     'getMulti,
                     'setMulti,
                     'getPlayersMulti,
                     'addNewGame,
                     'getGameByName,
                     'joinGamePlayer,
                     'leaveGameU])


newPlayer :: Comm PlayerNumber
newPlayer = do
   name <- putGetComm "Please enter your name:"
   --find that name among the list
   mpn <- query $ FindPlayer name
   pn <- case mpn of
      Just pl -> do
         putCom $ "Welcome back, " ++ mPlayerName pl
         pwd <- putGetComm  "Please enter your password:"
         case pwd == mPassword pl of
            True -> do
               putCom "password OK"
               return $ mPlayerNumber pl
            False -> do
               putCom "password false, please re-enter"
               newPlayer
      Nothing -> do
         putCom "New player"
         pwd <- putGetComm "Please create a password:"
         --add the new player to the list
         pn <- query GetNewPlayerNumber --CDU to check
         update $ NewPlayerU PlayerMulti { mPlayerNumber = pn, mPlayerName = name, mPassword = pwd, inGame = Nothing}
         return pn

   putCom "Entering multi game..."
   putCom "If your lost, try the help command"
   return pn


-- | list the active games
listGame :: PlayerNumber -> Comm ()
listGame _ = do
   gs <- query GetGames
   case length gs of
      0 -> putCom "No active games"
      _ -> do
         putCom "Active games:"
         putCom $ concatMap (\g -> gameName g ++ "\n") gs  


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
newGame :: String -> PlayerNumber -> Comm ()
newGame name _ = do
   m <- query GetMulti
   case uniqueGame name m of
      True -> do
         putCom $ "Creating a new game of name: " ++ name
         -- create a game with zero players
         update $ AddNewGame $ initialGame name
      False -> putCom $ "this name is already used" 

uniqueGame :: String -> Multi -> Bool
uniqueGame s m = null $ filter (\p -> gameName p == s) (games m)

-- | join a game.
joinGame :: GameName -> PlayerNumber -> Comm ()
joinGame game pn = do
   mg <- query $ GetGameByName game
   case mg of
      Nothing -> putCom $ "No game by that name"
      Just g -> do
         putCom "subscribing first."
         subscribeGame (gameName g) pn      
         putCom $ "Joining game: " ++ game
         update $ JoinGamePlayer pn game


-- | leave a game (you remain subscribed).				
leaveGame :: PlayerNumber -> Comm ()
leaveGame pn = do
   update $ LeaveGameU pn
   putCom "You left the game (you remain subscribed)." 


-- | subcribe to a game.			
subscribeGame :: GameName -> PlayerNumber -> Comm ()
subscribeGame game pn = do
   m <- query GetMulti
   inGameDo game $ do
      g <- get
      case find (\(PlayerInfo  { playerNumber=mypn}) -> mypn == pn ) (players g) of
         Just _ -> say "Already subscribed!"
         Nothing -> do
            say $ "Subscribing to game: " ++ game
            put g {players = PlayerInfo { playerNumber = pn,
                                          playerName = getPlayersName pn m} : (players g)}


-- | subcribe to a game.			
unsubscribeGame :: GameName -> PlayerNumber -> Comm ()
unsubscribeGame game pn = inGameDo game $ do
   g <- get
   case find (\(PlayerInfo  { playerNumber=mypn}) -> mypn == pn ) (players g) of
      Nothing -> say "Not subscribed!"
      Just _ -> do
         say $ "Unsubscribing to game: " ++ game
         put g {players = filter (\PlayerInfo { playerNumber = mypn} -> mypn /= pn) (players g)}


showSubGame :: GameName -> PlayerNumber -> Comm ()
showSubGame g _ = inGameDo g $ do
   ps <- gets players
   say $ concatMap show ps

showSubscribtion :: PlayerNumber -> Comm ()
showSubscribtion pn = inPlayersGameDo pn $ do
   ps <- gets players
   say $ concatMap show ps


-- | insert a rule in pending rules. This rule may be added to constitution later on with the "amendconstitution" command.
-- the rules are numbered incrementaly.
submitRule :: String -> String -> String -> PlayerNumber -> Comm ()						 
submitRule name text rule pn = inPlayersGameDo pn $ do
   --input the new rule (may fail if ill-formed)
   rs <- gets rules
   mnr <- lift $ enterRule (length rs + 1) name text rule pn
   case mnr of
      Just nr -> do
         modify (\gs@Game {rules=myrs} -> gs {rules = nr:myrs})
         say $ "Your rule has been added to pending rules."
      Nothing -> say $ "Please try again."


submitRuleI :: PlayerNumber -> Comm ()	
submitRuleI pn = inPlayersGameDo pn $ do
   rs <- gets rules
   name <- lift $ putGetComm "Please enter the rule name:"
   text <- lift $ putGetComm "Please enter a short description of the rule:"
   rule <- lift $ putGetComm "Please enter the rule's code:"
   mnr  <- lift $ enterRule (length rs + 1) name text rule pn
   case mnr of
      Just nr -> do
         modify (\gs@Game {rules=myrs} -> gs {rules = nr:myrs})
         say $ "Your rule has been added to pending rules."
      Nothing -> say "Please try again."

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
enterRule :: RuleNumber -> String -> String -> String -> PlayerNumber -> Comm (Maybe Rule)
enterRule num name text ruleText pn = do
   mrr <- maybeReadRule ruleText
   case mrr of
      Just ruleFunc -> return $ Just Rule {rNumber = num,
                      rName = name,
                      rDescription = text,
                      rProposedBy = pn,
                      rRuleCode = ruleText,
                      rRuleFunc = ruleFunc,
                      rStatus = Pending,
                      rejectedBy = Nothing}
      Nothing -> return Nothing
        

-- | show the constitution.
showConstitution :: PlayerNumber -> Comm ()	 
showConstitution pn = inPlayersGameDo pn $ get >>= (say  .  showRS  .  activeRules)


-- | show every rules (including pendings and deleted)
showAllRules :: PlayerNumber -> Comm ()	 
showAllRules pn = inPlayersGameDo pn $ get >>= (say . showRS . rules)

-- | show players      
listPlayers :: PlayerNumber -> Comm ()
listPlayers _ = do
   ps <- query GetPlayersMulti
   case length ps of
      0 -> putCom "No players"
      _ -> do
         putCom "Players:"
         putCom $ concatMap displayPlayer $ sort ps  

displayPlayer :: PlayerMulti -> String
displayPlayer (PlayerMulti pn name _ (Just game)) = show pn ++ ": " ++ name ++ " in game: " ++ game ++ "\n"
displayPlayer (PlayerMulti pn name _ Nothing)     = show pn ++ ": " ++ name ++ "\n"



-- | propose all pending rules to the current constitution
amendConstitution :: PlayerNumber -> Comm ()
amendConstitution pn = inPlayersGameDo pn proposeAllPendings


-- | actions

-- | get all pending actions, deduced from the pending rules.
getPendingActions :: PlayerNumber -> Comm [Action]
getPendingActions pn = stateMultiToComm $ do
   multi <- get
   let mg = getPlayersGame pn multi
   case mg of
      Nothing -> do
         say "You must be in a game"
         return []
      Just g -> lift $ evalStateT pendingActions g


-- | show all pending actions, deduced from the pending rules.
showPendingActions :: PlayerNumber -> Comm ()
showPendingActions pn = inPlayersGameDo pn $ do
   pa <- pendingActions
   sas <- showActions pa
   say sas

-- | show only my pending actions, deduced from the pending rules.
showMyPendingActions :: PlayerNumber -> Comm ()
showMyPendingActions pn = inPlayersGameDo pn $ do
   ppa <- playersPendingActions pn
   sas <- showActions ppa
   say sas

-- | show all already completed actions.
showCompletedActions :: PlayerNumber -> Comm ()
showCompletedActions pn = inPlayersGameDo pn $ do
   ar <- gets actionResults
   sas <- showActions ar
   say sas


-- | ask the player to fulfill his actions in interactive mode
doActionsI :: PlayerNumber -> Comm ()
doActionsI pn = inPlayersGameDo pn $ do
   ppa <- playersPendingActions pn
   case ppa of
      [] -> say "No pending actions"
      _:_ -> do
         ars <- mapM enterActionResult' ppa
         modify (\gs@Game {actionResults=oldars} -> gs {actionResults = ars ++ oldars})


-- | fulfill an action
doAction :: String -> String -> PlayerNumber -> Comm ()  --TODO: solve big conccurency problem. actions to do may change between show and this command.
doAction actionNumber result pn = inPlayersGameDo pn $ do
   --input the new rule (may fail if ill-formed)
   case maybeRead actionNumber of
      Just an -> do
         case maybeRead result of
            Just r -> enterActionResult an r pn
            Nothing -> say $ "Cannot read result"
      Nothing -> say $ "Cannot read action number"

-- | fulfill an action
doAction' :: ActionNumber -> ActionResult -> PlayerNumber -> Comm ()  --TODO: solve big conccurency problem. actions to do may change between show and this command.
doAction' actionNumber result pn = inPlayersGameDo pn $ enterActionResult actionNumber result pn

-- | create an action
enterActionResult :: ActionNumber -> ActionResult -> PlayerNumber -> GameState
enterActionResult actionNumber result pn = do
   ppa <- playersPendingActions pn
   case ppa `atMay` (actionNumber - 1) of
      Just a -> do
         modify (\gs@Game {actionResults=ars} -> gs {actionResults = (a {result = Just result}):ars})
         say "Your action result has been stored."
      Nothing -> say "No pending action by that number"

 
enterActionResult' :: Action -> GameStateWith Action
enterActionResult' a = do
   s <- showAction a
   say s
   r <- lift $ safeRead "Please enter your result for this action:"
   return a {result = Just r}




-- | quit the game
quit :: PlayerNumber -> Comm ()
quit _ = putCom "quit"

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


stateMultiToComm :: StateT Multi Comm a -> Comm a
stateMultiToComm ms = do
   m <- lift $ query GetMulti
   (a,s) <- runStateT ms m
   lift $ update $ SetMulti s
   return a

-- | this function apply the given game actions to the game the player is in.
inPlayersGameDo :: PlayerNumber -> GameState -> Comm ()	  
inPlayersGameDo pn action = stateMultiToComm $ do
   multi <- get
   let mg = getPlayersGame pn multi
   case mg of
      Nothing -> say "You must be in a game"
      Just g -> do
         myg <- lift $ runWithGame g action
         modifyGame myg	
							
inGameDo :: GameName -> GameState -> Comm ()	
inGameDo game action = stateMultiToComm $ do
   gs <- query GetGames
   case find (\(Game {gameName =n}) -> n==game) gs of
      Nothing -> say "No game by that name"
      Just g -> do
         myg <- lift $ runWithGame g action
         modifyGame myg						
							
							
instance Ord PlayerMulti where
  (<=) = (<=) `on` mPlayerNumber


