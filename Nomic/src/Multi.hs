
-- | This module manages multi-player games and command reading and executing.
module Multi where


import Prelude hiding (catch)
import Data.List
import Control.Monad.State
import Game
import Utils
import Data.Maybe
import Interpret
import Observable
import Comm
import Action
import Safe
import NamedRule
import Control.Monad.CatchIO
import System.IO.Error hiding (catch)

-- | A Player along with a name and a optionnaly a currently played game
data PlayerInfo = PlayerInfo { playerNumber :: PlayerNumber,
                               playerName :: Maybe String,
							          inGame :: Maybe GameName}
                               deriving (Eq, Show)

instance Ord PlayerInfo where
   h <= g = (playerNumber h) <= (playerNumber g)
							   		

type PlayerList = [PlayerInfo]
type GameList = [Game]

-- | A structure to hold the active games and players
data Multi = Multi { games :: GameList,
                     players :: PlayerList}
                     deriving (Eq)

instance Show Multi where
   show Multi{games=gs, players =p} = show (sort gs) ++ "\n" ++ show (sort p)

defaultMulti :: Multi
defaultMulti = Multi [] []

-- | A State to pass around active games and players.
-- Furthermore, the output are to be made with Comm to output to the right console.
type MultiState = StateT Multi Comm ()

-- | An helper function that makes it very clear how to use the state transformer MultiState.
runWithMulti :: Multi -> MultiState -> Comm Multi
runWithMulti = flip execStateT

		
-- | this function will ask you to re-enter your data if it cannot cast it to an a.
myCatch :: (PlayerNumber -> MultiState) -> PlayerNumber -> MultiState
myCatch f pn = do
   catch (f pn) eHandler where
      eHandler e 
         | isUserError e = (say "Aborted with UserError") >> return ()
         | otherwise     = (say "Aborted with IOError") >> return ()


-- | list the active games
listGame :: PlayerNumber -> MultiState
listGame _ = do
   gs <- gets games
   case length gs of
      0 -> say "No active games"
      _ -> do
         say "Active games:"
         say $ concatMap (\g -> gameName g ++ "\n") gs  

-- | Add an unamed player	
newPlayer :: PlayerNumber -> MultiState	 
newPlayer pn = do
   say $ "Welcome to Haskell Nomic!"
   say $ "If your lost, try the help command"
   modify (\multi -> multi {players = (PlayerInfo pn Nothing Nothing):(players multi)})
   
-- | sets the name of a player   
newName :: String -> PlayerNumber -> MultiState
newName name pn = do
   m <- get
   case uniqueName name m of
      True -> do say $ "setting your new name to: " ++ name
                 modify (\multi -> multi {players = setName name pn (players multi)})
      False -> say $ "this name is already used"


uniqueName :: String -> Multi -> Bool
uniqueName s m = null $ filter (\p -> playerName p == Just s) (players m)


-- | starts a new game
newGame :: String -> PlayerNumber -> MultiState
newGame name _ = do
   m <- get
   case uniqueGame name m of
      True -> do
         gs <- gets games
         say $ "Creating a new game of name: " ++ name
         -- create a game with zero players
         let new = initialGame name                
         modify (\multi -> multi {games =  new:gs})
      False -> say $ "this name is already used" 

uniqueGame :: String -> Multi -> Bool
uniqueGame s m = null $ filter (\p -> gameName p == s) (games m)

-- | join a game.
joinGame :: GameName -> PlayerNumber -> MultiState
joinGame game pn = do
   gs <- gets games
   ps <- gets players
   let mg = find (\(Game {gameName =n}) -> n==game) gs
   case mg of
      Nothing -> say $ "No game by that name"
      Just _ -> do
         let p = fromJust $ find (\(PlayerInfo h _ _) -> h == pn) ps
         case playerName p of
            Nothing -> say $ "you must have a name before joining a game"
            Just _ -> do say $ "Joining game: " ++ game
                         modify (\multi -> multi {players = mayJoinGame (Just game) pn (players multi)})

-- | helper function to change a player's ingame status.				 
mayJoinGame :: Maybe GameName -> PlayerNumber -> PlayerList -> PlayerList
mayJoinGame maybename pn pl = case find (\(PlayerInfo h _ _) -> h == pn) pl of
                     Just o -> replace o o{ inGame = maybename} pl
                     Nothing -> pl 

-- | quit a game.				
exitGame :: PlayerNumber -> MultiState
exitGame pn = do
   modify (\multi -> multi {players = mayJoinGame Nothing pn (players multi)})
   say $ "You exited the game." 


-- | insert a rule in pending rules. This rule may be added to constitution later on with the "amendconstitution" command.
-- the rules are numbered incrementaly.
submitRule :: String -> String -> String -> PlayerNumber -> MultiState						 
submitRule name text rule pn = inPlayersGameDo pn $ do
   --input the new rule (may fail if ill-formed)
   rs <- gets rules
   mnr <- lift $ enterRule (length rs + 1) name text rule pn
   case mnr of
      Just nr -> do
         modify (\gs@Game {rules=myrs} -> gs {rules = nr:myrs})
         say $ "Your rule has been added to pending rules."
      Nothing -> say $ "Please try again."


submitRuleI :: PlayerNumber -> MultiState	
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
      Nothing -> say $ "Please try again."

-- | finds the corresponding game in the multistate and replaces it.
modifyGame :: Game -> MultiState
modifyGame g = do
   Multi gs ps <- get
   case find (\myg -> gameName g == gameName myg) gs of
      Nothing -> error "No game by that name"
      Just oldg -> do
         let newgs = replace oldg g gs
         put $ Multi newgs ps

-- | reads a rule.
enterRule :: RuleNumber -> String -> String -> String -> PlayerNumber -> Comm (Maybe NamedRule)
enterRule num name text rule pn = do
   mrr <- maybeReadRule rule
   case mrr of
      Just _ -> return $ Just $ NamedRule {rNumber = num,
                      rName = name,
                      rText = text,
                      rProposedBy = pn,
                      rule = rule,
                      rStatus = Pending,
                      rejectedBy = Nothing}
      Nothing -> return Nothing
        

-- | show the constitution.
showConstitution :: PlayerNumber -> MultiState	 
showConstitution pn = inPlayersGameDo pn $ get >>= (say  .  showRS  .  activeRules)


-- | show every rules (including pendings and deleted)
showAllRules :: PlayerNumber -> MultiState	 
showAllRules pn = inPlayersGameDo pn $ get >>= (say  .  showRS  .  rules)

-- | show players      
listPlayers :: PlayerNumber -> MultiState
listPlayers _ = do
   ps <- gets players
   case length ps of
      0 -> say "No players"
      _ -> do
         say "Players:"
         say $ concatMap displayPlayer $ sort ps  

displayPlayer :: PlayerInfo -> String
displayPlayer (PlayerInfo pn (Just name) (Just game)) = show pn ++ ": " ++ name ++ " in game: " ++ game ++ "\n"
displayPlayer (PlayerInfo pn (Just name) Nothing)     = show pn ++ ": " ++ name ++ "\n"
displayPlayer (PlayerInfo pn Nothing _)               = show pn ++ ": " ++ "Unnamed Player\n"


-- | propose all pending rules to the current constitution
amendConstitution :: PlayerNumber -> MultiState
amendConstitution pn = inPlayersGameDo pn proposeAllPendings


-- | actions

-- | show all pending actions, deduced from the pending rules.
showPendingActions :: PlayerNumber -> MultiState
showPendingActions pn = inPlayersGameDo pn $ do
   pa <- pendingActions
   sas <- showActions pa
   say sas

-- | show only my pending actions, deduced from the pending rules.
showMyPendingActions :: PlayerNumber -> MultiState
showMyPendingActions pn = inPlayersGameDo pn $ do
   ppa <- playersPendingActions pn
   sas <- showActions ppa
   say sas

-- | show all already completed actions.
showCompletedActions :: PlayerNumber -> MultiState
showCompletedActions pn = inPlayersGameDo pn $ do
   ar <- gets actionResults
   sas <- showActions ar
   say sas


-- | ask the player to fulfill his actions in interactive mode
doActionsI :: PlayerNumber -> MultiState
doActionsI pn = inPlayersGameDo pn $ do
   ppa <- playersPendingActions pn
   case ppa of
      [] -> say "No pending actions"
      _:_ -> do
         ars <- mapM enterActionResult' ppa
         modify (\gs@Game {actionResults=oldars} -> gs {actionResults = ars ++ oldars})


-- | fulfill an action
doAction :: String -> String -> PlayerNumber -> MultiState  --TODO: solve big conccurency problem. actions to do may change between show and this command.
doAction actionNumber result pn = inPlayersGameDo pn $ do
   --input the new rule (may fail if ill-formed)
   mar <- enterActionResult actionNumber result pn
   case mar of
      Just ar -> do
         modify (\gs@Game {actionResults=ars} -> gs {actionResults = ar:ars})
         say $ "Your action result has been stored."
      Nothing -> say $ "Your action is ill-formed."


-- | create an action
enterActionResult :: String -> String -> PlayerNumber -> GameStateWith (Maybe Action)
enterActionResult actionNumber result pn = do
   ppa <- playersPendingActions pn
   return $ do
      myan <- maybeRead actionNumber   --TODO: add specific error messages
      myr <- maybeRead result
      ma <- ppa `atMay` (myan - 1)
      return $ ma {result = Just myr}
 
enterActionResult' :: Action -> GameStateWith Action
enterActionResult' a = do
   s <- showAction a
   say s
   r <- lift $ safeRead "Please enter your result for this action:"
   return a {result = Just r}




-- | quit the game
quit _ = say "quit"

-- | Utility functions

-- | replace the player's name in the list
setName :: String -> PlayerNumber -> PlayerList -> PlayerList
setName name pn pl = case find (\(PlayerInfo h _ _) -> h == pn) pl of
                        Just o -> replace o o{ playerName = Just name} pl
                        Nothing -> pl 


-- | returns the game the player is in						
getPlayersGame :: PlayerNumber -> Multi -> Maybe Game
getPlayersGame pn multi = do
        pi <- find (\(PlayerInfo n _ _) -> n==pn) (players multi)
        gn <- inGame pi
        find (\(Game {gameName=name}) -> name==gn) (games multi)


-- | this function apply the given game actions to the game the player is in.
inPlayersGameDo :: PlayerNumber -> GameState -> MultiState	  
inPlayersGameDo pn action = do
   multi <- get
   let mg = getPlayersGame pn multi
   case mg of
      Nothing -> say $ "You must be in a game"
      Just g -> do
         myg <- lift $ runWithGame g action
         modifyGame myg	
							
					 



