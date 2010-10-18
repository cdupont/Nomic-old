
-- | Nomic game in standalone (deprecated).
module Mono where

import Game
import Rule
import Control.Monad.State
import Comm
import Observable

type Turn = Int


startNomicMono :: Comm ()
startNomicMono = do
   putCom "Starting game in standalone..."
   nbPlayers <- safeRead "Number of Players:"
   _ <- runWithGame (initialGame "Jeu Mono") (game nbPlayers)
   putCom "GoodBye"


-- La fonction game demarre le jeu.
-- GameStateT "contient" l'état du jeu. C'est une monade qui permet de transmettre
-- commodément l'état du jeu d'une fonction à l'autre.
-- Cet état peut être modifié par différentes fonctions ci-dessous.
-- Il permet aussi de faire de l'IO.

game :: PlayerNumber -> GameState 
game nbP = do 
   say "Début du jeu\n" 
   mapM (aTurn nbP) [1..] 
   say "Fin du jeu\n"


-- Débute le tour n.

aTurn :: PlayerNumber -> Turn -> GameState
aTurn nbP n = do 
    --modify (\gs -> gs {turn = n })
    --turn <- gets turn 
    --nbP <- gets nbPlayers
    say $ "Début du tour n°" ++ (show $ n) ++ "!!\n"
    gs <- get
    say $ "La réglementation actuelle est: \n" ++ (show gs) ++ "\n"
    mapM play [1.. nbP]
    say "Fin du tour!!\n"


-- Fait jouer le joueur n.

play :: PlayerNumber -> GameState
play n = do
   say $ "Au tour du joueur n°: " ++ (show $ n) ++ "\n"
   -- input the new rule.
   rs <- gets rules
   nr <- lift $ enterRule' (length rs + 1) n
   modify (\gs@Game{rules=rs} -> gs {rules = nr:rs})
   proposeRule nr
   --modify (\gs -> gs {currentlyProposedRule = Nothing}) TODO: finish






-- Utility functions


-- 6.11 Instances


