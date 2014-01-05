
-- | Warning: Nomyx internals (not required to compose rules and play the game)
-- This module implements game engine.
-- the module manages the effects of rules over each others.
-- This module is not required
module Language.Nomyx.Engine(
   -- * Game management
   GameEvent(..),
   LoggedGame(..),
   GameName,
   Game(..),
   GameDesc(..),
   Simulation(..),
   execGameEvent, execGameEvent',
   execWithGame, execWithGame',
   game,
   emptyGame,
   getLoggedGame,
   gameName,
   simu,
   players,

   -- * Variables management
   Var(..),

   -- * Rules management
   SubmitRule(..),
   activeRules, pendingRules, rejectedRules,

   -- * Events management
   EventHandler(..),
   Status(..),
   getEventHandler,
   events,

   -- Inputs management
   UInputData(..),

   -- Outputs management
   Output(..),
   displayGame,
   Log(..),

   -- Time
   getTimes,
   currentTime,

   tracePN,
   replaceWith
   ) where

import Language.Nomyx.Engine.Evaluation
import Language.Nomyx.Engine.Game
import Language.Nomyx.Engine.GameEvents
import Language.Nomyx.Engine.Utils
