
-- | This module re-exports the functions necessary to execute and log game events.
module Language.NomyxAPI (
   -- * Game management
   GameEvent(..),
   LoggedGame(..),
   GameName,
   Game(..),
   GameDesc(..),
   execGameEvent, execGameEvent',
   execWithGame, execWithGame',
   game,
   emptyGame,
   getLoggedGame,
   gameName,

   -- * Variables management
   Var(..),

   -- * Rules management
   Rule(..),
   RuleStatus(..),
   SubmitRule(..),
   activeRules, pendingRules, rejectedRules,

   -- * Events management
   EventHandler(..),
   Event(..),
   Status(..),
   getEventHandler,
   events,

   -- Inputs management
   Input(..),
   InputForm(..),
   UInputData(..),

   -- Outputs management
   Output(..),
   displayGame,
   Log(..),

   -- Time
   getTimes,
   currentTime,

   tracePN,
   )  where


import Language.Nomyx.Game
import Language.Nomyx.Expression
import Language.Nomyx.Utils (tracePN)


