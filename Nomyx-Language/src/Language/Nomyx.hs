{-# LANGUAGE Trustworthy #-}

-- | This module re-exports the elements necessary to compose a Nomyx rule.
module Language.Nomyx (
   module Language.Nomyx.Expression, -- Nomyx Expression DSL
   module Language.Nomyx.Outputs,    -- create outputs
   module Language.Nomyx.Inputs,     -- create inputs
   module Language.Nomyx.Events,     -- create events
   module Language.Nomyx.Players,    -- manage players
   module Language.Nomyx.Variables,  -- create variables
   module Language.Nomyx.Rules,      -- manage rules
   module Language.Nomyx.Vote        -- create votations
   )  where

import Language.Nomyx.Expression
import Language.Nomyx.Outputs
import Language.Nomyx.Inputs
import Language.Nomyx.Events
import Language.Nomyx.Players
import Language.Nomyx.Variables
import Language.Nomyx.Rules
import Language.Nomyx.Vote



