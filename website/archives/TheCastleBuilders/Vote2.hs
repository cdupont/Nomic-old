{-# LANGUAGE GADTs #-}

-- | Voting system including a way to cancel a vote
module Vote2 where

import Prelude
import Language.Nomyx

-- When a rule is proposed, create a button to cancel it
-- by sending the "end of vote" message
cancelVote :: RuleFunc
cancelVote = voidRule $ onEvent_ (RuleEv Proposed) $ \(RuleData rule) -> do
    let rn = _rNumber rule
    let pn = _rProposedBy rule
    let msgEnd = Message ("Result of votes for rule " ++ (show rn))
    en <- onInputButtonOnce ("Cancel votes for rule " ++ (show rn) ++ " ") (\_ _ -> sendMessage msgEnd [Against]) pn
    onMessageOnce msgEnd (const $ delEvent_ en)

