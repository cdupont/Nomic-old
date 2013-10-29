
-- | This module re-exports the modules necessary to compose a Nomyx rule.
module Language.Nomyx (
   Nomex,

   -- * Variables
   V(..),
   VEvent(..),
   MsgVar(..),
   msgVar,
   newMsgVar, newMsgVar_, newMsgVar',
   writeMsgVar, writeMsgVar_,
   readMsgVar, readMsgVar_,
   modifyMsgVar,
   delMsgVar,
   onMsgVarEvent, onMsgVarChange,
   getMsgVarMessage,
   getMsgVarData, getMsgVarData_,
   getMsgVarName,
   ArrayVar,
   newArrayVar, newArrayVar_, newArrayVar',
   cleanOnFull,
   isFullArrayVar_,   
   putArrayVar, putArrayVar_,

   -- * Events
   Event(..),
   EventNumber,
   EventData(..),
   InputData(..),
   Msg,
   MsgData,
   onEvent, onEvent_,
   onEventOnce, onEventOnce_,
   delEvent, delEvent_, delAllEvents,
   sendMessage, sendMessage_,
   onMessage, onMessageOnce,
   schedule, schedule_, schedule', schedule'_,
   getCurrentTime,

   -- * Rules
   RuleFunc,
   RuleResp(..),
   Rule(..),
   RuleNumber,
   RuleCode,
   RuleEvent(..),
   voidRule,
   activateRule, activateRule_,
   rejectRule, rejectRule_,
   getRules, getActiveRules, getRule,
   getRulesByNumbers,
   getRuleFuncs,
   addRule, addRule_, addRuleParams,
   getFreeRuleNumber,
   suppressRule, suppressRule_, suppressAllRules,
   modifyRule,
   autoActivate,
   activateOrReject,
   noPlayPlayer,
   autoDelete,
   eraseAllRules,
   getSelfRuleNumber, getSelfRule,

   -- * Players
   PlayerNumber,
   PlayerName,
   PlayerInfo(..),
   Player(..),
   playerNumber, playerName,
   getPlayers, getPlayer, getPlayerName,
   setPlayerName,
   modifyPlayerName,
   getPlayersNumber, getAllPlayerNumbers,
   delPlayer,
   forEachPlayer, forEachPlayer_,
   createValueForEachPlayer, createValueForEachPlayer_,
   getValueOfPlayer,
   modifyValueOfPlayer, modifyAllValues,
   showPlayer,
   getSelfProposedByPlayer,

   -- * Victory
   setVictory, 
   giveVictory,
   
   -- * Outputs
   OutputNumber,
   newOutput, newOutput_,
   outputAll, outputAll_, outputAll',
   getOutput, getOutput_,
   updateOutput, updateOutput_, 
   delOutput, delOutput_,
   displayVar, displaySimpleVar,
   displayArrayVar, showArrayVar,
   
   -- * Inputs
   inputRadio, inputRadioHead, inputRadioEnum,
   inputRadioData, 
   onInputRadio, onInputRadio_,
   onInputRadioOnce, onInputRadioOnce_,
   onInputRadioEnum, onInputRadioEnum_,
   onInputRadioEnumOnce_, 
   inputText,
   inputTextData,
   onInputText, onInputText_,
   onInputTextOnce, onInputTextOnce_,
   inputCheckboxData,
   inputCheckbox,
   onInputCheckbox, onInputCheckbox_,
   onInputCheckboxOnce, onInputCheckboxOnce_,
   inputButtonData,
   inputButton,
   onInputButton, onInputButton_,
   onInputButtonOnce, onInputButtonOnce_,
   inputTextareaData,
   inputTextarea, 
   onInputTextarea, onInputTextarea_,
   onInputTextareaOnce, onInputTextareaOnce_,

   module Language.Nomyx.Vote,
   module Language.Nomyx.Utils)  where

import Language.Nomyx.Expression
import Language.Nomyx.Outputs
import Language.Nomyx.Inputs
import Language.Nomyx.Events
import Language.Nomyx.Players
import Language.Nomyx.Variables
import Language.Nomyx.Rule
import Language.Nomyx.Vote
import Language.Nomyx.Utils


