

-- | This module handles intrepreting players' commands.
module Commands where

import Text.ParserCombinators.Parsec
import Observable
import Data.List
import Comm
import Utils
import Multi


-- | All commands issuable.
data Command = NewPlayer
             | ListGame
             | Name
			    | NewGame
             | Join
             | SubmitRule
             | SubmitRuleI
             | Constitution
             | ShowAllRules
             | ListPlayers
             | ShowPendingActions
             | ShowMyPendingActions
             | ShowCompletedActions
             | DoMyActions
             | DoAction
             | Amend
             | ExitGame
             | Help
             | QuitNomic
             deriving Eq

-- | Command's Strings
commands = [("newPlayer",         NewPlayer,    ": Internal command."),
            ("listGame",          ListGame,     ": list the active games"),
            ("name",              Name,         " <yourname>: change own name"),
            ("newGame",           NewGame,      " <gamename>: start a new game"),
            ("join",              Join,         " <gamename>: join an existing game"),
            ("submitRule",        SubmitRule,   " <name> <text> <code>: submit a rule. your code must be in \"\""),
            ("iSubmitRule",       SubmitRuleI,  " submit a rule in interactive mode"),
            ("showConstitution",  Constitution, ": show the constitution (must be in a game)"),
            ("showAllRules    ",  ShowAllRules, ": show every rules: Active (the current Constitution), Pending (currently proposed rules), Rejected (Proposed and rejected rules) and Suppressed (Once Active but suppressed rules)"),            
            ("listPlayers",       ListPlayers,  ": show the list of the players"),
            ("amendConstitution", Amend,        ": amend the constitution with the currently proposed rules"),
            ("showPendingActions", ShowPendingActions, ": show all actions that has to be completed by players"), --TODO: verify "by players"
            ("showMyPendingActions", ShowMyPendingActions, ": show all actions that has to be completed by you"),
            ("iDoActions",        DoMyActions,  ": realize now all your actions, in interactive mode"),
            ("doAction",          DoAction,     " <num> <result>: give the result of your action n°num (the number is given by showmypendingactions)."),
            ("showCompletedActions", ShowCompletedActions, ": show all already completed actions"),
            ("exitGame",          ExitGame,     ": exit from a game"),
            ("help",              Help,         ": show an help message"),
            ("quit",              QuitNomic,    ": quit Nomic")]
            


			
-- | parseLine analysis the line and return a command with optionnal arguments 
parseLine :: String -> Either ParseError (String, [String])
parseLine = parse line "parse error"

-- | line is the parsec parser to analyse a command with optionnal arguments
line :: Parser (String, [String])
line = do skipMany space
          command <- simpleArg
          skipMany space
          args <- (quotedArg <|> simpleArg) `sepBy` (spaces)
          skipMany space  --TODO: trailing spaces
          return (command, args)
          
-- | a simple argument's parser.
simpleArg :: Parser String
simpleArg = many1 $ noneOf " \n"

-- | a rule parser.
quotedArg :: Parser String
quotedArg = do code <- between (char '\"') (char '\"') $ many $ noneOf ("\"" ++ ['\0'..'\31'])
               return code


-- | runLine takes a String a executes the represented command, if any. 
runLine :: String -> PlayerNumber -> MultiState
runLine s pn = do 
             let pl = parseLine s
             case pl of
                Left _ -> say "Command analysis error"
                Right (comm, args) -> runCommand comm args pn
             say "\n"


-- | runCommand takes a String representing a command, optionnal arguments and runs it.
runCommand :: String -> [String] -> PlayerNumber -> MultiState
runCommand comm args pn = do
                let comm' = toLowerS comm
                let commands' = map (\(a,b,_) -> (toLowerS a, b)) commands
                --lookup the commands...
                case lookup comm' commands' of
                   Just c -> runCommand' c args pn
                   Nothing -> do
                      --if not found, try abbreviations
                      let mycomms = filter (\(sc, _) -> comm' `isPrefixOf` sc) commands'
                      case mycomms of
                         []           -> say "No command matchs your input"
                         (_ , c):[] -> runCommand' c args pn
                         _ -> say $ "Several commands match your input:\n\r" ++ concatMap (\(cs, _) -> cs ++ "\n\r") mycomms

		

-- | commandMulti takes a command and optionnal arguments and runs it.	   
runCommand' :: Command -> [String] -> PlayerNumber -> MultiState
runCommand' NewPlayer _     = newPlayer
runCommand' ListGame _      = listGame
runCommand' Name (a:[])     = newName a
runCommand' NewGame (g:[])  = newGame g
runCommand' Join (g:[])     = joinGame g
runCommand' SubmitRule (name:text:rule:[]) = submitRule name text rule
runCommand' SubmitRuleI _   = myCatch submitRuleI
runCommand' Constitution _  = showConstitution
runCommand' ShowAllRules _  = showAllRules
runCommand' ListPlayers _   = listPlayers
runCommand' Amend _         = amendConstitution
runCommand' ShowPendingActions _ = showPendingActions
runCommand' ShowMyPendingActions _ = showMyPendingActions
runCommand' DoMyActions _   = doActionsI
runCommand' DoAction (num:result:[]) = doAction num result
runCommand' ShowCompletedActions _ = showCompletedActions
runCommand' ExitGame _      = exitGame
runCommand' QuitNomic _     = quit
runCommand' Help _          = const help
runCommand' c _             = const $ say $ "the number of arguments doesn't match. \nUsage: \n" ++ getCommandUsage c				   

getCommandUsage :: Command -> String
getCommandUsage c = maybe (error "Usage not found") id $ lookup c $ map (\(a,b,c) -> (b,a++c)) commands

-- | issue an help message
help :: MultiState
help = say $ "Nomic commands:\n" ++ concatMap (\(c, _, h) -> c ++"\t" ++ h ++ "\n\r") (tail commands)
