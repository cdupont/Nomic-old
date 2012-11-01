
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, TemplateHaskell,
   EmptyDataDecls, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable, PackageImports, GADTs,
   ScopedTypeVariables, NamedFieldPuns, Rank2Types, DoAndIfThenElse, StandaloneDeriving#-}

module Web (launchWebServer) where

import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (dir)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes.Site
import Web.Routes.PathInfo
import Web.Routes.Happstack
import Web.Routes.RouteT
import Web.Routes.TH (derivePathInfo)
import Text.Blaze.Internal
import Game
import Multi
import Control.Monad
import Paths_Nomic
import Control.Monad.State
import Data.Monoid
import Data.String
import Control.Concurrent.STM
import Language.Haskell.Interpreter.Server
import Language.Nomic.Expression
import Language.Nomic.Evaluation
import Utils
import Data.Maybe
import Text.Reform.Happstack
import Text.Reform
import Forms
import Data.Text hiding (concat, map, filter)
import Happstack.Server

-- | associate a player number with a handle
data PlayerClient = PlayerClient PlayerNumber deriving (Eq, Show)

-- | A structure to hold the active games and players
data Server = Server [PlayerClient] deriving (Eq, Show)

data PlayerCommand = Login
                   | PostLogin
                   | Noop            PlayerNumber
                   | JoinGame        PlayerNumber GameName
                   | LeaveGame       PlayerNumber
                   | SubscribeGame   PlayerNumber GameName
                   | UnsubscribeGame PlayerNumber GameName
                   | DoInputChoice   PlayerNumber EventNumber
                   | DoInputString
                   | NewRule
                   | NewGame
                   deriving (Show)

$(derivePathInfo ''PlayerCommand)

instance PathInfo Bool where
  toPathSegments i = [pack $ show i]
  fromPathSegments = pToken (const "bool") (checkBool . show)
   where checkBool str =
           case reads str of
             [(n,[])] -> Just n
             _ ->        Nothing

data AppError
    = Required
    | NotANatural String
    | AppCFE (CommonFormError [Input])
      deriving Show

type NomicServer       = ServerPartT IO
type RoutedNomicServer = RouteT PlayerCommand NomicServer


nomicSite :: ServerHandle -> (TVar Multi) -> Site PlayerCommand (ServerPartT IO Html)
nomicSite sh tm = setDefault Login $ mkSitePI (runRouteT $ routedNomicCommands sh tm)

viewGame :: Game -> PlayerNumber -> RoutedNomicServer Html
viewGame g pn = do
   rf <- ruleForm pn
   os <- viewOutput (outputs g) pn
   vi <- viewInputs $ events g
   ok $ table $ do
      td ! A.id "gameCol" $ do
         div ! A.id "gameName" $ h5 $ string $ "You are viewing game: " ++ gameName g
         div ! A.id "citizens" $ viewPlayers $ players g
      td $ do
         div ! A.id "rules" $ viewAllRules g
         div ! A.id "events" $ viewEvents $ events g
         div ! A.id "inputs" $ vi
         div ! A.id "newRule" $ rf
         div ! A.id "Outputs" $ os

viewOutput :: [Output] -> PlayerNumber -> RoutedNomicServer Html
viewOutput os pn = do
   let myos = map snd $ filter (\o -> fst o == pn) os
   ok $ mapM_ viewMessages [myos]

viewEvents :: [EventHandler] -> Html
viewEvents ehs = do
   table $ do
      caption $ h3 "Events"
      thead $ do
         td $ text "Event Number"
         td $ text "By Rule"
         td $ text "Event"
      mapM_ viewEvent ehs

viewEvent :: EventHandler -> Html
viewEvent (EH eventNumber ruleNumber event _) = tr $ do
   td $ string . show $ eventNumber
   td $ string . show $ ruleNumber
   td $ string . show $ event

viewInputs :: [EventHandler] -> RoutedNomicServer Html
viewInputs ehs = do
   let ihs = filter isInput ehs
   is <- mapM viewInput ihs
   ok $ table $ do
      caption $ h3 "Inputs"
      mconcat is

viewInput :: EventHandler -> RoutedNomicServer Html
viewInput eh@(EH eventNumber _ (InputChoice pn _ _ _) _) = do
    link <- showURL (DoInputChoice pn eventNumber)
    lf  <- lift $ viewForm "user" $ eventForm eh
    ok $ blazeForm lf (link)
viewInput _ = error "input not handled"

isInput :: EventHandler -> Bool
isInput (EH _ _ (InputChoice _ _ _ _) _)  = True
isInput _ = False

viewAllRules :: Game -> Html
viewAllRules g = do
   h4 "Rules"
   viewRules (h5 "Constitution:")  $ activeRules g
   viewRules (h5 "Pending rules:") $ pendingRules g
   viewRules (h5 "Suppressed rules:") $ rejectedRules g

viewRules :: Html -> [Rule] -> Html
viewRules _ [] = return ()
viewRules title nrs = do
   table $ do
      caption $ h3 title
      thead $ do
         td $ text "Number"
         td $ text "Name"
         td $ text "Description"
         td $ text "Proposed by"
         td $ text "Code of the rule"
         td $ text "Assessed by"
      forM_ nrs viewRule

viewRule :: Rule -> Html
viewRule nr = tr $ do
   td $ string . show $ rNumber nr
   td $ string $ rName nr
   td $ string $ rDescription nr
   td $ string . show $ rProposedBy nr
   td $ string $ rRuleCode nr
   td $ string . show $ rAssessedBy nr


ruleForm :: PlayerNumber -> RoutedNomicServer Html
ruleForm pn = do
   link <- showURL NewRule
   ok $ H.form ! A.method "POST" ! A.action (toValue link) ! enctype "multipart/form-data;charset=UTF-8"  $ do
      H.label ! A.for "name" $ "Name"
      input ! type_ "text" ! name "name" ! A.id "name" ! tabindex "1" ! accesskey "N"
      H.label ! A.for "text" $ "Text"
      input ! type_ "text" ! name "text" ! A.id "text" ! tabindex "2" ! accesskey "T"
      H.br
      H.label ! A.for "text" $ "Code"
      textarea ! name "code" ! A.id "code" ! tabindex "3" ! accesskey "C" $ "Enter here your rule"
      input ! type_ "hidden" ! name "pn" ! value (fromString $ show pn)
      input ! type_  "submit" ! tabindex "4" ! accesskey "S" ! value "Submit rule!"


viewPlayers :: [PlayerInfo] -> Html
viewPlayers pis = do
   table $ do
      caption $ h5 "Players in game:"
      mapM_ viewPlayer pis


viewPlayer :: PlayerInfo -> Html
viewPlayer pi = tr $ do
    td $ string $ show $ playerNumber pi
    td $ string $ show $ playerName pi


viewMulti :: PlayerNumber -> Multi -> RoutedNomicServer Html
viewMulti pn m = do
   gns <- viewGameNames pn (games m)
   g <- case getPlayersGame pn m of
            Just g -> viewGame g pn
            Nothing -> ok $ h5 "Not in game"
   ok $ do
      div ! A.id "gameList" $ gns
      div ! A.id "game" $ g
      --div ! A.id "message" $ viewMessages mess


viewMessages :: [String] -> Html
viewMessages = mapM_ (\s -> string s >> br)


viewGameNames :: PlayerNumber -> [Game] -> RoutedNomicServer Html
viewGameNames pn gs = do
   gns <- mapM (viewGameName pn) gs
   ng <- newGameForm pn
   ok $ do
      h5 "Games:"
      table $ do
         case gs of
            [] -> tr $ td "No Games"
            _ ->  sequence_ gns
      ng

viewGameName :: PlayerNumber -> Game -> RoutedNomicServer Html
viewGameName pn g = do
   let gn = gameName g
   join <- showURL (JoinGame pn gn)
   leave <- showURL (LeaveGame pn)
   subscribe <- showURL (SubscribeGame pn gn)
   unsubscribe <- showURL (UnsubscribeGame pn gn)
   ok $ do
      tr $ do
         td $ string $ gn
         td $ H.a "Join" ! (href $ toValue join)
         td $ H.a "Leave" ! (href $ toValue leave)
         td $ H.a "Subscribe" ! (href $ toValue subscribe)
         td $ H.a "Unsubscribe" ! (href $ toValue unsubscribe)

newGameForm :: PlayerNumber -> RoutedNomicServer Html
newGameForm pn = do
   link <- showURL NewGame
   ok $ H.form ! A.method "POST" ! A.action (toValue link) ! enctype "multipart/form-data;charset=UTF-8"  $ do
      H.label ! A.for "name" $ "Game name:"
      input ! type_ "text" ! name "name" ! A.id "name" ! tabindex "1" ! accesskey "G"
      input ! type_ "hidden" ! name "pn" ! value (fromString $ show pn)
      input ! type_  "submit" ! tabindex "2" ! accesskey "S" ! value "Create New Game!"


nomicPage :: Multi -> PlayerNumber -> RoutedNomicServer Html
nomicPage multi pn = do
   m <- viewMulti pn multi
   ok $ do
      H.html $ do
        H.head $ do
          H.title "Welcome to Nomic!"
          H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomic.css"
          H.meta ! A.httpEquiv "Content-Type" ! content "text/html;charset=utf-8"
          H.meta ! A.name "keywords" ! A.content "Nomic, game, rules, Haskell, auto-reference"
          --H.meta ! A.httpEquiv "refresh" ! A.content "10"
        H.body $ do
          H.div ! A.id "container" $ do
             H.div ! A.id "header" $ string $ "Welcome to Nomic, " ++ (getPlayersName pn multi) ++ "!"
             H.div ! A.id "multi" $ m
             H.div ! A.id "footer" $ string "footer"

loginPage :: RoutedNomicServer Html
loginPage = do
   link <- showURL PostLogin
   lf  <- lift $ viewForm "user" loginForm
   ok $ H.html $ do
      H.head $ do
        H.title "Login to Nomic"
        H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomic.css"
        H.meta ! A.httpEquiv "Content-Type" ! content "text/html;charset=utf-8"
        H.meta ! A.name "keywords" ! A.content "Nomic, game, rules, Haskell, auto-reference"
      H.body $ do
        H.div ! A.id "container" $ do
           H.div ! A.id "header" $ "Login to Nomic"
           H.div ! A.id "login" $ blazeForm lf (link)
           H.div ! A.id "footer" $ "footer"


routedNomicCommands :: ServerHandle -> (TVar Multi) -> PlayerCommand -> RoutedNomicServer Html
routedNomicCommands _ _  (Login)                     = loginPage
routedNomicCommands _ tm (PostLogin)                 = postLogin tm
routedNomicCommands _ tm (Noop pn)                   = nomicPageComm pn tm (return ())
routedNomicCommands _ tm (JoinGame pn game)          = nomicPageComm pn tm (joinGame game pn)
routedNomicCommands _ tm (LeaveGame pn)              = nomicPageComm pn tm (leaveGame pn)
routedNomicCommands _ tm (SubscribeGame pn game)     = nomicPageComm pn tm (subscribeGame game pn)
routedNomicCommands _ tm (UnsubscribeGame pn game)   = nomicPageComm pn tm (unsubscribeGame game pn)
routedNomicCommands sh tm (NewRule)                  = newRule sh tm
routedNomicCommands _ tm (NewGame)                   = newGameWeb tm
routedNomicCommands _ tm (DoInputChoice pn en)       = newInputChoice pn en tm
routedNomicCommands _ _ (DoInputString)              = undefined

--execute the given instructions (Comm) and embed the result in a web page
nomicPageComm :: PlayerNumber -> (TVar Multi) -> StateT Multi IO () -> RoutedNomicServer Html
nomicPageComm pn tm comm = execCommand tm comm >> nomicPageServer pn tm

execCommand :: (TVar Multi) -> StateT Multi IO a -> RoutedNomicServer a
execCommand tm sm = do
    m <- liftRouteT $ lift $ atomically $ readTVar tm
    (a, m') <- liftRouteT $ lift $ runStateT sm m
    liftRouteT $ lift $ atomically $ writeTVar tm m'
    return a

--getInputChoice :: (Show c, Eq c, Typeable c) => PlayerNumber -> String -> [String] -> String -> TypeRep -> Event (InputChoice c)
--getInputChoice pn s cs def = case cast (InputChoice pn s (read cs) (read def)) of
--    r | def == typeOf r -> r
--      | otherwise     -> error "cannot cast input choice"

--getInputChoice :: PlayerNumber -> String -> [String] -> String -> Event (InputChoice String)
--getInputChoice pn s cs def = InputChoice pn s cs def

newRule :: ServerHandle -> (TVar Multi) -> RoutedNomicServer Html
newRule sh tm = do
   methodM POST -- only accept a post method
   mbEntry <- getData -- get the data
   case mbEntry of
      Left a -> error $ "error: newRule " ++ (concat a)
      Right (NewRuleForm name text code pn) -> do
         --debugM ("Rule submitted: name =" ++ name ++ "\ntext=" ++ text ++ "\ncode=" ++ code ++ "\npn=" ++ (show pn))
         nomicPageComm pn tm (submitRule name text code pn sh)


newGameWeb :: (TVar Multi) -> RoutedNomicServer Html
newGameWeb tm = do
   methodM POST
   mbEntry <- getData
   case mbEntry of
      Left a                      -> error $ "error: newGame" ++ (concat a)
      Right (NewGameForm name pn) -> nomicPageComm pn tm (newGame name pn)

newInputChoice :: PlayerNumber -> EventNumber -> (TVar Multi) -> RoutedNomicServer Html
newInputChoice pn en tm = do
    multi <- liftRouteT $ lift $ atomically $ readTVar tm
    let mg = fromJust $ getPlayersGame pn multi
    let eventHandler = fromJust $ findEvent en (events mg)
    methodM POST
    --r <- liftRouteT $ happstackEitherForm (form "/post") "post" (eventForm eventHandler)
    r <- liftRouteT $ eitherForm environment "user" (eventForm eventHandler)
    link <- showURL $ Noop pn
    case r of
       (Right c) -> do
          liftRouteT $ lift $ putStrLn $ "choice:" ++ (show c)
          execCommand tm $ inputChoiceResult en c pn
          seeOther link $ string "Redirecting..."
       (Left _) -> do
          liftRouteT $ lift $ putStrLn $ "cannot retrieve form data"
          seeOther link $ string "Redirecting..."


nomicPageServer :: PlayerNumber -> (TVar Multi) -> RoutedNomicServer Html
nomicPageServer pn tm = do
   multi <- liftRouteT $ lift $ atomically $ readTVar tm
   nomicPage multi pn


postLogin :: (TVar Multi) -> RoutedNomicServer Html
postLogin tm = do
    methodM POST
    r <- liftRouteT $ eitherForm environment "user" loginForm
    case r of
       (Right (LoginPass login password)) -> do
          liftRouteT $ lift $ putStrLn $ "login:" ++ login
          liftRouteT $ lift $ putStrLn $ "password:" ++ password
          mpn <- execCommand tm $ newPlayerWeb login password
          case mpn of
             Just pn -> do
                link <- showURL $ Noop pn
                seeOther link $ string "Redirecting..."
             Nothing -> error "cannot login"
       (Left _) -> seeOther ("/Login?status=fail" :: String) $ string "Redirecting..."


newPlayerWeb :: PlayerName -> PlayerPassword -> StateT Multi IO (Maybe PlayerNumber)
newPlayerWeb name pwd = do
   --find that name among the list
   mpn <- findPlayer name
   case mpn of
      Just pl -> do
         say $ "Trying name:" ++ mPlayerName pl
         case pwd == mPassword pl of
            True -> do
               say "password OK"
               return $ Just $ mPlayerNumber pl
            False -> do
               say "password false"
               return Nothing
      Nothing -> do
         say "New player"
         --add the new player to the list
         pn <- getNewPlayerNumber
         newPlayerU PlayerMulti { mPlayerNumber = pn, mPlayerName = name, mPassword = pwd, inGame = Nothing}
         return (Just pn)


launchWebServer :: ServerHandle -> (TVar Multi) -> IO ()
launchWebServer sh tm = do
   putStrLn "Starting web server...\nTo connect, drive your browser to \"http://localhost:8000/Login\""
   d <- getDataDir
   simpleHTTP nullConf $ server d sh tm

server :: FilePath -> ServerHandle -> (TVar Multi) -> NomicServer Response
server d sh tm = mconcat [fileServe [] d, do
    lift $ putStrLn "toto"
    decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
    html <- implSite (pack "http://localhost:8000") "/Login" (nomicSite sh tm)
    return $ toResponse html]

instance FromData NewRuleForm where
  fromData = do
    name  <- look "name" `mplus` (error "need rule name")
    text <-  look "text" `mplus` (error "need rule text")
    code <-  look "code" `mplus` (error "need rule code")
    pn <- lookRead "pn" `mplus` (error "need player number")
    return $ NewRuleForm name text code pn

instance FromData NewGameForm where
  fromData = do
    name  <- look "name" `mplus` (error "need rule name")
    pn <- lookRead "pn" `mplus` (error "need player number")
    return $ NewGameForm name pn

