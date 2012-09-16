
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, TemplateHaskell,
   EmptyDataDecls, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable, PackageImports, GADTs,
   ScopedTypeVariables, NamedFieldPuns, Rank2Types#-}

module Web (launchWebServer) where

import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (dir)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Web.Routes.Site
import Web.Routes.PathInfo
import Web.Routes.Happstack
import Web.Routes.Regular
import Web.Routes.RouteT

import Generics.Regular
import qualified Text.Digestive.Blaze.Html5 as TDB
import Text.Digestive.Blaze.Html5
import Text.Digestive.Forms.Happstack

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as LU (fromString)

import Game
import Text.Blaze.Renderer.Pretty
import Multi
import Happstack.Server
import Happstack.State
import Happstack.Util.Common
import Control.Monad
import Paths_Nomic
import Control.Monad.State
import Data.Monoid
import Data.String
import Control.Concurrent.STM
import Language.Haskell.Interpreter.Server
import Control.Monad.Loops
import Control.Applicative (Applicative(..), (<$>))
import Text.Digestive
import qualified Text.Digestive as TD (check)
import Language.Nomic.Expression
import Language.Nomic.Evaluation
import Control.Monad.Error
import Data.IORef
import System.IO.Unsafe
import System.IO
import Data.Typeable
import Utils
import Unsafe.Coerce
import Data.Maybe

type SessionNumber = Integer

data LoginPass = LoginPass { login :: PlayerName,
                             password :: PlayerPassword}

-- | associate a player number with a handle
data PlayerClient = PlayerClient { cPlayerNumber :: PlayerNumber}
                                   deriving (Eq, Show)

-- | A structure to hold the active games and players
data Server = Server { playerClients :: [PlayerClient]}
                       deriving (Eq, Show)

--data PlayerCommand = PlayerCommand { playerNumber :: PlayerNumber,
--                                     command :: MCommand}
--                                   deriving (Eq, Show)
--
data NewRuleForm = NewRuleForm { ruleName :: String,
                         ruleText :: String,
                         ruleCode :: String,
                         pn :: PlayerNumber }

data NewGameForm = NewGameForm { newGameName :: String,
                         gamePn :: PlayerNumber }

-- | A State to pass around active games and players.
-- Furthermore, the output are to be made with Comm to output to the right console.
type ServerState = StateT Server IO ()

data PlayerCommand = Login
                   | PostLogin
                   | Noop            PlayerNumber
                   | JoinGame        PlayerNumber GameName
                   | LeaveGame       PlayerNumber
                   | SubscribeGame   PlayerNumber GameName
                   | UnsubscribeGame PlayerNumber GameName
                   | DoInputChoice
                   | DoInputString
                   | NewRule
                   | NewGame
                   deriving (Eq, Show, Read)

$(deriveAll ''PlayerCommand "PFPlayerCommand")

type instance PF PlayerCommand = PFPlayerCommand

instance PathInfo PlayerCommand where
    toPathSegments    = gtoPathSegments . from
    fromPathSegments  = fmap to gfromPathSegments

instance PathInfo Bool where
  toPathSegments i = [show i]
  fromPathSegments = pToken (const "bool") checkBool
   where checkBool str =
           case reads str of
             [(n,[])] -> Just n
             _ ->        Nothing


type NomicServer       = ServerPartT IO
type RoutedNomicServer = RouteT PlayerCommand NomicServer
type NomicForm a       = HappstackForm NomicServer Html BlazeFormHtml a
--Form m i e v a == Form (ServerPartT IO) Input Html BlazeFormHtml a

--runs a form, resulting in a view and a result if any
runFormNomic :: NomicForm a -> RoutedNomicServer (View Html BlazeFormHtml, Result Html a)
runFormNomic f = liftRouteT $ runForm f "prefix" happstackEnvironment

--transforms a View into an Html (second argument for errors, optional)
getHtmlForm :: View Html BlazeFormHtml -> [(FormRange, Html)] -> Html
getHtmlForm l e = formHtml (unView l e) defaultHtmlConfig

--handler for web routes
nomicSite :: ServerHandle -> (TVar Multi) -> Site PlayerCommand (NomicServer Html)
nomicSite sh tm = setDefault (Noop 0) Site {
      handleSite         = \f url -> unRouteT (routedNomicCommands sh tm url) f
    , formatPathSegments = \u -> (toPathSegments u, [])
    , parsePathSegments  = parseSegments fromPathSegments
}

--Game { gameName      :: GameName,
--                   rules         :: [Rule],
--                   players       :: [PlayerInfo],
--                   variables     :: [Var],
--                   events        :: [EventHandler],
--                   outputs       :: [Output],
--                   victory       :: [PlayerNumber],
--                   currentTime   :: UTCTime}

viewGame :: Game -> PlayerNumber -> RoutedNomicServer Html
viewGame g pn = do
   --ca <- viewActions (actionResults g) pn "Completed Actions"
   --pa <- viewActions actions pn "Pending Actions"
   rf <- ruleForm pn
   os <- viewOutput (outputs g) pn
   vi <- viewInputs $ events g
   --let f (UserEvent mypn s) = mypn == pn
   --let ues = filter f (events g)
   --ue <- viewUserEvent pn
   let inputStrings = filter (\(EH _ _ e _) -> typeOf e == typeOf InputString) $ events g
   ok $ table $ do
      td ! A.id "gameCol" $ do
         div ! A.id "gameName" $ h5 $ string $ "You are viewing game: " ++ gameName g
         div ! A.id "citizens" $ viewPlayers $ players g
      td $ do
         --div ! A.id "actionsresults" $ ca
--         --div ! A.id "pendingactions" $ pa
         div ! A.id "rules" $ viewAllRules g
         div ! A.id "events" $ viewEvents $ events g
         div ! A.id "inputs" $ vi
         div ! A.id "newRule" $ rf
         div ! A.id "Outputs" $ os




viewOutput :: [Output] -> PlayerNumber -> RoutedNomicServer Html
viewOutput os pn = do
    let myos = map snd $ filter (\o -> fst o == pn) os
    ok $ mapM_ viewMessages [myos]

--viewUserEvent :: PlayerNumber -> RoutedNomicServer Html
--viewUserEvent pn = do
--    let myos = map snd $ filter (\o -> fst o == pn) os
--    ok $ mapM_ viewMessages [myos]

--viewAmend :: PlayerNumber -> RoutedNomicServer Html
--viewAmend pn = do
--   linkAmend <- showURL (Amend pn)
--   ok $ button "Amend Constitution " ! A.onclick (fromString $ "location.href='" ++ linkAmend ++ "'")

--viewActions :: [Action] -> PlayerNumber -> String -> RoutedNomicServer Html
--viewActions as pn title = do
--   actions <- forM (zip as [1..]) (viewAction pn)
--   ok $ table $ do
--      caption $ h3 $ string title
--      thead $ do
--         td $ text "Testing Rule"
--         td $ text "Reason"
--         td $ text "Player"
--         td $ text "Result"
--      sequence_ actions

--resolveInputChoice :: Exp [String] -> RuleNumber -> ServerHandle -> Game -> RoutedNomicServer (Either [Action] [String])
--resolveInputChoice exp testing sh g = do
--   inc <- liftRouteT $ lift $ atomically newTChan
--   outc <- liftRouteT $ lift $ atomically newTChan
--   let communication = (Communication inc outc sh)
--   liftRouteT $ lift $ evalStateT (evalStateT (evalExp exp testing) g) communication

-- Enum c => InputChoice c  = InputChoice PlayerNumber String

--     EH {eventNumber :: EventNumber,
--         ruleNumber :: RuleNumber,
--         event :: e,

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
   is <- mapM viewInput ehs
   ok $ table $ do
      caption $ h3 "Inputs"
      mconcat is

viewInput :: EventHandler -> RoutedNomicServer Html
viewInput (EH _ _ (InputChoice pn t def) _) = viewInputChoice def pn t
viewInput (EH _ _ (InputString pn t) _)     = viewInputString pn t
viewInput _ = ok ""


viewInputChoice :: forall c. (Bounded c, Enum c, Show c) => c -> PlayerNumber -> String -> RoutedNomicServer Html
viewInputChoice c pn s = do
   link <- showURL DoInputChoice
   let list = enumFrom (minBound :: c)
   ok $ tr $ H.form ! A.method "POST" ! A.action (fromString link) ! enctype "multipart/form-data;charset=UTF-8"  $ do
      H.label ! A.for "text" $ toHtml s
      mapM_ viewRadio list
      input ! type_ "hidden" ! name "pn" ! value (fromString $ show pn)
      input ! type_  "submit" ! tabindex "4" ! accesskey "S" ! value "Submit"


viewRadio :: (Enum c, Show c) => c -> Html
viewRadio v = input ! type_ "radio" ! name "choices" ! value (fromString $ show v)


viewInputString :: PlayerNumber -> String -> RoutedNomicServer Html
viewInputString pn s = do
   link <- showURL DoInputString
   ok $ tr $ H.form ! A.method "POST" ! A.action (fromString link) ! enctype "multipart/form-data;charset=UTF-8"  $ do
      H.label ! A.for "text" $ toHtml s
      input ! type_ "text" ! name "text" ! A.id "text" ! tabindex "1" ! accesskey "N"
      input ! type_ "hidden" ! name "pn" ! value (fromString $ show pn)
      input ! type_  "submit" ! tabindex "4" ! accesskey "S" ! value "Submit"


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
   ok $ H.form ! A.method "POST" ! A.action (fromString link) ! enctype "multipart/form-data;charset=UTF-8"  $ do
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
         td $ H.a "Join" ! (href $ stringValue join)
         td $ H.a "Leave" ! (href $ fromString leave)
         td $ H.a "Subscribe" ! (href $ fromString subscribe)
         td $ H.a "Unsubscribe" ! (href $ fromString unsubscribe)

newGameForm :: PlayerNumber -> RoutedNomicServer Html
newGameForm pn = do
   link <- showURL NewGame
   ok $ H.form ! A.method "POST" ! A.action (fromString link) ! enctype "multipart/form-data;charset=UTF-8"  $ do
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
          H.title (H.string "Welcome to Nomic!")
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
   lf  <- loginForm
   ok $ H.html $ do
      H.head $ do
        H.title (H.string "Login to Nomic")
        H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomic.css"
        H.meta ! A.httpEquiv "Content-Type" ! content "text/html;charset=utf-8"
        H.meta ! A.name "keywords" ! A.content "Nomic, game, rules, Haskell, auto-reference"
      H.body $ do
        H.div ! A.id "container" $ do
           H.div ! A.id "header" $ "Login to Nomic"
           H.div ! A.id "login" $ lf
           H.div ! A.id "footer" $ "footer"

loginForm :: RoutedNomicServer Html
loginForm = do
   (l, _) <- runFormNomic loginForm'
   link <- showURL PostLogin
   ok $ H.form ! A.method "POST" ! A.action (fromString link) ! enctype "multipart/form-data;charset=UTF-8"  $ getHtmlForm l []

loginForm' :: NomicForm LoginPass
loginForm' = LoginPass <$> (TDB.label "Login: "    *> inputNonEmpty Nothing)
                       <*> (TDB.label "Password: " *> inputNonEmpty Nothing)
                       <*  (submit "Enter Nomic!")

inputNonEmpty :: Maybe String -> NomicForm String
inputNonEmpty v = inputText' (fmap show v) `validate` TD.check "You can not leave this field blank." (not . (== "")) <++ errors
--fbr :: NomicForm ()
--fbr = view H.br

routedNomicCommands :: ServerHandle -> (TVar Multi) -> PlayerCommand -> RoutedNomicServer Html
routedNomicCommands _ _  (Login)                     = loginPage
routedNomicCommands _ tm  (PostLogin)                = postLogin tm
routedNomicCommands _ tm (Noop pn)                   = nomicPageComm pn tm (return ())
routedNomicCommands _ tm (JoinGame pn game)          = nomicPageComm pn tm (joinGame game pn)
routedNomicCommands _ tm (LeaveGame pn)              = nomicPageComm pn tm (leaveGame pn)
routedNomicCommands _ tm (SubscribeGame pn game)     = nomicPageComm pn tm (subscribeGame game pn)
routedNomicCommands _ tm (UnsubscribeGame pn game)   = nomicPageComm pn tm (unsubscribeGame game pn)
routedNomicCommands sh tm (NewRule)                  = newRule sh tm
routedNomicCommands _ tm (NewGame)                   = newGameWeb tm


--execute the given instructions (Comm) and embed the result in a web page
nomicPageComm :: PlayerNumber -> (TVar Multi) -> StateT Multi IO () -> RoutedNomicServer Html
nomicPageComm pn tm comm = execCommand tm comm >> nomicPageServer pn tm

execCommand :: (TVar Multi) -> StateT Multi IO a -> RoutedNomicServer a
execCommand tm sm = do
    m <- liftRouteT $ lift $ atomically $ readTVar tm
    (a, m') <- liftRouteT $ lift $ runStateT sm m
    liftRouteT $ lift $ atomically $ writeTVar tm m'
    return a

newRule :: ServerHandle -> (TVar Multi) -> RoutedNomicServer Html
newRule sh tm = do
   methodM POST -- only accept a post method
   mbEntry <- getData -- get the data
   case mbEntry of
      Left a -> error $ "error: newRule " ++ (concat a)
      Right (NewRuleForm name text code pn) -> do
         debugM ("Rule submitted: name =" ++ name ++ "\ntext=" ++ text ++ "\ncode=" ++ code ++ "\npn=" ++ (show pn))
         nomicPageComm pn tm (submitRule name text code pn sh)


newGameWeb :: (TVar Multi) -> RoutedNomicServer Html
newGameWeb tm = do
   methodM POST
   mbEntry <- getData
   case mbEntry of
      Left a                      -> error $ "error: newGame" ++ (concat a)
      Right (NewGameForm name pn) -> nomicPageComm pn tm (newGame name pn)


nomicPageServer :: PlayerNumber -> (TVar Multi) -> RoutedNomicServer Html
nomicPageServer pn tm = do
   multi <- liftRouteT $ lift $ atomically $ readTVar tm
   nomicPage multi pn


postLogin :: (TVar Multi) -> RoutedNomicServer Html
postLogin tm = do
  liftRouteT $ lift $ putStrLn "postLogin"
  methodM POST
  (l, r) <- runFormNomic loginForm'
  case r of
    (Error e) -> ok $ getHtmlForm l e --error $ "error: postLogin"
    Ok (LoginPass login password)  -> do
      liftRouteT $ lift $ putStrLn $ "login:" ++ login
      liftRouteT $ lift $ putStrLn $ "password:" ++ password
      mpn <- execCommand tm $ newPlayerWeb login password
      case mpn of
         Just pn -> do
            link <- showURL $ Noop pn
            seeOther link $ string "Redirecting..."
         Nothing -> seeOther ("/Login?status=fail" :: String) $ string "Redirecting..."





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
    m <- lift $ atomically $ readTVar tm
    decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
    html <- implSite "http://localhost:8000/" "" (nomicSite sh tm)
    return $ toResponse html]


instance ToMessage (Response, Multi) where
    toContentType _ = B.pack "text/plain; charset=UTF-8"
    toResponse (r,m) = r -- >> LU.fromString $ show m


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

