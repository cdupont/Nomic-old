
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, TemplateHaskell,
   EmptyDataDecls, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable, PackageImports, GADTs,
   ScopedTypeVariables#-}

module Web where

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
import NamedRule
import Control.Monad
import Action
import Paths_Nomic
import Control.Monad.State
import Data.Monoid
import Data.String
import Control.Concurrent.STM
import Comm
import Language.Haskell.Interpreter.Server
import Control.Monad.Loops
import Control.Applicative (Applicative(..), (<$>))
import Text.Digestive
import qualified Text.Digestive as TD (check)
import Expression
import Evaluation


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
                   | DoAction        PlayerNumber ActionNumber ActionResult
                   | Amend           PlayerNumber
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
nomicSite :: ServerHandle -> Site PlayerCommand (NomicServer Html)
nomicSite sh = setDefault (Noop 0) Site {
      handleSite         = \f url -> unRouteT (routedNomicCommands sh url) f
    , formatPathSegments = \u -> (toPathSegments u, [])
    , parsePathSegments  = parseSegments fromPathSegments
}

viewGame :: Game -> PlayerNumber -> [Action] -> RoutedNomicServer Html
viewGame g pn actions = do
   ca <- viewActions (actionResults g) pn "Completed Actions"
   pa <- viewActions actions pn "Pending Actions"
   a <- viewAmend pn
   rf <- ruleForm pn
   ok $ table $ do
      td ! A.id "gameCol" $ do
         div ! A.id "gameName" $ h5 $ string $ "You are viewing game: " ++ gameName g
         div ! A.id "citizens" $ viewPlayers $ players g
         div ! A.id "amend" $ a
      td $ do
         div ! A.id "actionsresults" $ ca
         div ! A.id "pendingactions" $ pa
         div ! A.id "rules" $ viewRules g
         div ! A.id "newRule" $ rf

viewAmend :: PlayerNumber -> RoutedNomicServer Html
viewAmend pn = do
   linkAmend <- showURL (Amend pn)
   ok $ button "Amend Constitution " ! A.onclick (fromString $ "location.href='" ++ linkAmend ++ "'")

viewActions :: [Action] -> PlayerNumber -> String -> RoutedNomicServer Html
viewActions as pn title = do
   actions <- forM (zip as [1..]) (viewAction pn)
   ok $ table $ do
      caption $ h3 $ string title
      thead $ do
         td $ text "Testing Rule"
         td $ text "Reason"
         td $ text "Player"
         td $ text "Result"
      sequence_ actions

resolveInputChoice :: Exp [String] -> RuleNumber -> ServerHandle -> Game -> RoutedNomicServer (Either [Action] [String])
resolveInputChoice exp testing sh g = do
   inc <- liftRouteT $ lift $ atomically newTChan
   outc <- liftRouteT $ lift $ atomically newTChan
   let communication = (Communication inc outc sh)
   liftRouteT $ lift $ runWithComm communication $ evalStateT (evalExp exp testing) g


viewAction :: PlayerNumber -> (Action, ActionNumber) -> RoutedNomicServer Html
viewAction pn (a, n) = do
   let buildLink :: String -> RoutedNomicServer Html
       buildLink a = do link <- showURL (DoAction pn n a)
                        return $ td $ H.a (string a) ! (href $ stringValue link)
   ls <- mapM buildLink (choices $ Expression.action a)
   ok $ tr $ do
   td $ string . show $ Expression.aRuleNumber a
   td $ string . show $ reason $ Expression.action a
   td $ string . show $ player $ Expression.action a
   td $ do
      case (Expression.result a) of
         Just a -> string . show $ a
         Nothing -> table $ do
            sequence_ ls


viewRules :: Game -> Html
viewRules g = do
   h4 "Rules"
   viewNamedRules (h5 "Constitution:") $ activeRules g
   viewNamedRules (h5 "Pending rules:") $ pendingRules g
   viewNamedRules (h5 "Suppressed rules:") $ (suppressedRules g) ++ (rejectedRules g)


viewNamedRules :: Html -> [Rule] -> Html
viewNamedRules _ [] = return ()
viewNamedRules title nrs = do
   table $ do
      caption $ h3 title
      thead $ do
         td $ text "Number"
         td $ text "Name"
         td $ text "Description"
         td $ text "Proposed by"
         td $ text "Code of the rule"
         td $ text "Suppressed by"
      forM_ nrs viewNamedRule

viewNamedRule :: Rule -> Html
viewNamedRule nr = tr $ do
   td $ string . show $ rNumber nr
   td $ string $ rName nr
   td $ string $ rDescription nr
   td $ string . show $ rProposedBy nr
   td $ string $ rRuleCode nr
   td $ string . show $ rejectedBy nr


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
viewPlayer pi = tr $ td $ string $ show $ pi


viewMulti :: PlayerNumber -> Multi -> [String] -> [Action] -> RoutedNomicServer Html
viewMulti pn m mess actions = do
   gns <- viewGameNames pn (games m)
   g <- case getPlayersGame pn m of
            Just g -> viewGame g pn actions
            Nothing -> ok $ h5 "Not in game"
   ok $ do
      div ! A.id "gameList" $ gns
      div ! A.id "game" $ g
      div ! A.id "message" $ viewMessages mess


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

--newGameForm' :: PlayerNumber -> omicForm NewGameForm
--newGameForm' pn = NewGameForm <$> (TDB.label "Game name: "    *> inputText Nothing)
--                              <*> (inputHidden $ pn)
--                              <*  (submit "Create New Game!")

--demoForm :: NomicForm (String, String)
--demoForm =
--    (,) <$> ((TDB.label "greeting: " ++> inputNonEmpty Nothing)) -- <* br)
--        <*> ((TDB.label "noun: "     ++> inputNonEmpty Nothing))  -- <* br)
--        <*  (submit "submit")
    --where
      --br :: NomicForm ()
      --br = view H.br
      -- make sure the fields are not blank, shows errors in line if they are
      --inputNonEmpty :: Maybe String -> NomicForm String
      --inputNonEmpty v =
      --    (inputText (fmap show v) `validate` (TD.check "You can not leave this field blank." (not . T.null)) <++ errors)

nomicPage :: Multi -> PlayerNumber -> [String] -> [Action] -> RoutedNomicServer Html
nomicPage multi pn mess actions = do
   m <- viewMulti pn multi mess actions
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
inputNonEmpty v = inputText (fmap show v) `validate` TD.check "You can not leave this field blank." (not . (== "")) <++ errors
--fbr :: NomicForm ()
--fbr = view H.br

--choose the instruction to execute, based on the PlayerCommand
routedNomicCommands :: ServerHandle -> PlayerCommand -> RoutedNomicServer Html
routedNomicCommands _  (Login)                     = loginPage
routedNomicCommands _  (PostLogin)                 = postLogin
routedNomicCommands sh (Noop pn)                   = nomicPageComm pn sh (return ())
routedNomicCommands sh (JoinGame pn game)          = nomicPageComm pn sh (joinGame game pn)
routedNomicCommands sh (LeaveGame pn)              = nomicPageComm pn sh (leaveGame pn)
routedNomicCommands sh (SubscribeGame pn game)     = nomicPageComm pn sh (subscribeGame game pn)
routedNomicCommands sh (UnsubscribeGame pn game)   = nomicPageComm pn sh (unsubscribeGame game pn)
routedNomicCommands sh (Amend pn)                  = nomicPageComm pn sh (amendConstitution pn)
routedNomicCommands sh (DoAction pn an ar)         = nomicPageComm pn sh (doAction' an ar pn)
routedNomicCommands sh (NewRule)                   = newRule sh
routedNomicCommands sh (NewGame)                   = newGameWeb sh


--execute the given instructions (Comm) and embed the result in a web page
nomicPageComm :: PlayerNumber -> ServerHandle -> Comm () -> RoutedNomicServer Html
nomicPageComm pn sh comm = do
   --gets empty in out channels
   communication <- liftRouteT $ lift $ getEmptyComm sh
   --execute the command
   liftRouteT $ lift $ runWithComm communication comm
   --get the pending actions
   pendingsActions <- liftRouteT $ lift $ runWithComm communication $ getPendingActions pn
   --get the messages
   mess <- liftRouteT $ lift $ atomically $ whileM (fmap not (isEmptyTChan $ cout communication)) (readTChan $ cout communication)
   --display the result
   nomicPageServer pn mess pendingsActions


newRule :: ServerHandle -> RoutedNomicServer Html
newRule sh = do
   methodM POST -- only accept a post method
   mbEntry <- getData -- get the data
   case mbEntry of
      Left a -> error $ "error: newRule " ++ (concat a)
      Right (NewRuleForm name text code pn) -> do
         debugM ("Rule submitted: name =" ++ name ++ "\ntext=" ++ text ++ "\ncode=" ++ code ++ "\npn=" ++ (show pn))
         nomicPageComm pn sh (submitRule name text code pn)


newGameWeb :: ServerHandle -> RoutedNomicServer Html
newGameWeb sh = do
   methodM POST
   mbEntry <- getData
   case mbEntry of
      Left a                      -> error $ "error: newGame" ++ (concat a)
      Right (NewGameForm name pn) -> nomicPageComm pn sh (newGame name pn)


nomicPageServer :: PlayerNumber -> [String] -> [Action] -> RoutedNomicServer Html
nomicPageServer pn mess actions = do
   multi <- liftRouteT $ lift $ query GetMulti
   nomicPage multi pn mess actions


postLogin :: RoutedNomicServer Html
postLogin = do
  liftRouteT $ lift $ putStrLn "postLogin"
  methodM POST
  (l, r) <- runFormNomic loginForm'
  case r of
    (Error e) -> ok $ getHtmlForm l e --error $ "error: postLogin"
    Ok (LoginPass login password)  -> do
      liftRouteT $ lift $ putStrLn $ "login:" ++ login
      liftRouteT $ lift $ putStrLn $ "password:" ++ password
      mpn <- liftRouteT $ liftIO $ newPlayerWeb login password
      case mpn of
         Just pn -> do
            link <- showURL $ Noop pn
            seeOther link $ string "Redirecting..."
         Nothing -> seeOther ("/Login?status=fail" :: String) $ string "Redirecting..."


newPlayerWeb :: PlayerName -> PlayerPassword -> IO (Maybe PlayerNumber)
newPlayerWeb name pwd = do
   --find that name among the list
   mpn <- query $ FindPlayer name
   case mpn of
      Just pl -> do
         putStrLn $ "Trying name:" ++ mPlayerName pl
         case pwd == mPassword pl of
            True -> do
               putStrLn "password OK"
               return $ Just $ mPlayerNumber pl
            False -> do
               putStrLn "password false"
               return Nothing
      Nothing -> do
         putStrLn "New player"
         --add the new player to the list
         pn <- query GetNewPlayerNumber --CDU to check
         update $ NewPlayerU PlayerMulti { mPlayerNumber = pn, mPlayerName = name, mPassword = pwd, inGame = Nothing}
         return (Just pn)



launchWebServer :: ServerHandle -> IO ()
launchWebServer sh = do
   putStrLn "Starting web server...\nTo connect, drive your browser to \"http://localhost:8000/Login\""
   d <- getDataDir
   simpleHTTP nullConf $ mconcat [fileServe [] d,
                                  do
                                   decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
                                   html <- implSite "http://localhost:8000/" "" (nomicSite sh)
                                   return $ toResponse html]

-- | a loop that will handle client communication
--messages :: TChan String -> MVar String
--messages chan = do
--   s <- atomically $ readTChan chan
--   hPutStr (handle cc) s
--   messages chan

--instance ToMessage H.Html where
--    toContentType _ = B.pack "text/html; charset=UTF-8"
--    toMessage = LU.fromString . renderHtml

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

