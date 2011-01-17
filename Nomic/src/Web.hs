
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, TemplateHaskell,
   EmptyDataDecls, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable, PackageImports, GADTs,
   ScopedTypeVariables#-}

-- {-# OPTIONS_GHC -F -pgmFtrhsx #-}
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
import NamedRule
import Control.Monad
import Action
import Paths_Nomic
import Control.Monad.State
import Data.Monoid
import Observable
import Data.String
import Control.Concurrent.STM
import Comm
import Language.Haskell.Interpreter.Server
import Control.Monad.Loops
import Text.Blaze.Internal (ChoiceString(..))
import Control.Applicative (Applicative(..), (<$>))
import Text.Digestive             (Environment(..), Form, FormId, FormRange(..), Result(..), View(..), (<++), (++>), check, runForm, validate, view)
import qualified Text.Digestive as TD (check)
import qualified Data.Text        as T
import           Data.Text        (Text)
--import HSP
import Text.Digestive.Forms       (FormInput(..))
import qualified Data.ByteString.Lazy.UTF8 as LB (toString)
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Text.Encoding as TL (decodeUtf8)
import Data.String.UTF8 (toString)

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

data PlayerCommand = Noop            PlayerNumber
                   | JoinGame        PlayerNumber GameName
                   | LeaveGame       PlayerNumber
                   | SubscribeGame   PlayerNumber GameName
                   | UnsubscribeGame PlayerNumber GameName
                   | DoAction        PlayerNumber ActionNumber ActionResult
                   | Amend           PlayerNumber
                   | NewRule         PlayerNumber RuleName RuleText RuleCode
                   | NewGame         PlayerNumber GameName
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

nomicSite :: ServerHandle -> Site PlayerCommand (NomicServer Html)
nomicSite sh = setDefault (Noop 0) Site {
      handleSite         = \f url -> unRouteT (routedNomicHandle sh url) f
    , formatPathSegments = \u -> (toPathSegments u, [])
    , parsePathSegments  = parseSegments fromPathSegments
}

viewGame :: Game -> PlayerNumber -> [Action] -> ServerHandle -> RoutedNomicServer Html
viewGame g pn actions sh = do
   ca <- viewActions (actionResults g) g pn sh "Completed Actions"
   pa <- viewActions actions g pn sh "Pending Actions"
   a <- viewAmend pn
   ok $ table $ do
      td ! A.id "gameCol" $ do
         div ! A.id "gameName" $ h5 $ string $ "You are viewing game: " ++ gameName g
         div ! A.id "citizens" $ viewPlayers $ players g
         div ! A.id "amend" $ a
      td $ do
         div ! A.id "actionsresults" $ ca
         div ! A.id "pendingactions" $ pa
         div ! A.id "rules" $ viewRules g
         div ! A.id "newRule" $ ruleForm pn

viewAmend :: PlayerNumber -> RoutedNomicServer Html
viewAmend pn = do
   linkAmend <- showURL (Amend pn)
   ok $ button "Amend Constitution " ! A.onclick (fromString $ "location.href='" ++ linkAmend ++ "'")

viewActions :: [Action] -> Game -> PlayerNumber -> ServerHandle -> String -> RoutedNomicServer Html
viewActions as g pn sh title = do
   actions <- forM (zip as [1..]) (viewAction pn g sh)
   ok $ table $ do
      caption $ h3 $ string title
      thead $ do
         td $ text "Testing Rule"
         td $ text "Tested Rule"
         td $ text "Action"
         td $ text "Result"
      sequence_ actions

resolveInputChoice :: Obs [String] -> RuleNumber -> RuleNumber -> ServerHandle -> Game -> RoutedNomicServer (Either [Action] [String])
resolveInputChoice o testing tested sh g = do
   inc <- liftRouteT $ lift $ atomically newTChan
   outc <- liftRouteT $ lift $ atomically newTChan
   let communication = (Communication inc outc sh)
   liftRouteT $ lift $ runWithComm communication $ evalStateT (evalObs' o tested testing) g


viewAction :: PlayerNumber -> Game -> ServerHandle -> (Action, ActionNumber) -> RoutedNomicServer Html
viewAction pn g sh (a, n) = do
   let buildLink :: String -> RoutedNomicServer Html
       buildLink a = do link <- showURL (DoAction pn n a)
                        liftRouteT $ lift $ putStrLn link
                        return $ td $ H.a (string a) ! (href $ stringValue $ link)
   let os = (case (Action.action a) of
             InputChoice _ _ choices -> choices
             _ -> error "only InputChoice is allowed as action") :: Obs [String]

   let testing = Action.testing a
   let tested  = Action.tested a
   eas <- resolveInputChoice os testing tested sh g
   ls <- case eas of
               Left _ -> ok $ [string "actions left to complete"]
               Right as -> sequence $ map buildLink as
   ok $ tr $ do
   td $ showHtml $ testing
   td $ showHtml $ tested
   td $ showHtml $ Action.action a
   td $ do
      case (Action.result a) of
         Just a -> showHtml a
         Nothing -> table $ do
            sequence_ ls


viewRules :: Game -> Html
viewRules g = do
   h4 "Rules"
   viewNamedRules (h5 "Constitution:") $ activeRules g
   viewNamedRules (h5 "Pending rules:") $ pendingRules g
   viewNamedRules (h5 "Suppressed rules:") $ (suppressedRules g) ++ (rejectedRules g)


viewNamedRules :: Html -> [NamedRule] -> Html
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

viewNamedRule :: NamedRule -> Html
viewNamedRule nr = tr $ do
   td $ showHtml $ rNumber nr
   td $ string $ rName nr
   td $ string $ rText nr
   td $ showHtml $ rProposedBy nr
   td $ string $ rule nr
   td $ showHtml $ rejectedBy nr


ruleForm :: PlayerNumber -> Html
ruleForm pn = do
   H.form ! A.method "POST" ! A.action "/NewRule" ! enctype "multipart/form-data;charset=UTF-8"  $ do
      H.label ! for "name" $ "Name"
      input ! type_ "text" ! name "name" ! A.id "name" ! tabindex "1" ! accesskey "N"
      H.label ! for "text" $ "Text"
      input ! type_ "text" ! name "text" ! A.id "text" ! tabindex "2" ! accesskey "T"
      H.br
      H.label ! for "text" $ "Code"
      input ! type_ "text" ! name "code" ! A.id "code" ! tabindex "3" ! accesskey "C"
      input ! type_ "hidden" ! name "pn" ! value (fromString $ show pn)
      input ! type_  "submit" ! tabindex "4" ! accesskey "S" ! value "Submit rule!"


viewPlayers :: [PlayerInfo] -> Html
viewPlayers pis = do
   table $ do
      caption $ h5 "Players in game:"
      mapM_ viewPlayer pis


viewPlayer :: PlayerInfo -> Html
viewPlayer pi = tr $ td $ showHtml pi


viewMulti :: PlayerNumber -> Multi -> ServerHandle -> [String] -> [Action] -> RoutedNomicServer Html
viewMulti pn m sh mess actions = do
   gns <- viewGameNames pn (games m)
   g <- case getPlayersGame pn m of
            Just g -> viewGame g pn actions sh
            Nothing -> ok $ h5 "Not in game"
   ok $ do
      div ! A.id "gameList" $ gns
      div ! A.id "game" $ g
      div ! A.id "message" $ viewMessages mess


viewMessages :: [String] -> Html
viewMessages mess = mapM_ (\s -> string s >> br) mess


viewGameNames :: PlayerNumber -> [Game] -> RoutedNomicServer Html
viewGameNames pn gs = do
   gn <- mapM (viewGameName pn) gs
   ok $ do
      h5 "Games:"
      table $ do
         case gs of
            [] -> tr $ td $ "No Games"
            _ ->  sequence_ gn
      newGameForm pn

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
         td $ H.a "Subscribe" ! (href $ fromString $ subscribe)
         td $ H.a "Unsubscribe" ! (href $ fromString $ unsubscribe)

newGameForm :: PlayerNumber -> Html
newGameForm pn = do
   H.form ! A.method "POST" ! A.action "/NewGame" ! enctype "multipart/form-data;charset=UTF-8"  $ do
      H.label ! for "name" $ "Game name:"
      input ! type_ "text" ! name "name" ! A.id "name" ! tabindex "1" ! accesskey "G"
      input ! type_ "hidden" ! name "pn" ! value (fromString $ show pn)
      input ! type_  "submit" ! tabindex "2" ! accesskey "S" ! value "Create New Game!"

--type NomicForm a = HappstackForm IO String BlazeFormHtml a --NewGameForm
--NewGameForm
--Form (ServerPartT IO) Input String BlazeFormHtml a

--type AppForm = Form (XMLGenT NomicServer) Input String [XMLGenT NomicServer XML]

--instance FormInput Input (FilePath, FilePath) where
--    getInputString i =
--        case inputValue i of
--          (Left _)  ->  Nothing
--          (Right bs) -> Just $ LB.toString bs
--
--    getInputText i =
--        case inputValue i of
--          (Left _)  ->  Nothing
--          (Right bs) -> Just $ TL.toStrict $ TL.decodeUtf8 bs
--
--    getInputFile i =
--      case (inputFilename i, inputValue i) of
--        (Just fileName, Left tmpFile) -> Just (fileName, tmpFile)
--        _                             -> Nothing

--demo :: Form (ServerPartT IO) String Html BlazeFormHtml ()
--demo = submit "submit"
----submit :: Monad m
----       => String                            -- ^ Text on the submit button
----       -> Form m String e BlazeFormHtml ()  -- ^ Submit button
--
--type NomicForm a = Form (ServerPartT IO) String Html BlazeFormHtml a
--
--demoForm :: NomicForm (String, String)
--demoForm =
--    (,) <$> ((TDB.label "greeting: " ++> inputNonEmpty Nothing)) -- <* br)
--        <*> ((TDB.label "noun: "     ++> inputNonEmpty Nothing))  -- <* br)
--        <*  (submit "submit")
--    where
--      --br :: NomicForm ()
--      --br = view H.br
--      -- make sure the fields are not blank, shows errors in line if they are
--      inputNonEmpty :: Maybe String -> NomicForm String
--      inputNonEmpty v =
--          (inputText v `validate` (TD.check "You can not leave this field blank." (/= "")) <++ errors)

nomicPage :: Multi -> PlayerNumber -> ServerHandle -> [String] -> [Action] -> RoutedNomicServer Html
nomicPage multi pn sh mess actions = do
   m <- viewMulti pn multi sh mess actions

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
             H.div ! A.id "footer" $ "footer"

loginPage :: Html
loginPage = do
   H.html $ do
      H.head $ do
        H.title (H.string "Login to Nomic")
        H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomic.css"
        H.meta ! A.httpEquiv "Content-Type" ! content "text/html;charset=utf-8"
        H.meta ! A.name "keywords" ! A.content "Nomic, game, rules, Haskell, auto-reference"
      H.body $ do
        H.div ! A.id "container" $ do
           H.div ! A.id "header" $ "Login to Nomic"
           H.div ! A.id "login" $ loginForm
           H.div ! A.id "footer" $ "footer"

loginForm :: Html
loginForm = do
   H.form ! A.method "POST" ! A.action "/postLogin" ! enctype "multipart/form-data;charset=UTF-8"  $ do
      H.label ! for "login" $ "Login"
      input ! type_ "text" ! name "login" ! A.id "login" ! tabindex "1" ! accesskey "L"
      H.label ! for "password" $ "Password"
      input ! type_ "text" ! name "password" ! A.id "password" ! tabindex "2" ! accesskey "P"
      input ! type_  "submit" ! tabindex "3" ! accesskey "S" ! value "Enter Nomic!"



routedNomicHandle :: ServerHandle -> PlayerCommand -> RoutedNomicServer Html
routedNomicHandle sh (Noop pn)                   = nomicPageComm pn sh (return ())
routedNomicHandle sh (JoinGame pn game)          = nomicPageComm pn sh (joinGame game pn)
routedNomicHandle sh (LeaveGame pn)              = nomicPageComm pn sh (leaveGame pn)
routedNomicHandle sh (SubscribeGame pn game)     = nomicPageComm pn sh (subscribeGame game pn)
routedNomicHandle sh (UnsubscribeGame pn game)   = nomicPageComm pn sh (unsubscribeGame game pn)
routedNomicHandle sh (Amend pn)                  = nomicPageComm pn sh (amendConstitution pn)
routedNomicHandle sh (DoAction pn an ar)         = nomicPageComm pn sh (doAction' an ar pn)
routedNomicHandle sh (NewRule pn name text code) = nomicPageComm pn sh (submitRule name text code pn)
routedNomicHandle sh (NewGame pn game)           = nomicPageComm pn sh (newGame game pn)



--RouteT PlayerCommand ServerPartT IO Response
nomicPageComm :: PlayerNumber -> ServerHandle -> Comm () -> RoutedNomicServer Html
nomicPageComm pn sh comm = do
   --l <- showURL $ NewRule 1 "titi" "tata" "toto"
   --liftRouteT $ lift $ putStrLn l
   inc <- liftRouteT $ lift $ atomically newTChan
   outc <- liftRouteT $ lift $ atomically newTChan
   let communication = (Communication inc outc sh)
   liftRouteT $ lift $ runWithComm communication comm
   pendingsActions <- liftRouteT $ lift $ runWithComm communication $ getPendingActions pn
   mess <- liftRouteT $ lift $ atomically $ whileM (isEmptyTChan outc >>= (return . not)) (readTChan outc)
   nomicPageServer pn sh mess pendingsActions


newRule :: ServerHandle -> NomicServer Response
newRule sh = do
   methodM POST -- only accept a post method
   mbEntry <- getData -- get the data
   case mbEntry of
      Nothing -> error $ "error: newRule"
      Just (NewRuleForm name text code pn) -> do
         seeOther ("/Nomic/newrule/" ++ (show pn) ++ "/" ++ name ++ "/" ++ text ++ "/" ++ code) $ toResponse ("Redirecting..."::String)


newGameWeb :: ServerHandle -> NomicServer Response
newGameWeb sh = do
   methodM POST -- only accept a post method
   mbEntry <- getData -- get the data
   case mbEntry of
      Nothing -> error $ "error: newGame"
      Just (NewGameForm name pn)  -> do
         seeOther ("/Nomic/newgame/" ++ (show pn) ++ "/" ++ name) $ toResponse ("Redirecting..."::String)


nomicPageServer :: PlayerNumber -> ServerHandle -> [String] -> [Action] -> RoutedNomicServer Html
nomicPageServer pn sh mess actions = do
   multi <- liftRouteT $ lift $ query GetMulti
   nomicPage multi pn sh mess actions


postLogin :: NomicServer Response
postLogin = do
  lift $ putStrLn $ "postLogin"
  methodM POST -- only accept a post method
  mbEntry <- getData -- get the data
  case mbEntry of
    Nothing -> error $ "error: postLogin"
    Just (LoginPass login password)  -> do
      lift $ putStrLn $ "login:" ++ login
      lift $ putStrLn $ "password:" ++ password
      mpn <- liftIO $ newPlayerWeb login password
      case mpn of
         Just pn -> do
            --nomicLink <- showURL $ PlayerCommand pn Nothing
            --seeOther nomicLink $ toResponse ("Redirecting..."::String)
            seeOther ("/Nomic/noop/" ++ (show pn)) $ toResponse ("Redirecting..."::String)
         Nothing -> seeOther ("/Login?status=fail" :: String) $ toResponse ("Redirecting..."::String)


newPlayerWeb :: PlayerName -> PlayerPassword -> IO (Maybe PlayerNumber)
newPlayerWeb name pwd = do
   --find that name among the list
   mpn <- query $ FindPlayer name
   pn <- case mpn of
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
   return pn


launchWebServer :: ServerHandle -> IO ()
launchWebServer sh = do
   putStrLn "Starting web server...\nTo connect, drive your browser to \"http://localhost:8000/Login\""
   d <- liftIO getDataDir
   simpleHTTP nullConf $ mconcat [dir "postLogin" $ postLogin,
                                  fileServe [] d,
                                  dir "Login" $ ok $ toResponse $ loginPage,
                                  dir "NewRule" $ newRule sh,
                                  dir "NewGame" $ newGameWeb sh,
                                  dir "Nomic" $ do
                                     html <- implSite "http://localhost:8000/Nomic/" "" (nomicSite sh)
                                     ok $ toResponse html
                                  ]

-- | a loop that will handle client communication
--messages :: TChan String -> MVar String
--messages chan = do
--   s <- atomically $ readTChan chan
--   hPutStr (handle cc) s
--   messages chan

instance ToMessage H.Html where
    toContentType _ = B.pack "text/html; charset=UTF-8"
    toMessage = LU.fromString . renderHtml


-- this tells happstack how to turn post data into a datatype using 'withData'
instance FromData LoginPass where
  fromData = do
    login  <- look "login" `mplus` (error "need login")
    password <- look "password" `mplus` (error "need password")
    return $ LoginPass login password


instance FromData NewRuleForm where
  fromData = do
    name  <- look "name" `mplus` (error "need rule name")
    text <- look "text" `mplus` (error "need rule text")
    code <- look "code" `mplus` (error "need rule code")
    pn <- lookRead "pn" `mplus` (error "need player number")
    return $ NewRuleForm name text code pn

instance FromData NewGameForm where
  fromData = do
    name  <- look "name" `mplus` (error "need rule name")
    pn <- lookRead "pn" `mplus` (error "need player number")
    return $ NewGameForm name pn

