
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Web where

import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (dir)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


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


type SessionNumber = Integer

data LoginPass = LoginPass { login :: PlayerName,
                             password :: PlayerPassword}

-- | associate a player number with a handle
data PlayerClient = PlayerClient { cPlayerNumber :: PlayerNumber}
                                   deriving (Eq, Show)

-- | A structure to hold the active games and players
data Server = Server { playerClients :: [PlayerClient]}
                       deriving (Eq, Show)

data PlayerCommand = PlayerCommand { playerNumber :: PlayerNumber,
                                     command :: Maybe (Command, GameName, String, String)}
                                   deriving (Eq, Show)

data NewRule = NewRule { ruleName :: String,
                         ruleText :: String,
                         ruleCode :: String,
                         pn :: PlayerNumber }

-- | A State to pass around active games and players.
-- Furthermore, the output are to be made with Comm to output to the right console.
type ServerState = StateT Server IO ()

data Command = JoinGame
             | LeaveGame
             | SubscribeGame
             | UnsubscribeGame
             | DoAction
             | Amend
             | Noop
             deriving (Eq, Show, Read)

--webCommands = [("join",          JoinGame),
--               ("leave",         LeaveGame),
--               ("subscribe",     SubscribeGame),
--               ("unsubscribe",   UnsubscribeGame)]


viewGame :: Game -> PlayerNumber -> [Action] -> Html
viewGame g pn actions = do
   div ! A.id "gameName" $ h5 $ string $ "You are viewing game:" ++ gameName g
   div ! A.id "citizens" $ viewPlayers $ players g
   div ! A.id "amend" $ viewAmend pn
   div ! A.id "actionsresults" $ viewActions (actionResults g) pn "Completed Actions"
   div ! A.id "pendingactions" $ viewActions actions pn "Pending Actions"
   div ! A.id "rules" $ viewRules g
   div ! A.id "newRule" $ ruleForm pn

viewAmend :: PlayerNumber -> Html
viewAmend pn = H.a "Amend Constitution " ! (href $ fromString $ "Nomic?pn=" ++ (show pn) ++ "&query=Amend&game=&actionNumber=&actionResult=")

viewActions :: [Action] -> PlayerNumber -> String -> Html
viewActions as pn title = do
   table $ do
      caption $ h3 $ string title
      thead $ do
         td $ text "Testing Rule"
         td $ text "Tested Rule"
         td $ text "Action"
         td $ text "Result"
      forM_ (zip as [1..]) (viewAction pn)

viewAction :: PlayerNumber -> (Action, Int) -> Html
viewAction pn (a, n) = tr $ do
   td $ showHtml $ Action.testing a
   td $ showHtml $ Action.tested a
   td $ showHtml $ Action.action a
   td $ do
      case (Action.result a) of
         Just a -> showHtml a
         Nothing -> do
            H.a "for " !     (href $ fromString $ "Nomic?pn=" ++ (show pn) ++ "&query=DoAction&game=&actionNumber=" ++ (show n) ++ "&actionResult=True")
            H.a "against" ! (href $ fromString $ "Nomic?pn=" ++ (show pn) ++ "&query=DoAction&game=&actionNumber=" ++ (show n) ++ "&actionResult=False")


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


viewMulti :: PlayerNumber -> Multi -> [String] -> [Action] -> Html
viewMulti pn m mess actions = do
   div ! A.id "gameList"  $ do
      viewGameNames pn (games m)
   div ! A.id "game" $ do
      case getPlayersGame pn m of
         Just g -> viewGame g pn actions
         Nothing -> h5 "Not in game"
   div ! A.id "message" $ do
      viewMessages mess


viewMessages :: [String] -> Html
viewMessages mess = mapM_ (\s -> string s >> br) mess


viewGameNames :: PlayerNumber -> [Game] -> Html
viewGameNames pn gs = do
   h5 "Games:"
   table $ do
      case gs of
         [] -> tr $ td $ "No Games"
         _ -> mapM_ (viewGameName pn) gs

viewGameName :: PlayerNumber -> Game -> Html
viewGameName pn g = do
   tr $ do
      td $ string $ gameName g
      td $ H.a "Join" ! (href $ fromString $ "Nomic?pn=" ++ (show pn) ++ "&query=JoinGame&actionNumber=null&actionResult=null&game=" ++ (gameName g))
      td $ H.a "Leave" ! (href $ fromString $ "Nomic?pn=" ++ (show pn) ++ "&query=LeaveGame&actionNumber=null&actionResult=null&game=" ++ (gameName g))
      td $ H.a "Subscribe" ! (href $ fromString $ "Nomic?pn=" ++ (show pn) ++ "&query=SubscribeGame&actionNumber=null&actionResult=null&game=" ++ (gameName g))
      td $ H.a "Unsubscribe" ! (href $ fromString $ "Nomic?pn=" ++ (show pn) ++ "&query=UnsubscribeGame&actionNumber=&actionResult=null&game=" ++ (gameName g))

nomicPage :: Multi -> PlayerNumber -> [String] -> [Action] -> Html
nomicPage multi pn mess actions =
    H.html $ do
      H.head $ do
        H.title (H.string "Welcome to Nomic!")
        H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomic.css"
        H.meta ! http_equiv "Content-Type" ! content "text/html;charset=utf-8"
        H.meta ! A.name "keywords" ! A.content "Nomic, game, rules, Haskell, auto-reference"
      H.body $ do
        H.div ! A.id "container" $ do
           H.div ! A.id "header" $ string $ "Welcome to Nomic, " ++ (getPlayersName pn multi) ++ "!"
           H.div ! A.id "multi" $ viewMulti pn multi mess actions
           H.div ! A.id "footer" $ "footer"

loginPage :: Html
loginPage = do
   H.html $ do
      H.head $ do
        H.title (H.string "Login to Nomic")
        H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomic.css"
        H.meta ! http_equiv "Content-Type" ! content "text/html;charset=utf-8"
        H.meta ! A.name "keywords" ! A.content "Nomic, game, rules, Haskell, auto-reference"
      H.body $ do
        H.div ! A.id "container" $ do
           H.div ! A.id "header" $ "Login to Nomic"
           H.div ! A.id "multi" $ loginForm
           H.div ! A.id "footer" $ "footer"

loginForm :: Html
loginForm = do
   H.form ! A.method "POST" ! A.action "/postLogin" ! enctype "multipart/form-data;charset=UTF-8"  $ do
      H.label ! for "login" $ "Login"
      input ! type_ "text" ! name "login" ! A.id "login" ! tabindex "1" ! accesskey "L"
      H.label ! for "password" $ "Password"
      input ! type_ "text" ! name "password" ! A.id "password" ! tabindex "2" ! accesskey "P"
      input ! type_  "submit" ! tabindex "3" ! accesskey "S" ! value "Enter Nomic!"



nomicServer :: ServerHandle -> ServerPart Response
nomicServer sh = do
   mpc <- getData
   case mpc of
      Just (PlayerCommand pn c) -> case c of
         Just (query, game, actionsNumber, actionResult) -> case query of
            Noop ->            nomicPageComm pn sh (return ())
            JoinGame ->        nomicPageComm pn sh (joinGame game pn)
            LeaveGame ->       nomicPageComm pn sh (leaveGame pn)
            SubscribeGame ->   nomicPageComm pn sh (subscribeGame game pn)
            UnsubscribeGame -> nomicPageComm pn sh (unsubscribeGame game pn)
            Amend ->           nomicPageComm pn sh (amendConstitution pn)
            DoAction ->        nomicPageComm pn sh (doAction actionsNumber actionResult pn)
         Nothing -> error "program error"
      Nothing -> error "Read error"


nomicPageComm :: PlayerNumber -> ServerHandle -> Comm () -> ServerPart Response
nomicPageComm pn sh comm = do
   inc <- lift $ atomically newTChan
   outc <- lift $ atomically newTChan
   let communication = (Communication inc outc sh)
   lift $ runWithComm communication comm
   pendingsActions <- lift $ runWithComm communication $ getPendingActions pn
   mess <- lift $ atomically $ whileM (isEmptyTChan outc >>= (return . not)) (readTChan outc)
   nomicPageServer pn mess pendingsActions


newRule :: ServerHandle -> ServerPart Response
newRule sh = do
   methodM POST -- only accept a post method
   mbEntry <- getData -- get the data
   case mbEntry of
      Nothing -> error $ "error: postLogin"
      Just (NewRule name text code pn)  -> do
         nomicPageComm pn sh (submitRule name text code pn)

nomicPageServer :: PlayerNumber -> [String] -> [Action] -> ServerPart Response
nomicPageServer pn mess actions = do
   multi <- query GetMulti
   ok $ toResponse $ nomicPage multi pn mess actions


postLogin :: ServerPart Response
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
         --Just pn -> seeOther ("/Nomic?pn=" ++ (show pn) ++ "&name=" ++ login ++ "&password=" ++ password ++ "&inGame=no") $ toResponse ("Redirecting..."::String)
         Just pn -> seeOther ("/Nomic?pn=" ++ (show pn) ++ "&query=Noop&game=none&actionNumber=null&actionResult=null") $ toResponse ("Redirecting..."::String)
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


instance FromData PlayerCommand where
  fromData = do
    pn <- lookRead "pn" `mplus` (error "need Player Number")
    q <- lookRead "query" `mplus` (error "need query")
    g <- look "game" `mplus` (error "need game")
    actionNumber <- look "actionNumber" `mplus` (error "need actionNumber")
    actionResult <- look "actionResult" `mplus` (error "need actionResult")
    return $ PlayerCommand pn $ Just (q, g, actionNumber, actionResult)


launchWebServer :: ServerHandle -> IO ()
launchWebServer sh = do
   putStrLn "Starting web server...\nTo connect, drive your browser to \"http://localhost:8000/Login\""
   d <- liftIO getDataDir
   simpleHTTP nullConf $ mconcat [dir "postLogin" $ postLogin,
                                  fileServe [] d,
                                  dir "Nomic" $ nomicServer sh,
                                  dir "NewRule" $ newRule sh,
                                  ok $ toResponse $ loginPage]

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


-- this tells happstack how to turn post data into a datatype using 'withData'
instance FromData NewRule where
  fromData = do
    name  <- look "name" `mplus` (error "need rule name")
    text <- look "text" `mplus` (error "need rule text")
    code <- look "code" `mplus` (error "need rule code")
    pn <- lookRead "pn" `mplus` (error "need player number")
    return $ NewRule name text code pn
