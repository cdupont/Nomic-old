
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Web where

import Prelude hiding (div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (dir)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as LU (toString, fromString)

import Game
import Text.Blaze.Renderer.Pretty (renderHtml)
import Multi
import Happstack.Server
import Happstack.State
import NamedRule
import Control.Monad
import Action
import Paths_Nomic
import Control.Monad.State
import Safe
import Data.Monoid
import Observable
import System.IO.Unsafe
import Interpret

type SessionNumber = Integer

data LoginPass = LoginPass { login :: PlayerName,
                             password :: PlayerPassword}

-- | associate a player number with a handle
data PlayerClient = PlayerClient { cPlayerNumber :: PlayerNumber}
                                   deriving (Eq, Show)

-- | A structure to hold the active games and players
data Server = Server { playerClients :: [PlayerClient]}
                       deriving (Eq, Show)


-- | A State to pass around active games and players.
-- Furthermore, the output are to be made with Comm to output to the right console.
type ServerState = StateT Server IO ()

viewGame :: Game -> Html
viewGame g = do
   div ! A.id "gameName" $ h5 $ string $ "You are viewing game:" ++ gameName g
   div ! A.id "citizens" $ viewPlayers $ players g
   div ! A.id "actionsresults" $ viewActions $ actionResults g
   div ! A.id "rules" $ viewRules g

viewActions :: [Action] -> Html
viewActions as = do
   h5 $ "Completed Actions"
   table $ do
      forM_ as viewAction

viewAction :: Action -> Html
viewAction a = do
   showHtml a


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


viewPlayers :: [PlayerInfo] -> Html
viewPlayers pis = do
   table $ do
      caption $ h5 "Players in game:"
      mapM_ viewPlayer pis


viewPlayer :: PlayerInfo -> Html
viewPlayer pi = tr $ td $ showHtml pi


viewMulti :: PlayerNumber -> Multi -> Html
viewMulti pn m = do
   div ! A.id "gameList"  $ do
      viewGameNames (games m)
   div ! A.id "game" $ do
      case getPlayersGame pn m of
         Just g -> viewGame g
         Nothing -> h5 "Not in game"


viewGameNames :: [Game] -> Html
viewGameNames gs = do
   h5 "Games:"
   table $ do

      case gs of
         [] -> tr $ td $ "No Games"
         _ -> mapM_ viewGameName gs

viewGameName :: Game -> Html
viewGameName g = do
   tr $ td $ string $ gameName g

nomicPage :: Multi -> PlayerNumber -> Html
nomicPage multi pn =
    H.html $ do
      H.head $ do
        H.title (H.string "Welcome to Nomic!")
        H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomic.css"
        H.meta ! http_equiv "Content-Type" ! content "text/html;charset=utf-8"
        H.meta ! A.name "keywords" ! A.content "Nomic, game, rules, Haskell, auto-reference"
      H.body $ do
        H.div ! A.id "container" $ do
           H.div ! A.id "header" $ string $ "Welcome to Nomic, " ++ (getPlayersName pn multi) ++ "!"
           H.div ! A.id "multi" $ viewMulti pn multi
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



nomicServer :: ServerPart Response
nomicServer = do
   multi <- query GetMulti
   mpc <- getData
   case mpc of
      Just (PlayerClient pn) -> ok $ toResponse $ nomicPage multi pn
      Nothing -> error "Read error"


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
         Just pn -> seeOther ("/Nomic?pn=" ++ (show pn)) $ toResponse ("Redirecting..."::String)
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


instance FromData PlayerClient where
  fromData = do
    pn <- lookRead "pn" `mplus` (error "need Player Number")
    --game <- look "game"
    return $ PlayerClient pn


launchWebServer :: ServerHandle -> IO ()
launchWebServer sh = do
   putStrLn "Starting web server...\nTo connect, drive your browser to \"http://localhost:8000/Nomic\""
   d <- liftIO getDataDir
   simpleHTTP nullConf $ mconcat [dir "postLogin" $ postLogin,
                                  fileServe [] d,
                                  dir "Nomic" $ nomicServer sh,
                                  dir "NewRule" $ newRule sh,
                                  dir "Login" $ ok $ toResponse $ loginPage]

instance ToMessage H.Html where
    toContentType _ = B.pack "text/html; charset=UTF-8"
    toMessage = LU.fromString . renderHtml

-- this tells happstack how to turn post data into a datatype using 'withData'
instance FromData LoginPass where
  fromData = do
    login  <- look "login" `mplus` (error "need login")
    password <- look "password" `mplus` (error "need password")
    return $ LoginPass login password
