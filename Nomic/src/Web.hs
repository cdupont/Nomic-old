
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}

module Web where

import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as LU (toString, fromString)

import Game
import Text.Blaze.Renderer.String (renderHtml)
import Multi
import Happstack.Server
import Happstack.State (query)



viewGame :: Game -> Html
viewGame Game { gameName      = name,
                rules         = rs,
                actionResults = ars,
                players       = ps} = do
   p $ showHtml name



viewMulti :: Multi -> Html
viewMulti Multi { games = g,
                  mPlayers = mps} = do
                  sequence_ $ map viewGame g


appTemplate :: String -> [Html] -> Html -> Html
appTemplate title headers body =
htmlTemplate :: Html -> String -> Html
htmlTemplate body dataDir  =
    H.html $ do
      H.head $ do
        H.title (H.string "Welcome to Nomic!")
        H.link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/nomic.css"
        H.meta ! http_equiv "Content-Type" ! content "text/html;charset=utf-8"
        H.meta ! A.name "keywords" ! A.content "Nomic, game, rules, Haskell, auto-reference"
      H.body $ do
        h2 "testo"
        body


webServer :: ServerPart Response
webServer = do
   m <- query GetMulti
   d <- liftIO getDataDir
   --lift $ putStrLn $ "data dir is:" ++ d
   ok $ toResponse $ htmlTemplate (viewMulti m) d

launchWebServer :: IO ()
launchWebServer = do
   putStrLn "Starting web server...\nTo connect, drive your browser to \"http://localhost:8000/Nomic\""
   d <- liftIO getDataDir
   simpleHTTP nullConf $ mconcat [fileServe [] d,
                                  dir "Nomic" webServer]

instance ToMessage H.Html where
    toContentType _ = B.pack "text/html; charset=UTF-8"
    toMessage = LU.fromString . renderHtml


