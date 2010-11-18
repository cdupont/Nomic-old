
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
    H.html $ do
      H.head $ do
        H.title (H.string title)
        meta ! http_equiv "Content-Type" ! content "text/html;charset=utf-8"
        sequence_ headers
      H.body $ do
        body

helloNomic :: ServerPart Response
helloNomic = do
   m <- query GetMulti
   ok $ toResponse $
    appTemplate "Hello, Nomic!"
                [H.meta ! A.name "keywords" ! A.content "happstack, blaze, html"]
                (viewMulti m)

launchWebServer :: IO ()
launchWebServer = simpleHTTP nullConf $ helloNomic

instance ToMessage H.Html where
    toContentType _ = B.pack "text/html; charset=UTF-8"
    toMessage = LU.fromString . renderHtml


