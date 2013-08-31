{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, OverloadedStrings, NamedFieldPuns#-}


module Web.Settings where

import Text.Blaze.Html5 hiding (map, label, br)

import Prelude hiding (div)
import Text.Reform
import Text.Reform.Blaze.String as RB hiding (form)
import Text.Reform.Happstack()
import Text.Blaze.Html5.Attributes as A hiding (dir, label)
import qualified Text.Blaze.Html5 as H
import Control.Applicative
import Types
import Happstack.Server
import Text.Reform.Happstack
import Web.Common
import Control.Monad.State
import Web.Routes.RouteT
import Control.Concurrent.STM
import Data.Maybe
import Data.Text(Text)
import Multi as M
import Utils
import Web.Help as Help
import Language.Nomyx
import qualified Language.Haskell.HsColour.HTML as HSC
import Language.Haskell.HsColour.Colourise hiding (string)
import Text.Blaze.Internal hiding (Text)
default (Integer, Double, Data.Text.Text)

playerSettingsForm :: (Maybe PlayerSettings) -> [PlayerName] -> [String] -> NomyxForm PlayerSettings
playerSettingsForm (Just prof) ns emails = playerSettingsForm' (_pPlayerName prof) (_mail prof) (_mailNewRule prof) ns emails
playerSettingsForm Nothing ns emails = playerSettingsForm' "" "" True ns emails

playerSettingsForm':: String -> String -> Bool -> [PlayerName] -> [String] -> NomyxForm PlayerSettings
playerSettingsForm' name mailTo mailNewRule names emails = pure Types.PlayerSettings
   <*> errorList ++> label "Player Name: " ++> (RB.inputText name) `transformEither` (uniqueName names) `transformEither` (fieldRequired PlayerNameRequired) <++ br
   <*> errorList ++> label "Please enter your mail: " ++> (RB.inputText mailTo) `transformEither` (uniqueEmail emails) <++ br
   <*> pure True
   <*> RB.inputCheckbox mailNewRule <++ label " I want to be notified by email when a player proposes a new rule in my game (recommended)" <++ br
   <*> pure True
   <*> pure True

readPlayAs :: NomyxForm Bool -> NomyxForm String -> NomyxForm (Maybe PlayerNumber)
readPlayAs = liftA2 f where
   f b s = if b then (Just $ read s) else Nothing

uniqueName :: [String] -> String -> Either NomyxError String
uniqueName names name = case name `elem` names of
   True  -> Left UniqueName
   False -> Right name

uniqueEmail :: [String] -> String -> Either NomyxError String
uniqueEmail names name = case name `elem` names of
   True  -> Left UniqueEmail
   False -> Right name

settingsPage :: PlayerSettings -> [PlayerName] -> [String] -> RoutedNomyxServer Html
settingsPage ps names emails = do
   settingsLink <- showURL SubmitPlayerSettings
   mf <- lift $ viewForm "user" $ playerSettingsForm (Just ps) names emails
   mainPage  "Player settings"
             "Player settings"
             (blazeForm mf settingsLink)
             False
             True

playerSettings :: (TVar Session) -> RoutedNomyxServer Html
playerSettings ts  = do
   pn <- getPlayerNumber ts
   pfd <- getProfile' ts pn
   names <- liftIO $ forbiddenNames ts pn
   emails <- liftIO $ forbiddenEmails ts pn
   settingsPage (_pPlayerSettings $ fromJust pfd) names emails

newPlayerSettings :: (TVar Session) -> RoutedNomyxServer Html
newPlayerSettings ts = do
   methodM POST
   pn <- getPlayerNumber ts
   names <- liftIO $ forbiddenNames ts pn
   emails <- liftIO $ forbiddenEmails ts pn
   p <- liftRouteT $ eitherForm environment "user" $ playerSettingsForm Nothing names emails
   case p of
      Right ps -> do
         webCommand ts $ M.playerSettings ps pn
         link <- showURL MainPage
         seeOther link $ string "Redirecting..."
      (Left errorForm) -> do
         settingsLink <- showURL SubmitPlayerSettings
         mainPage  "Player settings" "Player settings" (blazeForm errorForm settingsLink) False True

forbiddenNames :: (TVar Session) -> PlayerNumber -> IO [PlayerName]
forbiddenNames ts pn = liftIO $ do
   session <- atomically $ readTVar ts
   pfs <- getAllProfiles session
   let filteredPfs = filter ((/= pn) . _pPlayerNumber) pfs
   return $ (_pPlayerName . _pPlayerSettings) <$> filteredPfs

forbiddenEmails :: (TVar Session) -> PlayerNumber -> IO [PlayerName]
forbiddenEmails ts pn = liftIO $ do
   session <- atomically $ readTVar ts
   pfs <- getAllProfiles session
   let filteredPfs = filter ((/= pn) . _pPlayerNumber) pfs
   return $ filter (not . null) $ (_mail . _pPlayerSettings) <$> filteredPfs


advanced :: (TVar Session) -> RoutedNomyxServer Html
advanced ts = do
   pn <- getPlayerNumber ts
   pfd <- getProfile' ts pn
   session <- liftIO $ atomically $ readTVar ts
   page <- advancedPage (_pLastUpload $ fromJust pfd) (_pAdmin $ fromJust pfd) (_mSettings $ _multi session)
   mainPage "Advanced" "Advanced" page False True


advancedPage :: LastUpload -> Admin -> Settings -> RoutedNomyxServer Html
advancedPage mlu (Admin admin (PlayAs mpn)) settings = do
   uploadLink <- showURL Upload
   submitAdminPass <- showURL SubmitAdminPass
   submitPlayAs <- showURL SubmitPlayAs
   submitSettings <- showURL SubmitSettings
   up  <- lift $ viewForm "user" uploadForm  --TODO add the file name (missing Reform feature)
   ap <- lift $ viewForm "user" $ adminPassForm
   paf <- lift $ viewForm "user" $ playAsForm []
   set <- lift $ viewForm "user" $ settingsForm (_sendMails settings)
   ok $ do
      p $ do
         pre $ string Help.getSaveFile
         H.a "get save file" ! (href $ "/Nomyx.save")
      H.br
      hr
      p $ do
         pre $ string Help.upload
         preEscapedString $ HSC.hscolour defaultColourPrefs False $ Help.uploadExample
         "Upload new rules file:" >> H.br
         blazeForm up (uploadLink)
         case mlu of
            UploadFailure (_, error) -> do
               h5 $ "Error in submitted file: "
               pre $ string $ error
            UploadSuccess -> h5 $ "File uploaded successfully!"
            NoUpload -> p ""
      hr
      p $ do
         h5 "Enter admin password to get admin rights:"
         blazeForm ap (submitAdminPass)
         when admin $ h5 "You are admin"
      when admin $ do
         hr
         p $ do
            h5 "Enter the number of the player you want to play for:"
            blazeForm paf submitPlayAs
            when (isJust mpn) $ h5 $ string $ "Playing as player " ++ (show $ fromJust mpn)
         p $ do
            h5 "Send mails:"
            blazeForm set submitSettings
            h5 $ string $ if (_sendMails settings) then "mails will be sent " else "mails will NOT be sent "



adminPassForm :: NomyxForm String
adminPassForm = RB.inputText ""

playAsForm :: [PlayerNumber] -> NomyxForm (Maybe PlayerNumber)
playAsForm _ = readPlayAs (label "Play as: " ++> RB.inputCheckbox False) (RB.inputText "")

newPlayAsSettings :: (TVar Session) -> RoutedNomyxServer Html
newPlayAsSettings ts = do
   methodM POST
   p <- liftRouteT $ eitherForm environment "user" $ playAsForm []
   pn <- getPlayerNumber ts
   case p of
      Right ps -> do
         webCommand ts $ playAsSetting ps pn
         link <- showURL Advanced
         seeOther link $ string "Redirecting..."
      (Left errorForm) -> do
         settingsLink <- showURL SubmitPlayAs
         mainPage  "Admin settings" "Admin settings" (blazeForm errorForm settingsLink) False True


settingsForm :: Bool -> NomyxForm Bool
settingsForm sendMails = label "Send mails: " ++> RB.inputCheckbox sendMails

newSettings :: (TVar Session) -> RoutedNomyxServer Html
newSettings ts = do
   methodM POST
   p <- liftRouteT $ eitherForm environment "user" $ settingsForm False
   case p of
      Right ps -> do
         webCommand ts $ globalSettings ps
         link <- showURL Advanced
         seeOther link $ string "Redirecting..."
      (Left errorForm) -> do
         settingsLink <- showURL SubmitSettings
         mainPage  "Admin settings" "Admin settings" (blazeForm errorForm settingsLink) False True


uploadForm :: NomyxForm (FilePath, FilePath, ContentType)
uploadForm = RB.inputFile

newUpload :: (TVar Session) -> RoutedNomyxServer Html
newUpload ts = do
    methodM POST
    pn <- getPlayerNumber ts
    r <- liftRouteT $ eitherForm environment "user" uploadForm
    link <- showURL Advanced
    (Types.Session sh _ _) <- liftIO $ readTVarIO ts
    case r of
       (Right (temp,name,_)) -> webCommand ts $ M.inputUpload pn temp name sh
       (Left _) -> liftIO $ putStrLn $ "cannot retrieve form data"
    seeOther link $ string "Redirecting..."

newAdminPass :: (TVar Session) -> RoutedNomyxServer Html
newAdminPass ts = do
   methodM POST
   p <- liftRouteT $ eitherForm environment "user" $ adminPassForm
   pn <- getPlayerNumber ts
   case p of
      Right ps -> do
         webCommand ts $ adminPass ps pn
         link <- showURL Advanced
         seeOther link $ string "Redirecting..."
      (Left errorForm) -> do
         settingsLink <- showURL SubmitAdminPass
         mainPage  "Admin settings" "Admin settings" (blazeForm errorForm settingsLink) False True
