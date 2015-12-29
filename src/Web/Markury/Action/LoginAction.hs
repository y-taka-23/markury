{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Markury.Action.LoginAction where

import Web.Markury.Action.Util
import Web.Markury.Model.DB
import Web.Markury.Model.Input
import Web.Markury.View.LoginView

import Web.Spock.Digestive ( runForm )
import Web.Spock.Safe
import qualified Database.Persist as P
import Database.Persist.Sql ( SqlBackend )
import qualified Data.Text as T

loginAction :: ActionT (WebStateM SqlBackend sess st) b
loginAction = do
    f <- runForm "login" loginForm
    case f of
        (view, Nothing) -> do
            renderBlaze $ loginView view "/login"
        (_, Just loginInput) -> do
            let email = loginInputEmail loginInput
            let password = loginInputPassword loginInput
            mUser <- runSql $ P.getBy $ UniqueEmail email
            case mUser of
                Nothing -> redirect "/login"
                Just user -> do
                    if authPassword (userPassword $ P.entityVal user) password
                        then do
                            createSession email
                            redirect "/bookmarks"
                        else do
                            redirect "/login"

checkSession :: ActionT (WebStateM SqlBackend sess st) ()
checkSession = do
    sessionId <- getSessionId
    mSession <- runSql $ P.getBy $ UniqueSpockSessionId sessionId
    case mSession of
        Nothing -> redirect "/login"
        Just _ -> return ()

createSession :: T.Text -> ActionT (WebStateM SqlBackend sess st) ()
createSession email = do
    sessionId <- getSessionId
    _ <- runSql $ P.insert $ Session sessionId email
    return ()

authPassword :: T.Text -> T.Text -> Bool
authPassword = (==)
