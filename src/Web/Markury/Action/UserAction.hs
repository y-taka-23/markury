{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Markury.Action.UserAction where

import Web.Markury.Action.Util
import Web.Markury.Model.DB
import Web.Markury.Model.Input
import Web.Markury.View.UserView

import Web.Spock.Digestive ( runForm )
import Web.Spock.Safe
import Text.Blaze.Html ( Html )
import Text.Blaze.Html.Renderer.Utf8 ( renderHtml )
import Control.Monad ( forM_ )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Logger ( NoLoggingT, runNoLoggingT )
import Control.Monad.Trans.Resource ( ResourceT, runResourceT )
import qualified Database.Persist as P
import Database.Persist.Sql ( SqlBackend, SqlPersistT, runSqlPool, runMigration, runSqlConn, unSqlBackendKey )
import Database.Persist.Sqlite ( createSqlitePool )
import qualified Data.Text as T
import Data.Time ( getCurrentTime )

allUsersAction :: ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
allUsersAction = do
    allUsers <- runSql $ P.selectList [] [P.Asc UserCreated]
    renderBlaze $ userListView (map P.entityVal allUsers)

viewUserAction :: P.BackendKey SqlBackend -> ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
viewUserAction id = do
    mUser <- runSql $ P.get $ UserKey id
    case mUser of
        Just user -> renderBlaze $ userView (unSqlBackendKey id) user
        Nothing -> redirect "/users"

addUserAction :: ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
addUserAction = do
    f <- runForm "addUser" userAddForm
    case f of
        (view, Nothing) -> do
            renderBlaze $ userAddView view "/users/add"
        (_, Just userInput) -> do
            let email = userInputEmail userInput
            let password = userInputEmail userInput
            now <- liftIO getCurrentTime
            _ <- runSql $ P.insert $ User email password now now
            redirect "/users"

deleteUserAction :: P.BackendKey SqlBackend -> ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
deleteUserAction id = do
    _ <- runSql $ P.delete $ UserKey id
    redirect "/users"
