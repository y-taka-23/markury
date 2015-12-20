{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Markury.Action.TagAction where

import Web.Markury.Action.Util
import Web.Markury.Model.DB
import Web.Markury.Model.Input
import Web.Markury.View.TagView

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

allTagsAction :: ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
allTagsAction = do
    allTags <- runSql $ P.selectList [] [P.Asc TagCreated]
    renderBlaze $ tagListView (map P.entityVal allTags)

viewTagAction :: P.BackendKey SqlBackend -> ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
viewTagAction id = do
    mTag <- runSql $ P.get $ TagKey id
    case mTag of
        Just tag -> do
            bookmarkTags <- runSql $ P.selectList [BookmarkTagTagId P.==. (TagKey id)] []
            let bookmarkIds = map (bookmarkTagBookmarkId . P.entityVal) bookmarkTags
            bookmarks <- runSql $ P.selectList [BookmarkId P.<-. bookmarkIds] [P.Asc BookmarkCreated]
            renderBlaze $ tagView (unSqlBackendKey id) tag (map P.entityVal bookmarks)
        Nothing -> redirect "/tags"

addTagAction :: ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
addTagAction = do
    f <- runForm "addTag" tagAddForm
    case f of
        (view, Nothing) -> do
            renderBlaze $ tagAddView view "/tags/add"
        (_, Just tagInput) -> do
            let title = tagInputTitle tagInput
            now <- liftIO getCurrentTime
            _ <- runSql $ P.insert $ Tag title now now
            redirect "/tags"

deleteTagAction :: P.BackendKey SqlBackend -> ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
deleteTagAction id = do
    let tagKey = TagKey id
    _ <- runSql $ P.delete tagKey
    _ <- runSql $ P.deleteWhere [BookmarkTagTagId P.==. tagKey]
    redirect "/tags"
