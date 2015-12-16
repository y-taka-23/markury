{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Markury.Action.BookmarkAction where

import Web.Markury.Action.Util
import Web.Markury.Model.DB
import Web.Markury.Model.Input
import Web.Markury.View

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

allBookmarksAction :: ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
allBookmarksAction = do
    allBookmarks <- runSql $ P.selectList [] [P.Asc BookmarkCreated]
    renderSite $ bookmarkListView (map P.entityVal allBookmarks)

addBookmarkAction :: ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
addBookmarkAction = do
    f <- runForm "addBookmark" bookmarkAddForm
    case f of
        (view, Nothing) -> do
            renderSite $ bookmarkAddView view "/bookmarks/add"
        (_, Just bookmarkInput) -> do
            let title = bookmarkInputTitle bookmarkInput
            let desc = bookmarkInputDescription bookmarkInput
            let url = bookmarkInputUrl bookmarkInput
            let concatTags = bookmarkInputTags bookmarkInput
            now <- liftIO getCurrentTime
            bookmarkId <- runSql $ P.insert $ Bookmark title desc url now now
            forM_ (T.words concatTags) $ \tagTitle -> do
                tagId <- runSql $ P.insert $ Tag tagTitle now now
                _ <- runSql $ P.insert $ BookmarkTag bookmarkId tagId
                return ()
            redirect "/bookmarks"

viewBookmarkAction :: P.BackendKey SqlBackend -> ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
viewBookmarkAction id = do
    mBookmark <- runSql $ P.get $ BookmarkKey id
    case mBookmark of
        Just bookmark -> do
            bookmarkTags <- runSql $ P.selectList [BookmarkTagBookmarkId P.==. (BookmarkKey id)] []
            let tagIds = map (bookmarkTagTagId . P.entityVal) bookmarkTags
            tags <- runSql $ P.selectList [TagId P.<-. tagIds] [P.Asc TagCreated]
            renderSite $ bookmarkView (unSqlBackendKey id) bookmark (map P.entityVal tags)
        Nothing -> redirect "/bookmarks"
