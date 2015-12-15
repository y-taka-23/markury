{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Markury where

import Web.Markury.View
import Web.Markury.Model.DB
import Web.Markury.Model.Input

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

runMarkury :: IO ()
runMarkury = do
    pool <- runNoLoggingT $ createSqlitePool "markury.db" 5
    runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
    runSpock 8080 $ spock (defaultSpockCfg Nothing (PCPool pool) Nothing) $ do
        get "/bookmarks" allBookmarksAction
        get ("/bookmarks/view" <//> var) viewBookmarkAction
        getpost "/bookmarks/add" addBookmarkAction
        get "/users" allUsersAction
        get ("/users/view" <//> var ) viewUserAction
        getpost "/users/add" addUserAction
        get "/tags" allTagsAction
        get ("/tags/view" <//> var ) viewTagAction
        getpost "/tags/add" addTagAction

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

allUsersAction :: ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
allUsersAction = do
    allUsers <- runSql $ P.selectList [] [P.Asc UserCreated]
    renderSite $ userListView (map P.entityVal allUsers)

viewUserAction :: P.BackendKey SqlBackend -> ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
viewUserAction id = do
    mUser <- runSql $ P.get $ UserKey id
    case mUser of
        Just user -> renderSite $ userView (unSqlBackendKey id) user
        Nothing -> redirect "/users"

addUserAction :: ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
addUserAction = do
    f <- runForm "addUser" userAddForm
    case f of
        (view, Nothing) -> do
            renderSite $ userAddView view "/users/add"
        (_, Just userInput) -> do
            let email = userInputEmail userInput
            let password = userInputEmail userInput
            now <- liftIO getCurrentTime
            _ <- runSql $ P.insert $ User email password now now
            redirect "/users"

allTagsAction :: ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
allTagsAction = do
    allTags <- runSql $ P.selectList [] [P.Asc TagCreated]
    renderSite $ tagListView (map P.entityVal allTags)

viewTagAction :: P.BackendKey SqlBackend -> ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
viewTagAction id = do
    mTag <- runSql $ P.get $ TagKey id
    case mTag of
        Just tag -> renderSite $ tagView (unSqlBackendKey id) tag
        Nothing -> redirect "/tags"

addTagAction :: ActionT (WebStateM SqlBackend (Maybe a) (Maybe b)) c
addTagAction = do
    f <- runForm "addTag" tagAddForm
    case f of
        (view, Nothing) -> do
            renderSite $ tagAddView view "/tags/add"
        (_, Just tagInput) -> do
            let title = tagInputTitle tagInput
            now <- liftIO getCurrentTime
            _ <- runSql $ P.insert $ Tag title now now
            redirect "/tags"

runSql :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSql action = runQuery $ \conn -> runResourceT $ runNoLoggingT $ runSqlConn action conn

renderSite :: MonadIO m => Html -> ActionT m a
renderSite = lazyBytes . renderHtml . siteView
