{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Markury where

import Web.Markury.View
import Web.Markury.Model

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
import Data.Time ( getCurrentTime )

runMarkury :: IO ()
runMarkury = do
    pool <- runNoLoggingT $ createSqlitePool "markury.db" 5
    runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
    runSpock 8080 $ spock (defaultSpockCfg Nothing (PCPool pool) Nothing) $ do
        get "/bookmarks" $ do
            allBookmarks <- runSql $ P.selectList [] [P.Asc BookmarkCreated]
            renderSite $ bookmarkListView (map P.entityVal allBookmarks) 1 1
        get ("/bookmarks/view" <//> var ) $ \id -> do
            mBookmark <- runSql $ P.get $ BookmarkKey id
            case mBookmark of
                Just bookmark -> renderSite $ bookmarkView (unSqlBackendKey id) bookmark
                Nothing -> redirect "/bookmarks"
        getpost "/bookmarks/add" $ do
            now <- liftIO getCurrentTime
            f <- runForm "addBookmark" $ bookmarkAddForm now now
            case f of
                (view, Nothing) -> do
                    renderSite $ bookmarkAddView view "/bookmarks/add"
                (_, Just newBookmark) -> do
                    bookmarkId <- runSql $ P.insert newBookmark
                    tags <- runSql $ P.selectList [TagTitle P.<-. []] []
                    forM_ (map P.entityKey tags) $ \tagId -> do
                        _ <- runSql $ P.insert $ BookmarkTag bookmarkId tagId
                        return ()
                    redirect "/bookmarks"
        get "/users" $ do
            allUsers <- runSql $ P.selectList [] [P.Asc UserCreated]
            renderSite $ userListView (map P.entityVal allUsers) 1 1
        get ("/users/view" <//> var ) $ \id -> do
            mUser <- runSql $ P.get $ UserKey id
            case mUser of
                Just user -> renderSite $ userView (unSqlBackendKey id) user
                Nothing -> redirect "/users"
        getpost "/users/add" $ do
            now <- liftIO getCurrentTime
            f <- runForm "addUser" $ userAddForm now now
            case f of
                (view, Nothing) -> do
                    renderSite $ userAddView view "/users/add"
                (_, Just newUser) -> do
                    _ <- runSql $ P.insert newUser
                    redirect "/users"
        get "/tags" $ do
            allTags <- runSql $ P.selectList [] [P.Asc TagCreated]
            renderSite $ tagListView (map P.entityVal allTags) 1 1
        get ("/tags/view" <//> var ) $ \id -> do
            mTag <- runSql $ P.get $ TagKey id
            case mTag of
                Just tag -> renderSite $ tagView (unSqlBackendKey id) tag
                Nothing -> redirect "/tags"
        getpost "/tags/add" $ do
            now <- liftIO getCurrentTime
            f <- runForm "addTag" $ tagAddForm now now
            case f of
                (view, Nothing) -> do
                    renderSite $ tagAddView view "/tags/add"
                (_, Just newTag) -> do
                    _ <- runSql $ P.insert newTag
                    redirect "/tags"

runSql :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSql action = runQuery $ \conn -> runResourceT $ runNoLoggingT $ runSqlConn action conn

renderSite :: MonadIO m => Html -> ActionT m a
renderSite = lazyBytes . renderHtml . siteView
