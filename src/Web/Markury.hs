{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Markury where

import Web.Markury.View
import Web.Markury.Model

import Web.Spock.Digestive ( runForm )
import Web.Spock.Safe
import Text.Blaze.Html.Renderer.Utf8 ( renderHtml )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Logger ( NoLoggingT, runNoLoggingT )
import Control.Monad.Trans.Resource ( ResourceT, runResourceT )
import Database.Persist ( insert, selectList, SelectOpt(..), PersistValue(..) )
import Database.Persist.Sql ( SqlBackend, SqlPersistT, runSqlPool, runMigration, runSqlConn )
import Database.Persist.Sqlite ( createSqlitePool )
import Data.Time ( getCurrentTime )

runMarkury :: IO ()
runMarkury = do
    pool <- runNoLoggingT $ createSqlitePool "markury.db" 5
    runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
    runSpock 8080 $ spock (defaultSpockCfg Nothing (PCPool pool) Nothing) $ do
        get "/bookmarks" $ do
            allBookmarks <- runSql $ selectList [] [Asc BookmarkCreated]
            lazyBytes $ renderHtml $ bookmarksView allBookmarks
        get "/users" $ do
            allUsers <- runSql $ selectList [] [Asc UserCreated]
            lazyBytes $ renderHtml $ usersView allUsers
        getpost "/users/add" $ do
            now <- liftIO getCurrentTime
            f <- runForm "addUser" $ userForm now now
            case f of
                (view, Nothing) -> do
                    lazyBytes $ renderHtml $ userView view "/users/add"
                (_, Just newUser) -> do
                    _ <- runSql $ insert newUser
                    redirect "/users"
        get "/tags" $ do
            allTags <- runSql $ selectList [] [Asc TagCreated]
            lazyBytes $ renderHtml $ tagsView allTags
        getpost "/tags/add" $ do
            now <- liftIO getCurrentTime
            f <- runForm "addTag" $ tagForm now now
            case f of
                (view, Nothing) -> do
                    lazyBytes $ renderHtml $ tagView view "/tags/add"
                (_, Just newTag) -> do
                    _ <- runSql $ insert newTag
                    redirect "/tags"

runSql :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSql action = runQuery $ \conn -> runResourceT $ runNoLoggingT $ runSqlConn action conn
