{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Markury where

import Web.Markury.View
import Web.Markury.Model

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
    ct <- liftIO getCurrentTime
    pool <- runNoLoggingT $ createSqlitePool "markury.db" 5
    runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
    runSpock 8080 $ spock (defaultSpockCfg Nothing (PCPool pool) Nothing) $ do
        get "bookmarks" $ do
            allBookmarks <- runSql $ selectList [] [Asc BookmarkCreated]
            lazyBytes $ renderHtml $ bookmarksView allBookmarks
        get "users" $ do
            allUsers <- runSql $ selectList [] [Asc UserCreated]
            lazyBytes $ renderHtml $ usersView allUsers
        get "tags" $ do
            allTags <- runSql $ selectList [] [Asc TagCreated]
            lazyBytes $ renderHtml $ tagsView allTags

runSql :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSql action = runQuery $ \conn -> runResourceT $ runNoLoggingT $ runSqlConn action conn
