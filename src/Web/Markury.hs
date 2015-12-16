{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Markury where

import Web.Markury.Action.BookmarkAction
import Web.Markury.Action.TagAction
import Web.Markury.Action.UserAction
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

runMarkury :: IO ()
runMarkury = do
    pool <- runNoLoggingT $ createSqlitePool "markury.db" 5
    runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
    runSpock 8080 $ spock (defaultSpockCfg Nothing (PCPool pool) Nothing) $ do
        get "/bookmarks" allBookmarksAction
        get ("/bookmarks/view" <//> var) viewBookmarkAction
        getpost "/bookmarks/add" addBookmarkAction
        get "/users" allUsersAction
        get ("/users/view" <//> var) viewUserAction
        getpost "/users/add" addUserAction
        get "/tags" allTagsAction
        get ("/tags/view" <//> var) viewTagAction
        getpost "/tags/add" addTagAction
