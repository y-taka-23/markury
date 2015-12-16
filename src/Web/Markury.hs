{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Markury where

import Web.Markury.Action.BookmarkAction
import Web.Markury.Action.TagAction
import Web.Markury.Action.UserAction
import Web.Markury.Model.DB
import Web.Markury.Model.Input
import Web.Markury.View

import Control.Monad.Logger ( runNoLoggingT )
import Database.Persist.Sql ( runSqlPool, runMigration )
import Database.Persist.Sqlite ( createSqlitePool )
import Web.Spock.Safe

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
