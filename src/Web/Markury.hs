{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Markury where

import Web.Markury.Action.BookmarkAction
import Web.Markury.Action.TagAction
import Web.Markury.Action.UserAction
import Web.Markury.Model.DB
import Web.Markury.Model.Input

import Control.Monad.Logger ( runNoLoggingT )
import Database.Persist.Sql ( runSqlPool, runMigration )
import Database.Persist.Sqlite ( createSqlitePool )
import Web.Spock.Safe

runMarkury :: IO ()
runMarkury = do
    pool <- runNoLoggingT $ createSqlitePool "markury.db" 5
    runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
    runSpock 8080 $ spock (defaultSpockCfg Nothing (PCPool pool) Nothing) $ do
        subcomponent "bookmarks" $ do
            get root allBookmarksAction
            get ("view" <//> var) viewBookmarkAction
            getpost "add" addBookmarkAction
        subcomponent "users" $ do
            get root allUsersAction
            get ("view" <//> var) viewUserAction
            getpost "add" addUserAction
            getpost ("delete" <//> var) deleteUserAction
        subcomponent "tags" $ do
            get root allTagsAction
            get ("view" <//> var) viewTagAction
            getpost "add" addTagAction
            getpost ("delete" <//> var) deleteTagAction
