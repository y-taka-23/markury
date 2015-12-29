{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Web.Markury where

import Web.Markury.Action.BookmarkAction
import Web.Markury.Action.LoginAction
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
        getpost "login" $ loginAction
        get "logout" $ logoutAction
        subcomponent "bookmarks" $ do
            get root allBookmarksAction
            get ("view" <//> var) viewBookmarkAction
            getpost "add" $ do
                checkSession
                addBookmarkAction
            getpost ("delete" <//> var) $ \id -> do
                checkSession
                deleteBookmarkAction id
        subcomponent "users" $ do
            get root allUsersAction
            get ("view" <//> var) viewUserAction
            getpost "add" $ do
--                checkSession
                addUserAction
            getpost ("delete" <//> var) $ \id -> do
                checkSession
                deleteUserAction id
        subcomponent "tags" $ do
            get root allTagsAction
            get ("view" <//> var) viewTagAction
            getpost "add" $ do
                checkSession
                addTagAction
            getpost ("delete" <//> var) $ \id -> do
                checkSession
                deleteTagAction id
