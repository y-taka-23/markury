{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web.Markury.Model.DB where

import Database.Persist.TH
import Data.Text ( Text )
import Data.Time ( UTCTime )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Bookmark
    title Text
    description Text
    url Text
    created UTCTime
    modified UTCTime
User
    email Text
    password Text
    created UTCTime
    modified UTCTime
    UniqueEmail email
Tag
    title Text
    created UTCTime
    modified UTCTime
BookmarkTag
    bookmarkId BookmarkId
    tagId TagId
    UniqueBookmarkTag bookmarkId tagId
Session
    spockSessionId Text
    userEmail Text
    UniqueSpockSessionId spockSessionId
|]
