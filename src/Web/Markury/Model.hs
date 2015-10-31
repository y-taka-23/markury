{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web.Markury.Model where

import Database.Persist.TH
import Data.Text ( Text )
import Data.Time ( UTCTime )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Bookmark
    userId UserId
    title Text
    description Text Maybe
    url Text
    created UTCTime
    modified UTCTime
User
    email Text
    password Text
    created UTCTime
    modified UTCTime
Tag
    title Text
    created UTCTime
    modified UTCTime
BookmarkTag
    bookmarkId BookmarkId
    tagId TagId
|]
