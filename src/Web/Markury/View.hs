{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View where

import Web.Markury.Model

import Prelude hiding ( head, div )
import Text.Blaze.XHtml5 hiding ( Tag, text, label, form )
import Control.Monad
import Data.Time ( UTCTime )
import Data.Time.Format ( FormatTime, formatTime, defaultTimeLocale )
import Data.Text hiding ( head )
import Text.Digestive
import Text.Digestive.Blaze.Html5

bookmarkListView :: [Bookmark] -> Html
bookmarkListView bookmarks = docTypeHtml $ do
    head $ do
        title "Markury - Simple Bookmarker"
    body $ do
        nav $ do
            ul $ do
                li $ h1 "Bookmarks"
            section $ do
                ul $ do
                    li "Documentation"
                    li "API"
        section $ do
            nav $ do
                ul $ do
                    li "Bookmarks"
                    li "Users"
                    li "Tags"
            div $ do
                h3 "Bookmarks"
                table $ do
                    thead $ do
                        th "Title"
                        th "Created"
                        th "Modified"
                        th "Actions"
                    tbody $ forM_ bookmarks $ \bookmark -> do
                        tr $ do
                            td $ toHtml $ bookmarkTitle bookmark
                            td $ toHtml $ showTime $ bookmarkCreated bookmark
                            td $ toHtml $ showTime $ bookmarkModified bookmark
                div $ do
                    ul $ do
                        li "< previous"
                        li "next >"
                    p "1 of 1"
        footer $ ""

userListView :: [User] -> Html
userListView users = do
        nav $ do
            ul $ do
                li $ h1 "Users"
            section $ do
                ul $ do
                    li "Documentation"
                    li "API"
        section $ do
            nav $ do
                ul $ do
                    li "Bookmarks"
                    li "Users"
                    li "Tags"
            div $ do
                h3 "Users"
                table $ do
                    thead $ do
                        th "Email"
                        th "Password"
                        th "Created"
                        th "Modified"
                        th "Actions"
                    tbody $ forM_ users $ \user -> do
                        tr $ do
                            td $ toHtml $ userEmail user
                            td $ toHtml $ userPassword user
                            td $ toHtml $ showTime $ userCreated user
                            td $ toHtml $ showTime $ userModified user
                div $ do
                    ul $ do
                        li "< previous"
                        li "next >"
                    p "1 of 1"
        footer $ ""

tagListView :: [Tag] -> Html
tagListView tags = docTypeHtml $ do
    head $ do
        title "Markury - Simple Bookmarker"
    body $ do
        nav $ do
            ul $ do
                li $ h1 "Tags"
            section $ do
                ul $ do
                    li "Documentation"
                    li "API"
        section $ do
            nav $ do
                ul $ do
                    li "Bookmarks"
                    li "Users"
                    li "Tags"
            div $ do
                h3 "Tags"
                table $ do
                    thead $ do
                        th "Title"
                        th "Created"
                        th "Modified"
                        th "Actions"
                    tbody $ forM_ tags $ \tag -> do
                        tr $ do
                            td $ toHtml $ tagTitle tag
                            td $ toHtml $ showTime $ tagCreated tag
                            td $ toHtml $ showTime $ tagModified tag
                div $ do
                    ul $ do
                        li "< previous"
                        li "next >"
                    p "1 of 1"
        footer $ ""

bookmarkView :: Show i => i -> Bookmark -> Html
bookmarkView id bookmark = docTypeHtml $ do
    head $ do
        title "Markury - Simple Bookmarker"
    body $ do
        nav $ do
            ul $ do
                li $ h1 "Bookmarks"
            section $ do
                ul $ do
                    li "Documentation"
                    li "API"
        section $ do
            nav $ do
                ul $ do
                    li "Bookmarks"
                    li "Users"
                    li "Tags"
            div $ do
                h3 $ toHtml $ bookmarkTitle bookmark
                table $ do
                    tr $ do
                        th "Title"
                        td $ toHtml $ bookmarkTitle bookmark
                    tr $ do
                        th "Id"
                        td $ toHtml $ show id
                    tr $ do
                        th "Created"
                        td $ toHtml $ showTime $ bookmarkCreated bookmark
                    tr $ do
                        th "Modified"
                        td $ toHtml $ showTime $ bookmarkModified bookmark
                div $ do
                    h4 "Description"
                    p $ toHtml $ bookmarkDescription bookmark
                div $ do
                    h4 "Url"
                    p $ toHtml $ bookmarkUrl bookmark
        footer $ ""

userView :: Show i => i -> User -> Html
userView id user = docTypeHtml $ do
    head $ do
        title "Markury - Simple Bookmarker"
    body $ do
        nav $ do
            ul $ do
                li $ h1 "Users"
            section $ do
                ul $ do
                    li "Documentation"
                    li "API"
        section $ do
            nav $ do
                ul $ do
                    li "Bookmarks"
                    li "Users"
                    li "Tags"
            div $ do
                h3 $ toHtml $ show id
                table $ do
                    tr $ do
                        th "Email"
                        td $ toHtml $ userEmail user
                    tr $ do
                        th "Password"
                        td $ toHtml $ userPassword user
                    tr $ do
                        th "Id"
                        td $ toHtml $ show id
                    tr $ do
                        th "Created"
                        td $ toHtml $ showTime $ userCreated user
                    tr $ do
                        th "Modified"
                        td $ toHtml $ showTime $ userModified user
        footer $ ""

tagView :: Show i => i -> Tag -> Html
tagView id tag = docTypeHtml $ do
    head $ do
        title "Markury - Simple Bookmarker"
    body $ do
        nav $ do
            ul $ do
                li $ h1 "Tags"
            section $ do
                ul $ do
                    li "Documentation"
                    li "API"
        section $ do
            nav $ do
                ul $ do
                    li "Bookmarks"
                    li "Users"
                    li "Tags"
            div $ do
                h3 $ toHtml $ tagTitle tag
                table $ do
                    tr $ do
                        th "Title"
                        td $ toHtml $ tagTitle tag
                    tr $ do
                        th "Id"
                        td $ toHtml $ show id
                    tr $ do
                        th "Created"
                        td $ toHtml $ showTime $ tagCreated tag
                    tr $ do
                        th "Modified"
                        td $ toHtml $ showTime $ tagModified tag
        footer $ ""

bookmarkAddForm :: Monad m => UTCTime -> UTCTime -> Form Html m Bookmark
bookmarkAddForm created modified = Bookmark
    <$> "title" .: text Nothing
    <*> "description" .: text Nothing
    <*> "url" .: text Nothing
    <*> "created" .: stringRead "Couldn't parse as UTCTime" (Just created)
    <*> "modified" .: stringRead "Couldn't parse as UTCTime" (Just modified)

bookmarkAddView :: View Html -> Text -> Html
bookmarkAddView view path = form view path $ do
    label "title" view "Title: "
    inputText "title" view
    label "description" view "Description: "
    inputText "description" view
    label "url" view "URL: "
    inputText "url" view
    inputSubmit "Add"

userAddForm :: Monad m => UTCTime -> UTCTime -> Form Html m User
userAddForm created modified = User
    <$> "email" .: text Nothing
    <*> "password" .: text Nothing
    <*> "created" .: stringRead "Couldn't parse as UTCTime" (Just created)
    <*> "modified" .: stringRead "Couldn't parse as UTCTime" (Just modified)

userAddView :: View Html -> Text -> Html
userAddView view path = form view path $ do
    label "email" view "Email: "
    inputText "email" view
    label "password" view "Password: "
    inputText "password" view
    inputSubmit "Add"

tagAddForm :: Monad m => UTCTime -> UTCTime -> Form Html m Tag
tagAddForm created modified = Tag
    <$> "title" .: text Nothing
    <*> "created" .: stringRead "Couldn't parse as UTCTime" (Just created)
    <*> "modified" .: stringRead "Couldn't parse as UTCTime" (Just modified)

tagAddView :: View Html -> Text -> Html
tagAddView view path = form view path $ do
    label "title" view "Title: "
    inputText "title" view
    inputSubmit "Add"

showTime :: FormatTime t => t -> String
showTime = formatTime defaultTimeLocale "%D, %R"
