{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View where

import Web.Markury.Model

import Prelude hiding ( head, div )
import Text.Blaze.XHtml5 hiding ( Tag, text, label, form )
import Database.Persist
import Control.Monad
import Data.Time
import Data.Text hiding ( head )
import Text.Digestive
import Text.Digestive.Blaze.Html5

bookmarksView :: [Entity Bookmark] -> Html
bookmarksView bookmarks = docTypeHtml $ do
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
                    li "Actions"
                    li "New Bookmark"
                    li "List Users"
                    li "New User"
                    li "List Tags"
                    li "New Tag"
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
                            td $ toHtml $ bookmarkTitle $ entityVal bookmark
                            td $ toHtml $ show $ bookmarkCreated $ entityVal bookmark
                            td $ toHtml $ show $ bookmarkModified $ entityVal bookmark
                div $ do
                    ul $ do
                        li "< previous"
                        li "next >"
                    p "1 of 1"
        footer $ ""

usersView :: [Entity User] -> Html
usersView users = do
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
                    li "Actions"
                    li "New User"
                    li "List Bookmarks"
                    li "New Bookmark"
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
                            td $ toHtml $ userEmail $ entityVal user
                            td $ toHtml $ userPassword $ entityVal user
                            td $ toHtml $ show $ userCreated $ entityVal user
                            td $ toHtml $ show $ userModified $ entityVal user
                div $ do
                    ul $ do
                        li "< previous"
                        li "next >"
                    p "1 of 1"
        footer $ ""

tagsView :: [Entity Tag] -> Html
tagsView tags = docTypeHtml $ do
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
                    li "Actions"
                    li "New Tag"
                    li "List Bookmarks"
                    li "New Bookmark"
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
                            td $ toHtml $ tagTitle $ entityVal tag
                            td $ toHtml $ show $ tagCreated $ entityVal tag
                            td $ toHtml $ show $ tagModified $ entityVal tag
                div $ do
                    ul $ do
                        li "< previous"
                        li "next >"
                    p "1 of 1"
        footer $ ""

bookmarkForm :: Monad m => UTCTime -> UTCTime -> Form Html m Bookmark
bookmarkForm created modified = Bookmark
    <$> "title" .: text Nothing
    <*> "description" .: optionalText Nothing
    <*> "url" .: text Nothing
    <*> "created" .: stringRead "Couldn't parse as UTCTime" (Just created)
    <*> "modified" .: stringRead "Couldn't parse as UTCTime" (Just modified)

bookmarkView :: View Html -> Text -> Html
bookmarkView view path = form view path $ do
    label "title" view "Title: "
    inputText "title" view
    label "description" view "Description: "
    inputText "description" view
    label "url" view "URL: "
    inputText "url" view
    inputSubmit "Add"

userForm :: Monad m => UTCTime -> UTCTime -> Form Html m User
userForm created modified = User
    <$> "email" .: text Nothing
    <*> "password" .: text Nothing
    <*> "created" .: stringRead "Couldn't parse as UTCTime" (Just created)
    <*> "modified" .: stringRead "Couldn't parse as UTCTime" (Just modified)

userView :: View Html -> Text -> Html
userView view path = form view path $ do
    label "email" view "Email: "
    inputText "email" view
    label "password" view "Password: "
    inputText "password" view
    inputSubmit "Add"

tagForm :: Monad m => UTCTime -> UTCTime -> Form Html m Tag
tagForm created modified = Tag
    <$> "title" .: text Nothing
    <*> "created" .: stringRead "Couldn't parse as UTCTime" (Just created)
    <*> "modified" .: stringRead "Couldn't parse as UTCTime" (Just modified)

tagView :: View Html -> Text -> Html
tagView view path = form view path $ do
    label "title" view "Title: "
    inputText "title" view
    inputSubmit "Add"
