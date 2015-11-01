{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View where

import Web.Markury.Model

import Prelude hiding ( head, div )
import Text.Blaze.XHtml5 hiding ( Tag, text )
import Database.Persist
import Control.Monad
import Data.Time
import Text.Digestive
import Text.Digestive.Bootstrap

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

tagForm :: Monad m => UTCTime -> UTCTime -> Form Html m Tag
tagForm created modified = Tag
    <$> "title" .: text Nothing
    <*> "created" .: stringRead "Couldn't parse as UTCTime" (Just created)
    <*> "modified" .: stringRead "Couldn't parse as UTCTime" (Just modified)

tagFormSpec :: FormMeta
tagFormSpec = FormMeta
    { fm_method = POST
    , fm_target = "/tags/add"
    , fm_elements = [ FormElement "title" (Just "Title") InputText ]
    , fm_submitText = "Add"
    }
