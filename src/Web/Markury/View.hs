{-# LANGUAGE OverloadedStrings #-}
module Web.Markury.View where

import Web.Markury.Model.DB
import Web.Markury.Model.Input

import Prelude hiding ( head, div )
import Text.Blaze.XHtml5 hiding ( Tag, text, label, form )
import Text.Blaze.XHtml5.Attributes ( href )
import Control.Monad
import Data.Time ( UTCTime )
import Data.Time.Format ( FormatTime, formatTime, defaultTimeLocale )
import Data.Text hiding ( head )
import Text.Digestive
import Text.Digestive.Blaze.Html5

siteView :: Html -> Html
siteView content = docTypeHtml $ do
    headerView
    body $ do
        topNavigationView
        section $ do
            sideNavigationView
            content

bookmarkListView :: [Bookmark] -> Html
bookmarkListView bookmarks =
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

userListView :: [User] -> Html
userListView users =
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

tagListView :: [Tag] -> Html
tagListView tags =
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

bookmarkView :: Show i => i -> Bookmark -> [Tag] -> Html
bookmarkView id bookmark tags =
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
        div $ do
            h4 "Related Tags"
            table $ do
                thead $ do
                    th "Title"
                    th "Created"
                    th "Modified"
                tbody $ forM_ tags $ \tag -> do
                    tr $ do
                        td $ toHtml $ tagTitle tag
                        td $ toHtml $ showTime $ tagCreated tag
                        td $ toHtml $ showTime $ tagModified tag

userView :: Show i => i -> User -> Html
userView id user =
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

tagView :: Show i => i -> Tag -> [Bookmark] -> Html
tagView id tag bookmarks =
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
        div $ do
            h4 "Related Bookmarks"
            table $ do
                thead $ do
                    th "Title"
                    th "Created"
                    th "Modified"
                tbody $ forM_ bookmarks $ \bookmark -> do
                    tr $ do
                        td $ toHtml $ bookmarkTitle bookmark
                        td $ toHtml $ showTime $ bookmarkCreated bookmark
                        td $ toHtml $ showTime $ bookmarkModified bookmark

headerView :: Html
headerView =
    head $ do
        title "Markury - Simple Bookmarker"

topNavigationView :: Html
topNavigationView =
    nav $ do
        ul $ do
            li $ h1 "Markury"
        section $ do
            ul $ do
                li $ a ! href "https://github.com/y-taka-23/markury" $ "GitHub"

sideNavigationView :: Html
sideNavigationView =
    nav $ do
        ul $ do
            li $ a ! href "/bookmarks" $ "Bookmarks"
            li $ a ! href "/users" $ "Users"
            li $ a ! href "/tags" $ "Tags"

bookmarkAddForm :: Monad m => Form Html m BookmarkInput
bookmarkAddForm = BookmarkInput
    <$> "title" .: text Nothing
    <*> "description" .: text Nothing
    <*> "url" .: text Nothing
    <*> "tags" .: text Nothing

bookmarkAddView :: View Html -> Text -> Html
bookmarkAddView view path =
    div $ do
        form view path $ do
            label "title" view "Title: "
            inputText "title" view
            label "description" view "Description: "
            inputText "description" view
            label "url" view "URL: "
            inputText "url" view
            label "tags" view "Tags: "
            inputText "tags" view
            inputSubmit "Add"

userAddForm :: Monad m => Form Html m UserInput
userAddForm = UserInput
    <$> "email" .: text Nothing
    <*> "password" .: text Nothing

userAddView :: View Html -> Text -> Html
userAddView view path =
    div $ do
        form view path $ do
            label "email" view "Email: "
            inputText "email" view
            label "password" view "Password: "
            inputText "password" view
            inputSubmit "Add"

tagAddForm :: Monad m => Form Html m TagInput
tagAddForm = TagInput
    <$> "title" .: text Nothing

tagAddView :: View Html -> Text -> Html
tagAddView view path =
    div $ do
        form view path $ do
            label "title" view "Title: "
            inputText "title" view
            inputSubmit "Add"

showTime :: FormatTime t => t -> String
showTime = formatTime defaultTimeLocale "%D, %R"
